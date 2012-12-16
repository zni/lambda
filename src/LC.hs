{-# LANGUAGE GADTs #-}
-- Simple lambda calculus β-reducer.
-- Currently supports the following reduction strategies:
--   - call-by-value
--
-- date   : 03/09/2012
-- author : Matt Godshall
--
module LC where

import qualified Data.Set as S

--
-- Data types and type class instances.
--

-- Lambda calculus expression
data LExpr where
    Var :: String -> LExpr          -- ^ Atom
    L   :: LExpr  -> LExpr -> LExpr -- ^ Lambda function
    App :: LExpr  -> LExpr -> LExpr -- ^ Function application
    deriving (Eq, Ord)

instance Show LExpr where
    show (Var v)          = v
    show (L bound expr)   = "(λ"++show bound++". "++show expr++")"
    show (App left right) = "("++show left++" "++show right++")"


--
-- Beta-reduction
--


-- Perform beta-reduction with the given reduction strategy.
betaReduce :: Strategy -> LExpr -> LExpr
betaReduce strat v@(Var _) = strat v
betaReduce strat l@(L _ _) = strat l
betaReduce strat app@(App (Var _) _) = app
betaReduce strat app@(App left right) =
  betaReduce strat (strat app) 


--
-- Reduction strategies.
--

type Strategy = (LExpr -> LExpr)

callByValue :: Strategy
callByValue v@(Var _) = v
callByValue l@(L _ _) = l
callByValue app@(App v@(Var _) _) = app
callByValue (App left right) =
  let right' = callByValue right
  in if isValue right'
       then let left'  = applyExpr callByValue right' left
            in callByValue left'
       else callByValue right'
  where isValue :: LExpr -> Bool
        isValue (Var _) = True
        isValue (L _ _) = True
        isValue (App _ _) = False

applyExpr :: Strategy -> LExpr -> LExpr -> LExpr
applyExpr _ val var@(Var _)        = App var val
applyExpr _ val (L v@(Var _) body) = rewrite v val body
applyExpr strat val app@(App _ _)  = applyExpr strat val (strat app) 

rewrite :: LExpr -> LExpr -> LExpr -> LExpr
rewrite bound val var@(Var _) =
  if bound == var
    then val
    else var
rewrite bound val (L v@(Var _) body) = 
  if bound == v
    then rewrite bound val body
    else L v (rewrite bound val body)
rewrite bound val (App left right) =
  App (rewrite bound val left) (rewrite bound val right)


--
-- Utility functions.
--

-- Test for alpha-equivalence.
isAlphaEquiv :: LExpr -> LExpr -> Bool
isAlphaEquiv (Var _) (Var _)                     = True
isAlphaEquiv (L (Var _) body) (L (Var _) body')  = isAlphaEquiv body body'
isAlphaEquiv (App left right) (App left' right') = isAlphaEquiv left left' && isAlphaEquiv right right'
isAlphaEquiv _ _                                 = False

-- XXX Test for beta-equivalence.
isBetaEquiv :: Strategy -> LExpr -> LExpr -> Bool
isBetaEquiv strat expr expr' =
  let redexExpr  = betaReduce strat expr
      redexExpr' = betaReduce strat expr'
  in isAlphaEquiv redexExpr redexExpr'


-- Get a list of free variables.
freeVars :: LExpr -> [LExpr]
freeVars expr = S.elems $ getVars' (S.member) S.empty S.empty expr

-- Get a list of bound variables.
boundVars :: LExpr -> [LExpr]
boundVars expr = 
  S.elems $ getVars' (\x -> \y -> not $ S.member x y) S.empty S.empty expr

getVars' :: (LExpr -> S.Set LExpr -> Bool) ->
            S.Set LExpr ->
            S.Set LExpr ->
            LExpr ->
            S.Set LExpr
getVars' p bound vars v@(Var _) =
  if p v bound
    then vars
    else S.insert v vars
getVars' p bound vars (L arg@(Var _) body) =
  getVars' p (S.insert arg bound) vars body
getVars' p bound vars (App left right) =
  (getVars' p bound vars left) `S.union` (getVars' p bound vars right)


--
-- Some simple expressions.
--

-- Identity function.
ident :: LExpr
ident = L (Var "x") (Var "x")

-- Identity applied.
appIdent :: LExpr
appIdent = App ident (Var "y")

-- A somewhat more involved function that doesn't reduce.
complexFunc :: LExpr
complexFunc = L (Var "x") (L (Var "z") (App (Var "x") (Var "z")))

-- Partial reduction...
appComplex :: LExpr
appComplex = App complexFunc (Var "t")

-- ...and a little more.
completeAppComplex :: LExpr
completeAppComplex = App (App complexFunc (Var "t")) $ Var "u"

-- Expression that does not terminate with the call-by-value reduction strategy.
cbvDoesNotTerminate :: LExpr
cbvDoesNotTerminate = App (L (Var "y") (Var "a")) (App (L (Var "x") (App (Var "x") (Var "x"))) (L (Var "x") (App (Var "x") (Var "x"))))

doesNotTerminate = (App (L (Var "x") (App (Var "x") (Var "x"))) (L (Var "x") (App (Var "x") (Var "x"))))

simpleTests :: [LExpr]
simpleTests = [ ident
              , appIdent
              , complexFunc
              , appComplex
              , completeAppComplex
              ]

runTests :: [LExpr]
runTests = map (betaReduce callByValue) simpleTests
