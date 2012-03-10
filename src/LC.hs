{-# LANGUAGE GADTs #-}
-- Simple lambda calculus reducer (AKA partial evaluator) I wrote while stuck
-- on the turnpike.
--
-- date   : 03/09/2012
-- author : Matt Godshall
--
module LC where

--
-- Data types and type class instances.
--

-- Thar she blows... lambda calculus expressions.
data LExpr where
    Var :: String -> LExpr          -- ^ Atom
    L   :: LExpr  -> LExpr -> LExpr -- ^ Lambda function
    App :: LExpr  -> LExpr -> LExpr -- ^ Function application

instance Show LExpr where
    show (Var v)          = v
    show (L bound expr)   = "(λ"++show bound++". "++show expr++")"
    show (App left right) = "("++show left++" "++show right++")"


--
-- Enter the reducer. The reducinator.
-- One expression enters... uh, one expression leaves.
--

reduce           :: LExpr -> LExpr
reduce v@(Var s) = v

reduce l@(L bound expr) =
    case bound of
        Var _ -> L bound (reduce expr)
        _     -> undefined

reduce a@(App left right) =
    let right' = reduce right
    in replaceBound left right'
    where
          replaceBound :: LExpr -> LExpr -> LExpr
          replaceBound (L var body) val =
              let (Var s) = var
                  body'   = mapVal s body val
              in body'
              where mapVal :: String -> LExpr -> LExpr -> LExpr
                    mapVal name p@(Var n) val      = if name == n then val else p
                    mapVal name (L bound@(Var b) expr) val = 
                      if name == b
                          then mapVal name expr val
                          else L bound (mapVal name expr val)
                    mapVal v (App l r) val = (App (mapVal v l val)
                                                  (mapVal v r val))

          replaceBound l@(Var _) r@(Var _) = App l r
          replaceBound a@(App lexp rexp) val = replaceBound (reduce a) val

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

simpleTests :: [LExpr]
simpleTests = [ ident
              , appIdent
              , complexFunc
              , appComplex
              , completeAppComplex
              ]

-- Lambda reductions! Get them while they're hot!
runTests :: [LExpr]
runTests = map reduce simpleTests
