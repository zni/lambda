{-# LANGUAGE GADTs #-}

-- date   : 02/24/2014
-- author : Matt Godshall

module LambdaCalculus (LambdaExpr(..)
                      ,DeBruijn(..)
                      ,toDeBruijn
                      ,sComb
                      ,kComb
                      ,iComb) where


import Data.List (elemIndex)
import Data.Maybe (maybe)

-- | Lambda calculus expression
data LambdaExpr a where
    Var :: String -> LambdaExpr String                        -- ^ Atom
    L   :: LambdaExpr String  -> LambdaExpr b -> LambdaExpr b -- ^ Lambda function
    App :: LambdaExpr b -> LambdaExpr b -> LambdaExpr b       -- ^ Function application

instance Show (LambdaExpr a) where
    show (Var v)          = v
    show (L bound expr)   = "(λ"++show bound++". "++show expr++")"
    show (App left right) = "("++show left++" "++show right++")"


-- | Expressions for de Bruijn indices.
data DeBruijn a where
    Index :: Integer -> DeBruijn Integer            -- ^ Index
    Lf    :: DeBruijn Integer -> DeBruijn Integer   -- ^ Lambda
    AppD  :: DeBruijn b -> DeBruijn b -> DeBruijn b -- ^ Application

instance Show (DeBruijn a) where
    show (Index i)         = show i
    show (Lf exp)          = "(λ "++show exp++")"
    show (AppD left right) = "("++show left++" "++show right++")"


-- | Convert lambda expression to de Bruijn indices.
toDeBruijn     :: LambdaExpr a -> DeBruijn Integer
toDeBruijn exp = toDeBruijn' exp [] 50

toDeBruijn'                      :: LambdaExpr a -> [String] -> Integer -> DeBruijn Integer
toDeBruijn' (Var s)          env free =
    let loc = getScope s env
    in if loc == 0
       then Index free
       else Index loc
toDeBruijn' (L (Var s) exp)  env free = Lf $ toDeBruijn' exp (s:env) (succ free)
toDeBruijn' (App left right) env free = AppD (toDeBruijn' left env (succ free)) (toDeBruijn' right env (free + 2))

getScope      :: String -> [String] -> Integer
getScope s ls = toInteger $ maybe 0 succ $ elemIndex s ls

-- | SKI combinators for conversion.
sComb = L (Var "x") (L (Var "y") (L (Var "z") (App (Var "x") (App (Var "z") (App (Var "y") (Var "z"))))))
kComb = L (Var "x") (L (Var "y") (Var "x"))
iComb = L (Var "x") (Var "x")

-- | Arbitrary test terms.
l2 = (L (Var "x") (L (Var "y") (L (Var "z") (L (Var "n") (App (Var "x") (App (Var "z") (Var "n")))))))
l3 = (App  l2 (Var "d"))
