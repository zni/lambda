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

-- Lambda calculus expression
data LambdaExpr a where
    Var :: String -> LambdaExpr String
    -- ^ Atom
    L   :: LambdaExpr String  -> LambdaExpr b -> LambdaExpr b
    -- ^ Lambda function
    App :: LambdaExpr b -> LambdaExpr b -> LambdaExpr b
    -- ^ Function application

instance Show (LambdaExpr a) where
    show (Var v)          = v
    show (L bound expr)   = "(λ"++show bound++". "++show expr++")"
    show (App left right) = "("++show left++" "++show right++")"

{-
data DeBruijn a where
    Index :: Integer -> DeBruijn Integer
    Lf    :: DeBruijn b -> DeBruijn b
    AppD  :: DeBruijn b -> DeBruijn b -> DeBruijn b
-}

data DeBruijn =
    Index Integer          |
    Lf DeBruijn            |
    AppD DeBruijn DeBruijn

instance Show (DeBruijn) where
    show (Index i)         = show i
    show (Lf exp)          = "(λ "++show exp++")"
    show (AppD left right) = "("++show left++" "++show right++")"


toDeBruijn :: LambdaExpr a -> DeBruijn
toDeBruijn exp = toDeBruijn' exp []

toDeBruijn' (Var s) env   = Index $ getScope s env
toDeBruijn' (L (Var s) exp) env = Lf $ toDeBruijn' exp (s:env)
toDeBruijn' (App left right) env = AppD (toDeBruijn' left env) (toDeBruijn' right env)

getScope s ls = toInteger $ maybe 0 succ $ elemIndex s ls

sComb = L (Var "x") (L (Var "y") (L (Var "z") (App (Var "x") (App (Var "z") (App (Var "y") (Var "z"))))))
kComb = L (Var "x") (L (Var "y") (Var "x"))
iComb = L (Var "x") (Var "x")
