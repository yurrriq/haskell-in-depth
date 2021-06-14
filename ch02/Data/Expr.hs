{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Expr where

import TextShow

data Expr a
  = Lit a
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)
  deriving (Eq, Show)

instance TextShow a => TextShow (Expr a) where
  showbPrec prec = \case
    Lit expr -> showb expr
    Add lhs rhs -> showbHelper prec 5 "+" lhs rhs
    Mult lhs rhs -> showbHelper prec 6 "*" lhs rhs
    where
      showbHelper outerPrec thisPrec op lhs rhs =
        showbParen (outerPrec > thisPrec) $
          showbPrec thisPrec lhs <> op <> showbPrec thisPrec rhs

myEval :: Num a => Expr a -> a
myEval (Lit expr) = expr
myEval (Add lhs rhs) = myEval lhs + myEval rhs
myEval (Mult lhs rhs) = myEval lhs * myEval rhs

expr1, expr2 :: Expr Int
expr1 = Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)
expr2 =
  Add
    ( Add
        (Lit 1)
        ( Mult
            (Add (Lit 1) (Lit 2))
            ( Add
                (Lit 2)
                (Mult (Lit 2) (Add (Lit 1) (Lit 2)))
            )
        )
    )
    (Add (Lit 1) (Mult (Lit 3) (Lit 2)))
