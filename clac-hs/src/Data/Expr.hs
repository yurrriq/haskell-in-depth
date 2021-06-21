module Data.Expr where

data Expr a
  = Lit a
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)
  deriving (Read, Show)
