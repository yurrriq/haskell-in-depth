{-# LANGUAGE DeriveAnyClass #-}

module Data.Radar.Types where

import HID.CyclicEnum

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Ord, Enum, Bounded, CyclicEnum)

instance Show Direction where
  show North = "N"
  show East = "E"
  show South = "S"
  show West = "W"

data Turn
  = TNone
  | TLeft
  | TRight
  | TAround
  deriving (Eq, Ord, Enum, Bounded)

instance Show Turn where
  show TNone = "--"
  show TLeft = "<-"
  show TRight = "->"
  show TAround = "||"

instance Semigroup Turn where
  TNone <> tern = tern
  TLeft <> TLeft = TAround
  TLeft <> TRight = TNone
  TLeft <> TAround = TRight
  TRight <> TRight = TAround
  TRight <> TAround = TLeft
  TAround <> TAround = TNone
  lhs <> rhs = rhs <> lhs

instance Monoid Turn where
  mempty = TNone
