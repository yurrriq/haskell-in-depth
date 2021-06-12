module HID.CyclicEnum where

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  -- | The successor of a value.
  csucc :: a -> a
  csucc x
    | x == maxBound = minBound
    | otherwise = succ x

  -- | The predecessor of a value.
  cpred :: a -> a
  cpred x
    | x == minBound = maxBound
    | otherwise = pred x
