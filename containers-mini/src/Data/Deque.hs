{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data.Deque
  ( Deque,
    empty,
    isEmpty,
    front,
    back,
    pushBack,
    pushFront,
    popBack,
    popFront,
  )
where

import Data.Bool (Bool)
import Data.Maybe (Maybe (..))
import Data.Sequence hiding (empty)
import qualified Data.Sequence as Seq

newtype Deque a = Deque (Seq a)

empty :: Deque a
empty = Deque Seq.empty

isEmpty :: Deque a -> Bool
isEmpty (Deque seq) = null seq

front :: Deque a -> Maybe a
front (Deque (a :<| _)) = Just a
front _ = Nothing

back :: Deque a -> Maybe a
back (Deque (_ :|> x)) = Just x
back _ = Nothing

pushFront :: a -> Deque a -> Deque a
pushFront x (Deque seq) = Deque (x :<| seq)

pushBack :: a -> Deque a -> Deque a
pushBack x (Deque seq) = Deque (seq :|> x)

popFront :: Deque a -> Deque a
popFront (Deque (_ :<| seq)) = Deque seq

popBack :: Deque a -> Deque a
popBack (Deque (seq :|> _)) = Deque seq
