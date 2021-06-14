{-# OPTIONS_GHC -Wno-type-defaults #-}

module Test.Stack where

import Data.Stack
import Test.Tasty
import Test.Tasty.HUnit

test_stack :: TestTree
test_stack =
  testGroup
    "Stack"
    [ testCase "caddr" $
        top st' @?= Just 5,
      testCase "push 100" $
        top st'' @?= Just 100,
      testCase "pop pop pop pop" $
        isEmpty (pop (pop st')) @? "Four pops is empty"
    ]
  where
    st = push 15 $ push 10 $ push 5 $ push 0 empty
    st' = pop (pop st)
    st'' = push 100 st'
