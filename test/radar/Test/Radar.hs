{-# LANGUAGE OverloadedStrings #-}

import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Radar
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main =
  defaultMain $
    testGroup
      "radar tests"
      [ testCase
          "orient is surjective"
          test_orient_surjective,
        testProperty
          "orient and rotate agree"
          prop_orient_rotate_agree
      ]

test_orient_surjective :: Assertion
test_orient_surjective = test_binop_surjective orient

test_binop_surjective :: (Eq a, Ord a, Enum a, Bounded a, Show a, Eq b, Ord b, Enum b, Bounded b, Show b) => (a -> a -> b) -> Assertion
test_binop_surjective f =
  sort (nub [f x y | x <- enumFrom minBound, y <- enumFrom minBound])
    @?= enumFrom minBound

prop_orient_rotate_agree :: Property
prop_orient_rotate_agree =
  property $
    do
      ds_@(d :| _) <- forAll genDirectionList
      let ds = NE.toList ds_
      classify "singleton" $ length ds == 1
      classify "small" $ length ds < 100
      classify "large" $ length ds >= 100
      rotateManySteps d (orientMany ds) === ds

genDirectionList :: Gen (NonEmpty Direction)
genDirectionList =
  Gen.nonEmpty (Range.linear 0 1000) Gen.enumBounded
