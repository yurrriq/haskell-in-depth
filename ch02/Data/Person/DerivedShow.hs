{-# LANGUAGE StandaloneDeriving #-}

module Data.Person.DerivedShow where

import Data.Person

deriving instance Show Person

deriving instance Read Person

deriving instance Eq Person
