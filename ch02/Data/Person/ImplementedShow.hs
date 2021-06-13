module Data.Person.ImplementedShow where

import Data.Person

instance Show Person where
  show (Person name Nothing) = name
  show (Person name (Just age)) = name <> " (" <> show age <> ")"
