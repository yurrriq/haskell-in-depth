{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Person.TextShow where

import Data.Person
import TextShow

instance TextShow Person where
  showb (Person name Nothing) =
    fromString name
  showb (Person name (Just age)) =
    fromString name <> " (" <> showb age <> ")"
