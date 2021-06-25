{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Suntimes.App where

import Control.Monad.Catch
import Control.Monad.Reader
import Suntimes.Types

newtype MyApp a = MyApp
  { runApp :: ReaderT WebAPIAuth IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader WebAPIAuth,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runMyApp :: MyApp a -> WebAPIAuth -> IO a
runMyApp app = runReaderT (runApp app)
