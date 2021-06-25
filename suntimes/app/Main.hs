module Main where

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.Extra (eitherM)
import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative as Opt
import Suntimes.App
import Suntimes.ProcessRequest
import Suntimes.STExcept
import System.Exit
import System.IO.Error (ioeGetFileName, isDoesNotExistError)

data AppMode
  = FileInput FilePath
  | Interactive

data Params = Params AppMode FilePath

main :: IO ()
main =
  (execParser opts >>= withConfig)
    `catches` [ Handler parserExit,
                Handler printIOError,
                Handler printOtherErrors
              ]
  where
    opts =
      info
        (mkParams <**> helper)
        ( fullDesc
            <> progDesc "Reports sunrise/sunset times for the specified location"
        )
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
    printIOError :: IOException -> IO ()
    printIOError ex
      | isDoesNotExistError ex =
        do
          let mbfn = ioeGetFileName ex
          putStrLn (unwords ["File", maybe "" id mbfn, "not found"])
      | otherwise = putStrLn ("I/O error: " <> show ex)
    printOtherErrors :: SomeException -> IO ()
    printOtherErrors = print

withConfig :: Params -> IO ()
withConfig (Params appMode config) =
  do
    eitherM (const (throwM ConfigError)) (runMyApp (run appMode)) $
      eitherDecodeStrict `fmap` BS.readFile config
  where
    run :: AppMode -> MyApp ()
    run (FileInput fname) =
      liftIO (TIO.readFile fname) >>= processMany . T.lines
    run Interactive = processInteractively

mkParams :: Opt.Parser Params
mkParams = Params <$> (fileInput <|> interactive) <*> config
  where
    fileInput =
      FileInput
        <$> strOption
          ( long "file" <> short 'f'
              <> metavar "FILENAME"
              <> help "Input file"
          )
    interactive =
      flag
        Interactive
        Interactive
        ( long "interactive" <> short 'i'
            <> help "Interactive mode"
        )
    config =
      strOption
        ( long "config" <> short 'c'
            <> value "config.json"
            <> showDefault
            <> metavar "CONFIG_FILE"
            <> help "Configuration file"
        )
