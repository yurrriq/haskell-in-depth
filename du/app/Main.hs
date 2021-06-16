module Main where

import App
import Data.Text.IO as TIO
import DirTree
import DiskUsage
import FileCounter
import Options.Applicative as Opt
import TextShow

main :: IO ()
main = execParser opts >>= work
  where
    opts =
      info
        (mkConfig <**> helper)
        (fullDesc <> progDesc "Directory usage info")

mkConfig :: Opt.Parser AppConfig
mkConfig =
  AppConfig
    <$> strArgument
      ( metavar "DIRECTORY"
          <> value "."
          <> showDefault
      )
    <*> option
      auto
      ( metavar "DEPTH"
          <> long "depth"
          <> short 'd'
          <> value maxBound
          <> help "Display an entry for all directories DEPTH directories deep"
      )
    <*> optional
      ( strOption
          ( metavar "EXT"
              <> long "extension"
              <> short 'e'
              <> help "Filter files by extension"
          )
      )
    <*> switch
      ( short 'L'
          <> help "Follow symlinkds (OFF by default)"
      )

work :: AppConfig -> IO ()
work config =
  do
    dirs <- snd <$> runMyApp dirTree config ()
    counters <- snd <$> runMyApp fileCount config ()
    usages <- snd <$> runMyApp diskUsage config (0 :: FileOffset)
    let report =
          toText $
            buildEntries "Directory tree:" treeEntryBuilder dirs
              <> buildEntries "File counter:" tabEntryBuilder counters
              <> buildEntries "File space usage:" tabEntryBuilder usages
    TIO.putStr report

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries =
  unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2 * n) ' '
