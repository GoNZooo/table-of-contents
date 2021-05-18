module Main where

import Library (Command (..), WatchCommand (..))
import qualified Library
import Options.Applicative
import Prelude

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = do
  let parserOptions =
        info
          (parseOptions <**> helper)
          ( fullDesc <> progDesc programDescription
              <> header ("table-of-contents - " <> programDescription)
          )
      programDescription = "Generate a table of contents for a markdown file"
  options <- execParser parserOptions

  Library.runMain options

parseOptions :: Parser Library.Options
parseOptions =
  let printCommand =
        command "print" (info parsePrintCommand $ progDesc "Print table of contents for a Markdown file")
      injectCommand =
        command "inject" (info parseInjectCommand $ progDesc "Inject table of contents into a Markdown file")
      watchCommand =
        command
          "watch"
          ( info parseWatchCommand $
              progDesc "Watch a path for changes and execute a command on each file change"
          )
   in Library.Options <$> subparser (watchCommand <> printCommand <> injectCommand)

parseWatchCommand :: Parser Command
parseWatchCommand =
  let printCommand =
        command "print" (info (pure WatchPrint) $ progDesc "Watch a directory and print ToC on updates")
      injectCommand =
        command "inject" (info (pure WatchInject) $ progDesc "Watch a directory and inject ToC on updates")
   in Watch <$> subparser (printCommand <> injectCommand) <*> argument str (metavar "FILEPATH")

parsePrintCommand :: Parser Command
parsePrintCommand = PrintToC <$> argument str (metavar "FILEPATH")

parseInjectCommand :: Parser Command
parseInjectCommand = InjectToC <$> argument str (metavar "FILEPATH")
