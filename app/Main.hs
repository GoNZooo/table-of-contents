module Main where

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
   in Library.Options <$> subparser (printCommand <> injectCommand)

parsePrintCommand :: Parser Library.Command
parsePrintCommand = Library.PrintToC <$> argument str (metavar "FILEPATH")

parseInjectCommand :: Parser Library.Command
parseInjectCommand = Library.InjectToC <$> argument str (metavar "FILEPATH")
