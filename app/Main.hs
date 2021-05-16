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
  Library.Options <$> argument str (metavar "FILEPATH")
