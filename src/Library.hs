module Library where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Function ((&))
import qualified Data.List as List
import Prelude

newtype Options = Options {source :: FilePath}

data Heading = Heading
  { name :: String,
    depth :: Int,
    subHeadings :: [Heading]
  }
  deriving (Eq, Show)

runMain :: Options -> IO ()
runMain Options {source} = do
  linesWithHeading <- (lines >>> filter (List.isPrefixOf "#")) <$> readFile source
  let headings = makeHeadings linesWithHeading
  printTableOfContents headings

makeHeadings :: [String] -> [Heading]
makeHeadings [] = []
makeHeadings (l : ls) =
  let name = List.dropWhile (`elem` ['#', ' ']) l
      depth = depthOfHeading l
      subHeadings =
        ls
          & takeWhile (\l' -> depthOfHeading l' > depth)
          & makeHeadings
   in [Heading {name, depth, subHeadings}] <> makeHeadings (drop (length subHeadings) ls)

depthOfHeading :: String -> Int
depthOfHeading = takeWhile (== '#') >>> length

printTableOfContents :: [Heading] -> IO ()
printTableOfContents headings =
  forM_ headings $ \Heading {name, depth, subHeadings} -> do
    let indentation = List.repeat ' ' & take indentationDepth
        indentationDepth = pred depth * 2
    putStrLn $ mconcat [indentation, "- ", name]
    printTableOfContents subHeadings
