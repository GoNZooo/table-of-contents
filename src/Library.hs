module Library where

import Control.Category ((>>>))
import Control.Monad (forM_)
import qualified Data.Char as Char
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
      remaining = drop (countSubHeadings subHeadings) ls
   in [Heading {name, depth, subHeadings}] <> makeHeadings remaining

depthOfHeading :: String -> Int
depthOfHeading = takeWhile (== '#') >>> length

printTableOfContents :: [Heading] -> IO ()
printTableOfContents headings =
  forM_ headings $ \Heading {name, depth, subHeadings} -> do
    let indentation = List.repeat ' ' & take indentationDepth
        indentationDepth = pred depth * 2
        anchor = name & filter (`notElem` symbols) & map sanitizeCharacter & lowercase
        sanitizeCharacter ' ' = '-'
        sanitizeCharacter c = c
        lowercase = map Char.toLower
        link = mconcat ["[", name, "]", "(#", anchor, ")"]
    putStrLn $ mconcat [indentation, "- ", link]
    printTableOfContents subHeadings

symbols :: String
symbols = "[]/();=`\"'.!,?:*&<>|#^$@\\%"

countSubHeadings :: [Heading] -> Int
countSubHeadings =
  foldr (\Heading {subHeadings} accumulator -> 1 + countSubHeadings subHeadings + accumulator) 0
