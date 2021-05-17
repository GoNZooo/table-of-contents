module Library where

import Control.Category ((>>>))
import Control.Monad (forM_, join)
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List as List
import System.IO.Strict (readFile)
import Prelude hiding (readFile)

newtype Options = Options {command :: Command}

data Heading = Heading
  { name :: String,
    depth :: Int,
    subHeadings :: [Heading]
  }
  deriving (Eq, Show)

data Command
  = PrintToC FilePath
  | InjectToC FilePath

runMain :: Options -> IO ()
runMain Options {command = PrintToC source} = do
  linesWithHeading <- (lines >>> filter (List.isPrefixOf "#")) <$> readFile source
  let headingLines = linesWithHeading & makeHeadings & headingsToLines
  forM_ headingLines putStrLn
runMain Options {command = InjectToC source} = injectTableOfContents source

injectTableOfContents :: FilePath -> IO ()
injectTableOfContents source = do
  fileContents <- readFile source
  let fileLines = lines fileContents
      linesWithHeading = filter (List.isPrefixOf "#") fileLines
      headingLines = linesWithHeading & makeHeadings & headingsToLines
      (prelude, withoutPrelude) = List.splitAt 2 fileLines
      withoutToC = dropToCLines withoutPrelude
      dropToCLines ls
        | hasTableOfContents ls = dropWhile (trimLeft >>> (List.isPrefixOf "- ")) ls
        | otherwise = insertPadding ls
      trimLeft = List.dropWhile (== ' ')
      insertPadding = ([""] <>) >>> id
      newFileContents = unlines $ join [prelude, headingLines, withoutToC]
  writeFile source newFileContents

hasTableOfContents :: [String] -> Bool
hasTableOfContents = head >>> ("- " `List.isPrefixOf`)

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

headingsToLines :: [Heading] -> [String]
headingsToLines = foldr (headingToLines >>> (<>)) []

headingToLines :: Heading -> [String]
headingToLines Heading {name, depth, subHeadings} =
  let indentation = List.repeat ' ' & take indentationDepth
      indentationDepth = pred depth * 2
      anchor = name & filter (`notElem` symbols) & map sanitizeCharacter & lowercase
      sanitizeCharacter ' ' = '-'
      sanitizeCharacter c = c
      lowercase = map Char.toLower
      link = mconcat ["[", name, "]", "(#", anchor, ")"]
   in mconcat [indentation, "- ", link] : headingsToLines subHeadings

symbols :: String
symbols = "[]/();=`\"'.!,?:*&<>|#^$@\\%"

countSubHeadings :: [Heading] -> Int
countSubHeadings =
  foldr (\Heading {subHeadings} accumulator -> 1 + countSubHeadings subHeadings + accumulator) 0
