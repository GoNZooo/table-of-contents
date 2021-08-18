module Library where

import Control.Category ((>>>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, forever, join, unless)
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import qualified System.FSNotify as Notify
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
  | Watch WatchCommand FilePath

data WatchCommand
  = WatchInject
  | WatchPrint

runMain :: Options -> IO ()
runMain Options {command = PrintToC source} = printTableOfContents source
runMain Options {command = InjectToC source} = injectTableOfContents source
runMain Options {command = Watch WatchInject path} = do
  writeReference <- newTVarIO False
  watchInject writeReference path
runMain Options {command = Watch WatchPrint path} = watchPrint path

watch :: (Notify.Event -> Bool) -> (Notify.Event -> IO ()) -> FilePath -> IO ()
watch eventPredicate action path = do
  Notify.withManager $ \manager -> do
    _ <- Notify.watchTree manager path eventPredicate action
    forever $ threadDelay 1000000

-- | Watch a directory for injecting tables of contents into Markdown files on changes. Uses a
-- `TVar` for synchronization across threads so that we don't react to our own writes from new event
-- handler threads.
watchInject :: TVar Bool -> FilePath -> IO ()
watchInject writeReference watchPath = do
  let watchInjectEvent (Notify.Modified path _time _) = do
        justWroteFile <- readTVarIO writeReference
        unless justWroteFile $ do
          _threadId <- forkIO $ do
            atomically $ writeTVar writeReference True
            threadDelay vsCodeDelay
            injectTableOfContents path
            atomically $ writeTVar writeReference False
          pure ()
      watchInjectEvent _ = mempty
      eventPredicate (Notify.Modified filePath _time _) = ".md" `List.isSuffixOf` map Char.toLower filePath
      eventPredicate _ = False
   in watch eventPredicate watchInjectEvent watchPath

watchPrint :: FilePath -> IO ()
watchPrint watchPath =
  let watchPrintEvent (Notify.Modified path _time _) =
        printTableOfContents path
      watchPrintEvent _ = mempty
      eventPredicate (Notify.Modified filePath _time _) =
        ".md" `List.isSuffixOf` map Char.toLower filePath
      eventPredicate _ = False
   in watch eventPredicate watchPrintEvent watchPath

printTableOfContents :: FilePath -> IO ()
printTableOfContents source = do
  linesWithHeading <- (lines >>> filter (List.isPrefixOf "#")) <$> readFile source
  let headingLines = linesWithHeading & makeHeadings & headingsToLines
  forM_ headingLines putStrLn

injectTableOfContents :: FilePath -> IO ()
injectTableOfContents source = do
  fileContents <- readFile source
  let fileLines = lines fileContents
      linesWithHeading = filter (List.isPrefixOf "#") fileLines
      headingLines = linesWithHeading & makeHeadings & headingsToLines
      (prelude, withoutPrelude) = List.splitAt 2 fileLines
      withoutToC = dropToCLines withoutPrelude
      dropToCLines ls
        | hasTableOfContents ls = dropWhile (trimLeft >>> List.isPrefixOf "- ") ls
        | otherwise = insertPadding ls
      trimLeft = List.dropWhile (== ' ')
      insertPadding = ([""] <>) >>> id
      newFileContents = unlines $ join [prelude, headingLines, withoutToC]
  writeFile source newFileContents

hasTableOfContents :: [String] -> Bool
hasTableOfContents = listToMaybe >>> maybe False ("- " `List.isPrefixOf`)

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

-- I don't know why, but VSCode won't co-operate without this pause. `vim` works fine without it.
vsCodeDelay :: Int
vsCodeDelay = 100000
