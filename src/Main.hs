{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Applicative
import Control.Monad
import System.IO
import System.Directory
import System.FilePath
import Data.List
import System.Environment
import Text.Regex.Posix
import System.Console.CmdArgs

infixr 5 $.
($.) :: (a -> b) -> a -> b
($.) = ($)

data Option = Option
    { src :: String
    , dist :: String
    } deriving (Show, Data, Typeable)

option :: Option
option = Option
    { src = "."     &= typDir
    , dist = "/tmp" &= typDir
    }

getValidContents :: FilePath -> IO [String]
getValidContents path = filter (`notElem` [".", ".."]) <$> getDirectoryContents path

isSearchableDir :: FilePath -> IO Bool
isSearchableDir dir = (&&) <$> doesDirectoryExist dir <*> (searchable <$> getPermissions dir)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
  cnts <- map (dir </>) <$> getValidContents dir
  concat <$> forM cnts $. \path -> do
    isDirectory <- isSearchableDir path
    if isDirectory
      then getRecursiveContents path
      else return [path]

dumlRemover :: String -> String
dumlRemover s
  | (s =~ "///DUML" :: Bool)  == True = ""
  | otherwise = s

readContents :: FilePath -> IO String
readContents rf = do
    s <- readFile rf
    return s

main :: IO ()
main = do
    [src, dist] <- getArgs
    print src
    print dist
    fs <- getRecursiveContents src
    forM_ fs $ \x -> do
        inpStr <- readContents x

        let outputFile = dist ++ "/" ++ takeFileName x
        writeFile outputFile $ unlines $ map(dumlRemover) $ lines inpStr
