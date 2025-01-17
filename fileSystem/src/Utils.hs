module Utils
  ( root,
    splitByChar,
    indexOf,
    empty,
    startsWith,
    getUserCredentials,
    resolveMaybeWithErr,
    isBlank,
    isAnyBlank,
    displaySuccess,
    displayError,
    displayInfoStatusMessage,
  )
where

import DataConfig (rootDirName)
import FileTreeStructure
  ( Content (..),
    FileTree (..),
  )
import System.Console.ANSI
  ( Color (Cyan, Green, Red),
    ColorIntensity (Vivid),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGR,
  )
import Data.Char (isSpace)
import System.IO (hFlush, stdout)
import SystemErrors (SystemError (..))

root :: FileTree
root = FileNode rootDirName (Files [])

splitByChar :: Char -> String -> [String]
splitByChar _ [] = []
splitByChar c str =
  let (word, rest) = span (/= c) str
   in word : case rest of
        [] -> []
        (_ : rest') -> splitByChar c rest'

indexOf :: (Eq t1, Num t2) => t1 -> [t1] -> Maybe t2
indexOf el array = indexOfWithCounter el array 0

indexOfWithCounter :: (Eq t1, Num t2) => t1 -> [t1] -> t2 -> Maybe t2
indexOfWithCounter _ [] _ = Nothing
indexOfWithCounter el (x : xs) index
  | x == el = Just index
  | otherwise = indexOfWithCounter el xs (index + 1)

empty :: [a] -> Bool
empty [] = True
empty _ = False

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith prefix xs = prefix == take (length prefix) xs

getUserCredentials :: IO (String, String)
getUserCredentials = do
  putStr "Enter username: "
  hFlush stdout
  currUsername <- getLine

  putStr "Enter password: "
  hFlush stdout
  currPassword <- getLine

  return (currUsername, currPassword)

resolveMaybeWithErr :: Maybe t -> SystemError -> Either SystemError t
resolveMaybeWithErr maybeValue err =
  case maybeValue of
    Nothing -> Left err
    Just value -> Right value

isBlank :: String -> Bool
isBlank str = 
  all isSpace str

isAnyBlank :: [String] -> Bool
isAnyBlank [] = False
isAnyBlank (x:xs) = isBlank x || isAnyBlank xs

displaySuccess :: String -> IO ()
displaySuccess =
  displayColourfulText Green

displayError :: String -> IO ()
displayError =
  displayColourfulText Red

displayInfoStatusMessage :: String -> IO ()
displayInfoStatusMessage =
  displayColourfulText Cyan

displayColourfulText :: Color -> String -> IO ()
displayColourfulText color msg = do
  setSGR [SetColor Foreground Vivid color]
  putStrLn msg
  setSGR [Reset]
