module IOStringManipulation
  ( getSeparatedStringInput,
    printFormatted,
    readUntilDot,
    getHelpMenu,
    printFileContentInVimMode,
  )
where

import FileTreeStructure
  ( Content (..),
    FileTree (..),
  )
import Utils (splitByChar)

printFormatted :: FileTree -> IO ()
printFormatted tree
  | innerStr == "" = putStrLn "<empty>"
  | otherwise = putStr formattedString
  where
    formattedString =
      unlines
        [ "\n*******************************Files*******************************\n",
          innerStr,
          "*******************************************************************\n\n"
        ]
    innerStr = getString 0 tree

getString :: Int -> FileTree -> String
getString padding (FileNode name (TextContent _)) =
  getPadding padding ++ toPrint
  where
    toPrint = "File: " ++ name ++ "\n"
getString padding (FileNode name (Files [])) =
  getPadding padding ++ toPrint
  where
    toPrint = "Dir: " ++ name ++ "\n"
getString padding (FileNode name (Files (first : rest))) =
  getPadding padding ++ toPrint ++ getString (padding + 1) first ++ getForRest (padding + 1) rest
  where
    toPrint = "Dir: " ++ name ++ "\n"

getForRest :: Int -> [FileTree] -> String
getForRest _ [] = ""
getForRest padding (first : rest) = getString padding first ++ getForRest padding rest

getPadding :: (Eq t, Num t) => t -> [Char]
getPadding 0 = ""
getPadding padding = "--------" ++ getPadding (padding - 1)

getSeparatedStringInput :: String -> [String]
getSeparatedStringInput "" = []
getSeparatedStringInput input = splitByChar ' ' input

readUntilDot :: IO String
readUntilDot = readUntilDotFormatted True

readUntilDotFormatted :: Bool -> IO [Char]
readUntilDotFormatted isFirstLine = do
  line <- getLine
  if line == "."
    then
      return ""
    else do
      rest <- readUntilDotFormatted False
      if isFirstLine
        then
          return (line ++ rest)
        else
          return ("\n" ++ line ++ rest)

getHelpMenu :: [String] -> IO ()
getHelpMenu options = do
  putStrLn "**********Options**********"
  printHelpMenuAligned options 1

printHelpMenuAligned :: [String] -> Int -> IO ()
printHelpMenuAligned [] _ = putStrLn "***************************"
printHelpMenuAligned (x : xs) index = do
  putStrLn (show index ++ "." ++ x)
  printHelpMenuAligned xs (index + 1)

printFileContentInVimMode :: String -> IO ()
printFileContentInVimMode content = do
  putStrLn "****************Content****************"
  putStrLn content
  putStrLn "**************content end**************"