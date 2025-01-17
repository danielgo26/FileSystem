{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use null" #-}

module TreeOperations
  ( isValidRegFileAbsPath,
    isValidDirAbsPath,
    findByAbsolutePathWithin,
    addNewRegFileWithAbsPath,
    addNewDirWithAbsPath,
    removeFileWithAbsPath,
  )
where

import DataConfig
  ( pathsSeparatorStr,
    pathsSeparatorSymbol,
    rootDirName,
  )
import FileTreeStructure
  ( Content (..),
    FileTree (..),
  )
import PathOperations
  ( resolveAbsPath,
    separateIntoOriginPathAndFilename,
  )
import SystemErrors
  ( FSError (..),
    SystemError (..),
  )
import Utils (splitByChar)
import Prelude hiding (FilePath)

isValidRegFileAbsPath :: String -> FileTree -> Either SystemError Bool
isValidRegFileAbsPath path tree = do
  foundTree <- findByAbsolutePathWithin path tree

  case foundTree of
    Just (FileNode _ (TextContent _)) -> Right True
    _ -> Right False

isValidDirAbsPath :: String -> FileTree -> Either SystemError Bool
isValidDirAbsPath path tree = do
  foundTree <- findByAbsolutePathWithin path tree

  case foundTree of
    Just (FileNode _ (Files _)) -> Right True
    _ -> Right False

-- find function
-- that function searches only within the files of the current root, it will not match the root if we search
-- for it except if it is the corner case - '/'
findByAbsolutePathWithin :: String -> FileTree -> Either SystemError (Maybe FileTree)
findByAbsolutePathWithin "" _ = Right Nothing
findByAbsolutePathWithin path tree = do
  resolvedPath <- resolveAbsPath path

  let splittedPath = splitByChar pathsSeparatorSymbol resolvedPath
      (first, rest) = splitAt 1 splittedPath

  if length splittedPath > 0 && first == [""]
    then
      Right (findBySplittedAbsPath (pathsSeparatorStr : rest) tree) -- root
    else
      Right (findBySplittedAbsPath splittedPath tree)

findBySplittedAbsPath :: [String] -> FileTree -> Maybe FileTree
findBySplittedAbsPath [] _ = Nothing
findBySplittedAbsPath [x] tree = findByNameInCurrTree x tree
findBySplittedAbsPath (x : xs) tree = do
  resultedSubTree <- findByNameInCurrTree x tree
  findBySplittedAbsPath xs resultedSubTree

findByNameInCurrTree :: String -> FileTree -> Maybe FileTree
findByNameInCurrTree "" _ = Nothing
findByNameInCurrTree nameSearched t@(FileNode name fileContent)
  | nameSearched == rootDirName = if name == rootDirName then Just t else Nothing
  | otherwise = case fileContent of
      TextContent _ -> Nothing
      Files subFiles -> findByNameInSubFiles nameSearched subFiles

findByNameInSubFiles :: String -> [FileTree] -> Maybe FileTree
findByNameInSubFiles _ [] = Nothing
findByNameInSubFiles nameSearched (f@(FileNode name _) : rest) =
  case searchResult of
    Nothing -> findByNameInSubFiles nameSearched rest
    _ -> searchResult
  where
    searchResult
      | name == nameSearched = Just f
      | otherwise = Nothing

-- Add regFile function
addNewRegFileWithAbsPath :: String -> String -> FileTree -> Either SystemError FileTree
addNewRegFileWithAbsPath filePath fileContent tree = do
  addNewFile filePath (TextContent fileContent) tree

-- Add dir function like the avobe one, but for files
addNewDirWithAbsPath :: String -> FileTree -> Either SystemError FileTree
addNewDirWithAbsPath filePath =
  addNewFile filePath (Files [])

addNewFile :: String -> Content -> FileTree -> Either SystemError FileTree
addNewFile filePath content tree = do
  resolvedPath <- resolveAbsPath filePath
  let (pathToFile, fileName) = separateIntoOriginPathAndFilename resolvedPath
  searchedPathInTree <- findByAbsolutePathWithin resolvedPath tree

  case searchedPathInTree of
    Nothing -> do
      validPathToFile <- findByAbsolutePathWithin pathToFile tree
      case validPathToFile of
        Nothing ->
          Left $ FileSystemError $ InvalidPath "All the directories from the path must exists in order to create a new one!"
        Just _ ->
          Right $ addNewFileInto pathToFile fileName content "" tree
    Just _ ->
      Left $ FileSystemError $ InvalidPath ("File \"" ++ fileName ++ "\" already exists in that location!")

addNewFileInto :: String -> String -> Content -> String -> FileTree -> FileTree
addNewFileInto _ _ _ _ regFile@(FileNode _ (TextContent _)) = regFile
addNewFileInto pathToFile fileName fileContent currPath (FileNode dirName (Files files))
  | newCurrPath == pathToFile = FileNode dirName (Files (newFileNode : files))
  | otherwise = FileNode dirName (Files modifiedFiles)
  where
    newCurrPath
      | dirName == rootDirName = rootDirName
      | currPath == rootDirName = rootDirName ++ dirName
      | otherwise = currPath ++ pathsSeparatorStr ++ dirName
    newFileNode = FileNode fileName fileContent
    modifiedFiles = modifySubfiles pathToFile fileName fileContent newCurrPath files

modifySubfiles :: String -> String -> Content -> String -> [FileTree] -> [FileTree]
modifySubfiles _ _ _ _ [] = []
modifySubfiles pathToFile fileName fileContent currPath (first : rest) =
  modifiedTree : modifiedSubFiles
  where
    modifiedTree = addNewFileInto pathToFile fileName fileContent currPath first
    modifiedSubFiles = modifySubfiles pathToFile fileName fileContent currPath rest

-- remove file function
removeFileWithAbsPath :: String -> FileTree -> Either SystemError (Maybe FileTree)
removeFileWithAbsPath filePath tree = do
  resolvedPath <- resolveAbsPath filePath

  if resolvedPath == rootDirName
    then
      Left $ FileSystemError $ MissingHomeDirectory "Cannot remove the root directory!"
    else
      Right $ removeFileFrom resolvedPath "" tree

removeFileFrom :: String -> String -> FileTree -> Maybe FileTree
removeFileFrom fullPathToFile currPath regFile@(FileNode regFileName (TextContent _))
  | fullPathToFile == newCurrPath = Nothing
  | otherwise = Just regFile
  where
    newCurrPath
      | currPath == pathsSeparatorStr = pathsSeparatorStr ++ regFileName
      | otherwise = currPath ++ pathsSeparatorStr ++ regFileName
removeFileFrom fullPathToFile currPath (FileNode dirName (Files files))
  | newCurrPath == fullPathToFile = Nothing
  | otherwise = Just $ FileNode dirName (Files removedFromSubfiles)
  where
    newCurrPath
      | currPath == "" = rootDirName
      | currPath == rootDirName = rootDirName ++ dirName
      | otherwise = currPath ++ pathsSeparatorStr ++ dirName
    removedFromSubfiles = removeFromSubfiles fullPathToFile newCurrPath files

removeFromSubfiles :: String -> String -> [FileTree] -> [FileTree]
removeFromSubfiles _ _ [] = []
removeFromSubfiles fullPathToFile currPath (first : rest) = do
  let removedFromFirstTree = removeFileFrom fullPathToFile currPath first
  let removedFromRest = removeFromSubfiles fullPathToFile currPath rest

  case removedFromFirstTree of
    Nothing -> removedFromRest
    Just removedFromFirstNode -> removedFromFirstNode : removedFromRest
