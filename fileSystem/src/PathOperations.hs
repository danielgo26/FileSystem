module PathOperations
  ( separateIntoOriginPathAndFilename,
    validatePathsEndsWith,
    isAbsolutePath,
    getAbsolutePathFrom,
    resolveAbsPath,
    removeParentDirRefsFromAbsPath,
    removeCurrDirRefsFromAbsPath,
    validateAbsPathIsWithinLoggedUserHomeDir,
    ensureResolvedPathIsWithinUserHomeDir,
  )
where

import Data.List (intercalate)
import DataConfig
  ( backReference,
    pathsSeparatorStr,
    pathsSeparatorSymbol,
  )
import SystemErrors
  ( FSError (..),
    SystemError (..),
  )
import Utils
  ( splitByChar,
    startsWith,
  )

separateIntoOriginPathAndFilename :: String -> (String, String)
separateIntoOriginPathAndFilename pathToFile =
  (cleanPath, concat fileNameArr)
  where
    splittedPath = splitByChar pathsSeparatorSymbol pathToFile
    splittedPathLen = length splittedPath
    (cleanPathArr, fileNameArr) = splitAt (splittedPathLen - 1) splittedPath
    path = intercalate pathsSeparatorStr cleanPathArr
    cleanPath
      | path == "" = pathsSeparatorStr
      | otherwise = path

validatePathsEndsWith :: Char -> String -> String
validatePathsEndsWith symbol path
  | lastStr == [symbol] = path
  | otherwise = path ++ [symbol]
  where
    reversed = reverse path
    lastStr = case reversed of
      "" -> ""
      (x : _) -> [x]

isAbsolutePath :: String -> Bool
isAbsolutePath "" = False
isAbsolutePath (x : _) =
  x == pathsSeparatorSymbol

getAbsolutePathFrom :: String -> String -> String
getAbsolutePathFrom path currDir
  | isAbsolutePath path = path
  | otherwise = currDir ++ path

-- By resolve -> transform path without '..' and '.'
resolveAbsPath :: String -> Either SystemError String
resolveAbsPath path =
  removeParentDirRefsFromAbsPath $ removeCurrDirRefsFromAbsPath path

removeParentDirRefsFromAbsPath :: String -> Either SystemError String
removeParentDirRefsFromAbsPath "" = Left $ FileSystemError $ InvalidPath "Path cannot be empty"
removeParentDirRefsFromAbsPath path = do
  let splittedPath = splitByChar pathsSeparatorSymbol path
  removedParentDirRefsArray <- removeParentDirRefsFromArray [] (pathsSeparatorStr : drop 1 splittedPath)
  let mergedPathWithFileSeparator = intercalate pathsSeparatorStr $ drop 1 removedParentDirRefsArray

  if take 1 removedParentDirRefsArray /= [pathsSeparatorStr]
    then
      Left $ FileSystemError $ InvalidPath "Invalid path given! Path must start with root!"
    else
      Right (pathsSeparatorStr ++ mergedPathWithFileSeparator)

removeParentDirRefsFromArray :: [String] -> [String] -> Either SystemError [String]
removeParentDirRefsFromArray removed [] = Right removed
removeParentDirRefsFromArray removed [x]
  | x == backReference = Left $ FileSystemError $ InvalidPath "Invalid path given!"
  | otherwise = Right (removed ++ [x])
removeParentDirRefsFromArray removed (f : s : rest)
  | f == backReference = Left $ FileSystemError $ InvalidPath "Invalid path given!"
  | s == backReference && f == pathsSeparatorStr = Left $ FileSystemError $ InvalidPath "Invalid path given! Cannot locate before the root!"
  | s == backReference && f /= pathsSeparatorStr = removeParentDirRefsFromArray [] (removed ++ rest)
  | otherwise = removeParentDirRefsFromArray (removed ++ [f]) (s : rest)

removeCurrDirRefsFromAbsPath :: String -> String
removeCurrDirRefsFromAbsPath "" = ""
removeCurrDirRefsFromAbsPath path =
  pathsSeparatorStr ++ intercalate pathsSeparatorStr (removeCurrDirRefsFromArray splittedPath)
  where
    splittedPath = splitByChar pathsSeparatorSymbol (drop 1 path)

removeCurrDirRefsFromArray :: [String] -> [String]
removeCurrDirRefsFromArray [] = []
removeCurrDirRefsFromArray (x : xs)
  | x == "." = removeCurrDirRefsFromArray xs
  | otherwise = x : removeCurrDirRefsFromArray xs

validateAbsPathIsWithinLoggedUserHomeDir :: String -> String -> Either SystemError String
validateAbsPathIsWithinLoggedUserHomeDir path homeDir = do
  let unifiedPath = path ++ (if length path == length homeDir - 1 then pathsSeparatorStr else "")

  if startsWith homeDir unifiedPath
    then
      Right path
    else
      Left $ FileSystemError $ IllegalOperation "The given path does not belong to that user's home dir!"

ensureResolvedPathIsWithinUserHomeDir :: String -> Maybe String -> Either SystemError String
ensureResolvedPathIsWithinUserHomeDir resolvedPath loggedUserHomeDir =
  case loggedUserHomeDir of
    Nothing ->
      Left $ FileSystemError $ MissingHomeDirectory "The given user does not have a home directory in the file tree!"
    Just loggedUserResolvedHomeDir -> do
      validatedPath <- validateAbsPathIsWithinLoggedUserHomeDir resolvedPath loggedUserResolvedHomeDir
      Right validatedPath