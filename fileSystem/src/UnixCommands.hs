{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module UnixCommands
  ( CatResult (..),
    getHomeDirectoryPath,
    changeWorkingDirectory,
    LastValidRemovalState (..),
    removeAllFrom,
    makeDirectory,
    makeRegFile,
    concatenateFiles,
    getValidPath,
  )
where

import DataConfig
  ( adminHomeDirPath,
    pathsSeparatorSymbol,
    regUserHomeDirPath,
    rootDirName,
  )
import FileTreeStructure
  ( Content (..),
    FileTree (..),
  )
import IOStringManipulation (readUntilDot)
import PathOperations
  ( ensureResolvedPathIsWithinUserHomeDir,
    getAbsolutePathFrom,
    resolveAbsPath,
    validatePathsEndsWith,
  )
import SystemErrors
  ( FSError (..),
    SystemError (..),
  )
import SystemStateConfiguration (SystemState (..))
import SystemUsers
  ( SystemUser (..),
    UserInfo (..),
    UserType (..),
  )
import TreeOperations
  ( addNewDirWithAbsPath,
    addNewRegFileWithAbsPath,
    findByAbsolutePathWithin,
    isValidDirAbsPath,
    removeFileWithAbsPath,
  )
import Utils
  ( empty,
    resolveMaybeWithErr,
    splitByChar,
  )

-- ~
getHomeDirectoryPath :: SystemUser -> FileTree -> Either SystemError (Maybe String)
getHomeDirectoryPath systemUser tree = do
  let expectedHomeDir = getExpectedHomeDirName $ getHomeDirectoryName systemUser

  validSearchedHomeDirInTree <- findByAbsolutePathWithin expectedHomeDir tree
  case validSearchedHomeDirInTree of
    Nothing -> Right Nothing
    _ -> Right $ Just expectedHomeDir

getExpectedHomeDirName :: String -> String
getExpectedHomeDirName homeDirName = do
  if homeDirName == rootDirName
    then
      adminHomeDirPath
    else
      regUserHomeDirPath homeDirName

getHomeDirectoryName :: SystemUser -> String
getHomeDirectoryName systemUser =
  case userType systemUser of
    Admin ->
      rootDirName
    RegularUser ->
      username $ userInfo systemUser

-- cd
changeWorkingDirectory :: String -> SystemUser -> String -> FileTree -> Either SystemError String
changeWorkingDirectory filePath loggedUser currDir tree = do
  validatedPath <- getValidPath filePath loggedUser currDir tree

  isValidDirectoryInTree <- isValidDirAbsPath validatedPath tree

  if isValidDirectoryInTree
    then
      Right (validatePathsEndsWith pathsSeparatorSymbol validatedPath)
    else
      Left $ FileSystemError $ InvalidPath "The given path is not a path to a directory!"

-- rm
-- Note: In next project versions we can make LastValidRemovalState to held as a Monad in order
-- to use an early returning value (with <-). That way if we use a function like the following,
-- we can get free from the several case checks by just using "_ <- resolveEitherForRemovalState ...":
--
-- resolveEitherForRemovalState :: Either SystemError t -> FileTree -> Either LastValidRemovalState t
-- resolveEitherForRemovalState eitherValue tree = do
--    case eitherValue of
--      Left err -> Left $ LastValidRemovalState (Just err) tree
--      Right value -> Right value

data LastValidRemovalState = LastValidRemovalState (Maybe SystemError) FileTree

removeAllFrom :: [String] -> SystemUser -> String -> SystemState -> LastValidRemovalState
removeAllFrom [] _ _ (SystemState _ tree) = LastValidRemovalState Nothing tree
removeAllFrom (toRemove : restToRemove) loggedUser currDir state@(SystemState users tree) = do
  let currResolvedPath = resolveAbsPath (getAbsolutePathFrom toRemove currDir)

  case currResolvedPath of
    Left err ->
      LastValidRemovalState (Just err) tree
    Right resolvedPath -> do
      let splittedPath = splitByChar pathsSeparatorSymbol resolvedPath
          homeDirNamePosition = 2
          possibleHomeDirName = splittedPath !! (homeDirNamePosition - 1)

      if (length splittedPath == homeDirNamePosition) && isHomeDirectory possibleHomeDirName users
        then do
          let err = FileSystemError $ IllegalOperation ("Could not remove dir \"" ++ possibleHomeDirName ++ "\", because it is a home directory of a user!")
          LastValidRemovalState (Just err) tree
        else do
          let currValidatedPath = getValidPath resolvedPath loggedUser currDir tree
          case currValidatedPath of
            Left err ->
              LastValidRemovalState (Just err) tree
            Right validatedPath -> do
              let newState = tryToRemoveFileFromPath validatedPath currDir state

              case newState of
                Left err ->
                  LastValidRemovalState (Just err) tree
                Right validNewState ->
                  removeAllFrom restToRemove loggedUser currDir validNewState

tryToRemoveFileFromPath :: String -> String -> SystemState -> Either SystemError SystemState
tryToRemoveFileFromPath validatedPath currDir (SystemState users tree) = do
  foundFileToRemove <- findByAbsolutePathWithin validatedPath tree

  _ <- resolveMaybeWithErr foundFileToRemove (FileSystemError $ InvalidPath "The given path to file does not exist!")
  validTreeWithRemovedFile <- removeFileWithAbsPath validatedPath tree

  treeWithRemovedFile <- resolveMaybeWithErr validTreeWithRemovedFile (FileSystemError $ IllegalOperation "Could not remove the whole file tree!")
  validFindCurrDirInRemovedFileTree <- findByAbsolutePathWithin currDir treeWithRemovedFile

  _ <- resolveMaybeWithErr validFindCurrDirInRemovedFileTree (FileSystemError $ IllegalOperation "Could not remove a file within the current directory!")
  Right $ SystemState users treeWithRemovedFile

isHomeDirectory :: String -> [SystemUser] -> Bool
isHomeDirectory _ [] = False
isHomeDirectory nameOfDir (x : xs) =
  (getHomeDirectoryName x == nameOfDir) || isHomeDirectory nameOfDir xs

-- create files
makeDirectory :: String -> SystemUser -> String -> FileTree -> Either SystemError FileTree
makeDirectory filePath loggedUser currDir tree = do
  validatedPath <- getValidPath filePath loggedUser currDir tree
  addNewDirWithAbsPath validatedPath tree

makeRegFile :: String -> SystemUser -> String -> String -> FileTree -> Either SystemError FileTree
makeRegFile filePath loggedUser currDir regFileContent tree = do
  validatedPath <- getValidPath filePath loggedUser currDir tree
  addNewRegFileWithAbsPath validatedPath regFileContent tree

-- cat
data CatResult = OutputContent String | FileCreation FileTree

concatenateFiles :: Int -> [String] -> SystemUser -> String -> SystemState -> IO (Either SystemError CatResult)
concatenateFiles indexOfGreaterThanSign inputArr loggedUser currDir s@(SystemState _ tree) = do
  let lastIndexOfArray = length inputArr - 1
      shouldWriteToStdout = indexOfGreaterThanSign == lastIndexOfArray
      filesToReadContentFrom = takeWhile (/= ">") (drop 1 inputArr)
      currContent = getContentFrom filesToReadContentFrom loggedUser currDir tree

  case currContent of
    Left errMsg -> return $ Left errMsg
    Right validContent ->
      if shouldWriteToStdout
        then do
          contentToWrite <- validContent
          return $ Right $ OutputContent contentToWrite
        else do
          contentToWrite <- validContent
          let tr = writeContentToFile contentToWrite (inputArr !! lastIndexOfArray) loggedUser currDir s
          case tr of
            Left err -> return $ Left err
            Right validTree -> return $ Right (FileCreation validTree)

getContentFrom :: [String] -> SystemUser -> String -> FileTree -> Either SystemError (IO String)
getContentFrom filesToReadContentFrom loggedUser currDir tree = do
  if not (empty filesToReadContentFrom)
    then do
      let currContent = getContentFromFiles filesToReadContentFrom loggedUser currDir tree

      case currContent of
        Left errMsg -> Left $ FileSystemError $ CouldNotProceedOperation ("Could not obtain the content from the given files! " ++ show errMsg)
        Right validContent -> Right (return validContent)
    else
      Right getContentFromStdin

getContentFromFiles :: [String] -> SystemUser -> String -> FileTree -> Either SystemError String
getContentFromFiles [] _ _ _ = Right ""
getContentFromFiles (x : xs) loggedUser currDir tree = do
  validatedPath <- getValidPath x loggedUser currDir tree
  validFoundFileTree <- findByAbsolutePathWithin validatedPath tree
  fileTree <- resolveMaybeWithErr validFoundFileTree $ FileSystemError $ InvalidPath ("Could not find file: " ++ x)

  case fileTree of
    FileNode _ (TextContent textContent) -> do
      validRestOfTheContent <- getContentFromFiles xs loggedUser currDir tree
      Right (textContent ++ validRestOfTheContent)
    FileNode _ (Files _) ->
      Left $ FileSystemError $ InvalidPath "All of the given files to read from must be regular files! Found a directory instead!"

-- Note: we rename it just for the seek of more descriptive name and also to hide the low level function
getContentFromStdin :: IO String
getContentFromStdin = readUntilDot

writeContentToFile :: String -> String -> SystemUser -> String -> SystemState -> Either SystemError FileTree
writeContentToFile contentToWrite fileToWriteTo loggedUser currDir state@(SystemState _ tree) = do
  resolvedPath <- resolveAbsPath (getAbsolutePathFrom fileToWriteTo currDir)
  isValidRegFile <- findByAbsolutePathWithin resolvedPath tree
  
  case isValidRegFile of
    Just (FileNode _ (Files _)) ->
      Left $ FileSystemError $ InvalidPath "The given path to file must not be to a directory!"
    Just (FileNode _ (TextContent _)) -> do
      let (LastValidRemovalState maybeErr treeWithRemovedFileToWriteTo) = removeAllFrom [resolvedPath] loggedUser currDir state

      case maybeErr of
        Just _ ->
          Left $ FileSystemError $ CouldNotProceedOperation $ "Could not update the content of the given output file!"
        Nothing -> do
          verifyFileCreated fileToWriteTo loggedUser currDir contentToWrite treeWithRemovedFileToWriteTo
    Nothing ->
      verifyFileCreated fileToWriteTo loggedUser currDir contentToWrite tree

verifyFileCreated :: String -> SystemUser -> String -> String -> FileTree -> Either SystemError FileTree
verifyFileCreated fileToWriteTo loggedUser currDir contentToWrite tree = do
  let newFile = makeRegFile fileToWriteTo loggedUser currDir contentToWrite tree

  case newFile of
    Left errMsg ->
      Left $ FileSystemError $ CouldNotProceedOperation ("Unable to create the output file! " ++ show errMsg)
    Right newTree ->
      Right newTree

-- function to get the fully resolved file path that each Unix command uses
getValidPath :: String -> SystemUser -> String -> FileTree -> Either SystemError String
getValidPath path loggedUser currDir tree = do
  resolvedPath <- resolveAbsPath (getAbsolutePathFrom path currDir)
  userHomeDir <- getHomeDirectoryPath loggedUser tree
  ensureResolvedPathIsWithinUserHomeDir resolvedPath userHomeDir