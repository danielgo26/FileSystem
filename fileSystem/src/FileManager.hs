module FileManager
  ( safeSaveToFile,
    safeRestoreFromFile,
    ensureAllDirectoriesFromPathExist,
  )
where

import Control.Exception
  ( IOException,
    try,
  )
import Data.Binary
  ( Binary,
    decode,
    encode,
  )
import PathOperations (separateIntoOriginPathAndFilename)
import qualified Data.ByteString.Lazy as BL
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
  )
import System.IO
  ( IOMode (ReadMode, WriteMode),
    withBinaryFile,
  )
import System.IO.Error
  ( ioeGetErrorString,
    isDoesNotExistError,
    isPermissionError,
  )
import SystemErrors
  ( IOError (..),
    SystemError (..),
  )
import Utils
  ( displayInfoStatusMessage,
    displaySuccess,
  )
import Prelude hiding (IOError)

safeSaveToFile :: (Binary a) => FilePath -> a -> IO (Either SystemError ())
safeSaveToFile filePath dataToSave = do
  result <- try (saveToFile filePath dataToSave) :: IO (Either IOException ())
  case result of
    Left ex ->
      return $ Left $ handleException ex
    Right _ -> do
      return $ Right ()

safeRestoreFromFile :: (Binary a) => FilePath -> IO (Either SystemError a)
safeRestoreFromFile filePath = do
  result <- try (restoreFromFile filePath) :: (Binary a) => IO (Either IOException a)
  case result of
    Left ex ->
      return $ Left $ handleException ex
    Right content -> do
      return $ Right content

handleException :: IOException -> SystemError
handleException ex
  | isDoesNotExistError ex = InputOutputError $ FileNotFound "File does not exist!"
  | isPermissionError ex = InputOutputError $ PermissionDenied "Permission denied!"
  | otherwise = InputOutputError $ Unknown ("Unknown error: " ++ ioeGetErrorString ex)

saveToFile :: (Binary a) => FilePath -> a -> IO ()
saveToFile filePath dataToSave = do
  let byteString = encode dataToSave

  withBinaryFile filePath WriteMode $ \handle -> do
    BL.hPut handle byteString

restoreFromFile :: (Binary a) => FilePath -> IO a
restoreFromFile filePath = do
  withBinaryFile filePath ReadMode $ \handle -> do
    byteString <- BL.hGetContents handle

    let restoredData = decode byteString
    restoredData `seq` return restoredData

ensureAllDirectoriesFromPathExist :: String -> IO (Either SystemError ())
ensureAllDirectoriesFromPathExist filePath = do
  let (mostInnerDirPath, fileName) = separateIntoOriginPathAndFilename filePath
  mostInnerDirExists <- doesDirectoryExist mostInnerDirPath

  if mostInnerDirExists
    then
      return $ Right ()
    else do
      displayInfoStatusMessage ("Trying to create all the directories from the path to " ++ fileName ++ "...")
      createAllDirectoriesFrom mostInnerDirPath

createAllDirectoriesFrom :: FilePath -> IO (Either SystemError ())
createAllDirectoriesFrom filePath = do
  result <- try (createDirectoryIfMissing True filePath) :: IO (Either IOException ())

  case result of
    Left ex ->
      if isPermissionError ex
        then
          return $ Left $ InputOutputError $ PermissionDenied "Permission denied! Cannot create the directories from the given path!"
        else
          return $ Left $ InputOutputError $ Unknown ("Unknown error: " ++ ioeGetErrorString ex)
    Right _ -> do
      displaySuccess "All directories from the path are created successfully."
      return $ Right ()
