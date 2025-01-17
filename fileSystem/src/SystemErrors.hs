module SystemErrors
  ( SystemError (..),
    FSError (..),
    IdError (..),
    IOError (..),
  )
where

import Prelude hiding (IOError)

data SystemError = FileSystemError FSError | IdentificationError IdError | InputOutputError IOError
  deriving (Eq)

data FSError = InvalidPath String | MissingHomeDirectory String | IllegalOperation String | CouldNotProceedOperation String
  deriving (Eq)

data IdError = AlreadyExistingUser String | UserNotFound String | InvalidCredentials String | IncorrectPassword String
  deriving (Eq)

data IOError = FileNotFound String | PermissionDenied String | Unknown String
  deriving (Eq)

instance Show SystemError where
  show (FileSystemError err) = "System error(FileSystem error): " ++ show err
  show (IdentificationError err) = "System error(Identification error): " ++ show err
  show (InputOutputError err) = "System error(InputOutput error): " ++ show err

instance Show FSError where
  show (InvalidPath msg) = "Invalid path - " ++ msg
  show (MissingHomeDirectory msg) = "Missing home directory - " ++ msg
  show (IllegalOperation msg) = "Illegal operation - " ++ msg
  show (CouldNotProceedOperation msg) = "Could not proceed operation - " ++ msg

instance Show IdError where
  show (AlreadyExistingUser msg) = "Already existing user - " ++ msg
  show (UserNotFound msg) = "User not found - " ++ msg
  show (InvalidCredentials msg) = "Invalid credentials - " ++ msg
  show (IncorrectPassword msg) = "Incorrect password - " ++ msg

instance Show IOError where
  show (FileNotFound msg) = "File not found - " ++ msg
  show (PermissionDenied msg) = "Permission denied - " ++ msg
  show (Unknown msg) = "Unknown error - " ++ msg
