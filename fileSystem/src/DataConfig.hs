module DataConfig
  ( pathsSeparatorSymbol,
    pathsSeparatorStr,
    rootDirName,
    backReference,
    adminHomeDirPath,
    regUserHomeDirPath,
    usersSerializationFilePath,
    fileTreeSerializationFilePath,
    defaultAdmin,
    supportedUnixCommands,
    supportedIdentificationMenuOptions,
  )
where

import SystemUsers
  ( SystemUser (..),
    UserInfo (..),
    UserType (..),
  )

pathsSeparatorSymbol :: Char
pathsSeparatorSymbol = '/'

pathsSeparatorStr :: String
pathsSeparatorStr = "/"

rootDirName :: String
rootDirName = "/"

backReference :: String
backReference = ".."

adminHomeDirPath :: String
adminHomeDirPath = rootDirName

regUserHomeDirPath :: String -> String
regUserHomeDirPath nameOfRegUser = rootDirName ++ nameOfRegUser ++ pathsSeparatorStr

usersSerializationFilePath :: String
usersSerializationFilePath = "SerializationFiles/SystemUsers/users.bin"

fileTreeSerializationFilePath :: String
fileTreeSerializationFilePath = "SerializationFiles/FileSystem/filetree.bin"

defaultAdmin :: SystemUser
defaultAdmin = SystemUser Admin $ UserInfo "admin" "admin"

supportedUnixCommands :: [String]
supportedUnixCommands = ["help", "cat", "touch", "mkdir", "ls", "vim", "pwd", "cd", "rm", "logout"]

supportedIdentificationMenuOptions :: [String]
supportedIdentificationMenuOptions = ["help", "login", "register", "exit"]