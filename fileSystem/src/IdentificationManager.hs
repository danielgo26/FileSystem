module IdentificationManager
  ( registerRegUser,
    registerAdmin,
    login,
    getUserByUsername,
  )
where

import DataConfig (rootDirName)
import FileTreeStructure (FileTree)
import SystemErrors
  ( IdError (..),
    SystemError (..),
  )
import SystemStateConfiguration (SystemState (..))
import SystemUsers
  ( SystemUser (..),
    UserInfo (..),
    UserType (..),
  )
import TreeOperations (addNewDirWithAbsPath)

login :: UserInfo -> SystemState -> Either SystemError SystemState
login (UserInfo "" _) _ = Left $ IdentificationError $ InvalidCredentials "Username cannot be empty!"
login (UserInfo _ "") _ = Left $ IdentificationError $ InvalidCredentials "Password cannot be empty!"
login (UserInfo usernameForLogin passwordForLogin) (SystemState users tree) = do
  let searchedUser = getUserByUsername usernameForLogin users
  case searchedUser of
    Nothing ->
      Left $ IdentificationError $ UserNotFound ("User: " ++ usernameForLogin ++ " is not found in the system!")
    Just foundUser -> do
      let currUserInfo = userInfo foundUser
      let passwordsMatch = checkPasswordsMatch passwordForLogin currUserInfo

      if passwordsMatch
        then
          Right $ SystemState (foundUser : users) tree
        else
          Left $ IdentificationError $ IncorrectPassword "The passwords do not match!"

checkPasswordsMatch :: String -> UserInfo -> Bool
checkPasswordsMatch passToCheckWith (UserInfo _ pass)
  | pass == passToCheckWith = True
  | otherwise = False

registerRegUser :: UserInfo -> SystemState -> Either SystemError SystemState
registerRegUser userInfoToRegister systemState =
  register userInfoToRegister systemState (SystemUser RegularUser)

registerAdmin :: UserInfo -> SystemState -> Either SystemError SystemState
registerAdmin userInfoToRegister systemState =
  register userInfoToRegister systemState (SystemUser Admin)

register :: UserInfo -> SystemState -> (UserInfo -> SystemUser) -> Either SystemError SystemState
register (UserInfo "" _) _ _ = Left $ IdentificationError $ InvalidCredentials "Username cannot be empty!"
register (UserInfo _ "") _ _ = Left $ IdentificationError $ InvalidCredentials "Password cannot be empty!"
register ui@(UserInfo usernameToReg _) (SystemState users tree) determineRole = do
  let searchedUser = getUserByUsername usernameToReg users
  case searchedUser of
    Nothing -> do
      treeWithAddedHomeDir <- createHomeDirectory (rootDirName ++ usernameToReg) tree
      Right $ SystemState (determineRole ui : users) treeWithAddedHomeDir
    Just _ -> do
      Left $ IdentificationError $ AlreadyExistingUser ("User: " ++ usernameToReg ++ " is already is the system!")

getUserByUsername :: String -> [SystemUser] -> Maybe SystemUser
getUserByUsername _ [] = Nothing
getUserByUsername usernameToSearch (user : rest) = do
  let currUsername = username $ userInfo user
  if usernameToSearch == currUsername
    then
      Just user
    else
      getUserByUsername usernameToSearch rest

-- Note: we rename it just for the seek of more descriptive name and also to hide the low level function
createHomeDirectory :: String -> FileTree -> Either SystemError FileTree
createHomeDirectory = addNewDirWithAbsPath
