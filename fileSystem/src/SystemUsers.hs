{-# LANGUAGE DeriveGeneric #-}

module SystemUsers
  ( SystemUser (..),
    UserType (..),
    UserInfo (..),
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

data SystemUser = SystemUser {userType :: UserType, userInfo :: UserInfo}
  deriving (Show, Eq, Generic)

data UserType = RegularUser | Admin
  deriving (Show, Eq, Generic)

data UserInfo = UserInfo {username :: String, password :: String}
  deriving (Show, Eq, Generic)

instance Binary SystemUser

instance Binary UserType

instance Binary UserInfo