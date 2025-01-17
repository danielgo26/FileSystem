{-# LANGUAGE DeriveGeneric #-}

module FileTreeStructure
  ( FileTree (..),
    Content (..),
  )
where

import Data.Binary (Binary)
import GHC.Generics (Generic)

data FileTree = FileNode {fileNodeName :: String, fileNodeContent :: Content}
  deriving (Eq, Show, Generic)

data Content = TextContent String | Files [FileTree]
  deriving (Eq, Show, Generic)

instance Binary FileTree

instance Binary Content