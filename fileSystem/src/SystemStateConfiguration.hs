module SystemStateConfiguration (SystemState (..)) where

import FileTreeStructure (FileTree)
import SystemUsers (SystemUser)

data SystemState = SystemState {systemUsers :: [SystemUser], systemTree :: FileTree}