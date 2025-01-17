module IO (startSystem) where

import DataConfig
  ( defaultAdmin,
    fileTreeSerializationFilePath,
    pathsSeparatorStr,
    rootDirName,
    supportedIdentificationMenuOptions,
    supportedUnixCommands,
    usersSerializationFilePath,
  )
import FileManager
  ( ensureAllDirectoriesFromPathExist,
    safeRestoreFromFile,
    safeSaveToFile,
  )
import FileTreeStructure
  ( Content (..),
    FileTree (..),
  )
import IOStringManipulation
  ( getHelpMenu,
    getSeparatedStringInput,
    printFileContentInVimMode,
    printFormatted,
  )
import IdentificationManager
  ( getUserByUsername,
    login,
    registerRegUser,
  )
import System.IO
  ( hFlush,
    stdout,
  )
import SystemErrors
  ( IOError (..),
    SystemError (..),
  )
import SystemStateConfiguration (SystemState (..))
import SystemUsers
  ( SystemUser (..),
    UserInfo (..),
    UserType (..),
  )
import TreeOperations (findByAbsolutePathWithin)
import UnixCommands
  ( CatResult (..),
    LastValidRemovalState (..),
    changeWorkingDirectory,
    concatenateFiles,
    getValidPath,
    makeDirectory,
    makeRegFile,
    removeAllFrom,
  )
import Utils
  ( displayError,
    displayInfoStatusMessage,
    displaySuccess,
    getUserCredentials,
    indexOf,
    root,
    isBlank,
    isAnyBlank,
  )

operateWithFileSystem :: SystemUser -> String -> SystemState -> IO FileTree
operateWithFileSystem loggedUser currDir state@(SystemState users tree) = do
  putStr "$ "
  hFlush stdout
  input <- getLine

  let inputArr = getSeparatedStringInput input
      inputArrLen = length inputArr
      countArgs = inputArrLen - 1
      cmd = head inputArr
      continue = operateWithFileSystem loggedUser currDir state

  if inputArrLen == 0
    then do
      displayError "The command must not be empty!"
      continue
    else case cmd of
      "help" -> do
        case countArgs of
          0 -> getHelpMenu supportedUnixCommands
          _ -> displayError ("Invalid count of arguments given to command 'help'! Expected zero, but given " ++ show countArgs)
        continue
      "cat" ->
        case countArgs of
          0 -> do
            displayError "Expected at least one argument ('>'), but zero given!"
            continue
          _ -> do
            let grThanPositionInArray = indexOf ">" inputArr

            case grThanPositionInArray of
              Nothing -> do
                displayError "Invalid syntax! Expected '>' to be given as argument!"
                continue
              Just index -> do
                if isAnyBlank inputArr then do
                  displayError "Invalid syntax! Found blank arguments!"
                  continue
                else do
                  resuledTree <- concatenateFiles index inputArr loggedUser currDir state

                  case resuledTree of
                    Left err -> do
                      displayError $ show err
                      continue
                    Right catResult -> do
                      case catResult of
                        OutputContent outputContent -> do
                          putStrLn outputContent
                          continue
                        FileCreation newTree -> do
                          displaySuccess "Successfuly written content to the output file."
                          operateWithFileSystem loggedUser currDir (SystemState users newTree)
      "touch" -> do
        case countArgs of
          1 -> do
            let filePath = (inputArr !! 1)
            if isBlank filePath then do
              displayError "Invalid syntax! Found blank arguments!"
              continue
            else do
              let result = makeRegFile filePath loggedUser currDir "" tree
              case result of
                Left err -> do
                  displayError $ show err
                  continue
                Right newTree -> do
                  displaySuccess "Successfully created file."
                  operateWithFileSystem loggedUser currDir (SystemState users newTree)
          _ -> do
            displayError ("Invalid count of arguments given to command 'touch'! Expected one, but given " ++ show countArgs)
            continue
      "mkdir" -> do
        case countArgs of
          1 -> do
            let filePath = (inputArr !! 1)
            if isBlank filePath then do
              displayError "Invalid syntax! Found blank arguments!"
              continue
            else do
              let result = makeDirectory filePath loggedUser currDir tree

              case result of
                Left err -> do
                  displayError $ show err
                  continue
                Right newTree -> do
                  displaySuccess "Successfully created directory."
                  operateWithFileSystem loggedUser currDir (SystemState users newTree)
          _ -> do
            displayError ("Invalid count of arguments given to command 'mkdir'! Expected one, but given " ++ show countArgs)
            continue
      "ls" -> do
        if countArgs > 1 then
          displayError ("Invalid count of arguments given to command 'ls'! Expected zero or one, but given " ++ show countArgs)
        else do
          let pathToSearch = case countArgs of
                0 -> currDir
                _ -> inputArr !! 1
          if isBlank pathToSearch then do
            displayError "Invalid syntax! Found blank arguments!"
          else do
            let validPath = getValidPath pathToSearch loggedUser currDir tree
            case validPath of
              Left err ->
                displayError $ show err
              Right path -> do
                let foundTree = findByAbsolutePathWithin path tree
                case foundTree of
                  Left err ->
                    displayError $ show err
                  Right maybeTree ->
                    case maybeTree of
                      Nothing -> displayError "The given file path does not exists!"
                      Just validTree -> printFormatted validTree
        continue
      "vim" -> do
        case countArgs of
          1 -> do
            let filePath = (inputArr !! 1)
            if isBlank filePath then do
              displayError "Invalid syntax! Found blank arguments!"
            else do
              let validPath = getValidPath filePath loggedUser currDir tree

              case validPath of
                Left err ->
                  displayError $ show err
                Right path -> do
                  let foundTree = findByAbsolutePathWithin path tree

                  case foundTree of
                    Left err ->
                      displayError $ show err
                    Right maybeTree ->
                      case maybeTree of
                        Nothing -> displayError "The given file path does not exists!"
                        Just (FileNode _ (TextContent textContent)) ->
                          printFileContentInVimMode textContent
                        Just _ ->
                          displayError "Not a regular file given! Only a content of a regular file can be visualized with the command vim!"
          _ -> do
            displayError ("Invalid count of arguments given to command 'vim'! Expected one, but given " ++ show countArgs)
        continue
      "pwd" -> do
        case countArgs of
          0 ->
            putStrLn currDir
          _ ->
            displayError ("Invalid count of arguments given to command 'pwd'! Expected zero, but given " ++ show countArgs)
        continue
      "cd" ->
        case countArgs of
          1 -> do
            let filePath = (inputArr !! 1)
            if isBlank filePath then do
              displayError "Invalid syntax! Found blank arguments!"
              continue
            else do
              let newWorkingDirResult = changeWorkingDirectory filePath loggedUser currDir tree
              case newWorkingDirResult of
                Left err -> do
                  displayError $ show err
                  continue
                Right newWorkingDir ->
                  operateWithFileSystem loggedUser newWorkingDir state
          _ -> do
            displayError ("Invalid count of arguments given to command 'cd'! Expected one, but given " ++ show countArgs)
            continue
      "rm" ->
        case countArgs of
          0 -> do
            displayError "Invalid count of arguments given to command 'rm'! Expected at least one, but given zero"
            continue
          _ -> do
            let toRemoveArr = (drop 1 inputArr)
            if isAnyBlank toRemoveArr then do
              displayError "Invalid syntax! Found blank arguments!"
              continue
            else do
              let (LastValidRemovalState maybeErr treeWithRemoved) = removeAllFrom toRemoveArr loggedUser currDir state

              case maybeErr of
                Nothing ->
                  displaySuccess "Successfully removed all files from the list."
                Just err ->
                  displayError ("An error occured while removing a file from the given list! " ++ show err)

              operateWithFileSystem loggedUser currDir (SystemState users treeWithRemoved)
      "logout" ->
        case countArgs of
          0 -> do
            putStrLn "Loging out..."
            return tree
          _ -> do
            displayError ("Invalid count of arguments given to command 'logout'! Expected one, but given " ++ show countArgs)
            continue
      _ -> do
        displayError "Unknown command entered!"
        continue

identificateWithin :: SystemState -> IO ()
identificateWithin state@(SystemState users tree) = do
  putStr "Choose action (help for more): "
  hFlush stdout
  input <- getLine

  let inputArr = getSeparatedStringInput input
      inputArrLen = length inputArr
      countArgs = inputArrLen - 1
      cmd = head inputArr
      continue = identificateWithin state

  if inputArrLen == 0
    then do
      displayError "The command must not be empty!"
      continue
    else case cmd of
      "help" -> do
        case countArgs of
          0 ->
            getHelpMenu supportedIdentificationMenuOptions
          _ ->
            displayError ("Invalid count of arguments given to command 'help'! Expected zero but given " ++ show countArgs)
        continue
      "login" -> do
        case countArgs of
          0 -> do
            (currUsername, currPassword) <- getUserCredentials
            let loggedState = login (UserInfo currUsername currPassword) state
            case loggedState of
              Left err -> do
                displayError $ show err
                continue
              Right newState@(SystemState newUsers _) -> do
                displaySuccess "Successfully logged into the system."

                let loggedUser = getUserByUsername currUsername newUsers
                case loggedUser of
                  Nothing -> do
                    displayError "The current user has logged in, but unexpectedly its login data does not exist! Try to register instead!"
                    continue
                  Just u@(SystemUser Admin _) -> do
                    let loggedUserCurrDir = rootDirName

                    operatedTree <- operateWithFileSystem u loggedUserCurrDir newState
                    identificateWithin (SystemState newUsers operatedTree)
                  Just u@(SystemUser RegularUser _) -> do
                    let loggedUserCurrDir = rootDirName ++ currUsername ++ pathsSeparatorStr

                    operatedTree <- operateWithFileSystem u loggedUserCurrDir newState
                    identificateWithin (SystemState newUsers operatedTree)
          _ -> do
            displayError ("Invalid count of arguments given to command 'login'! Expected zero, but given " ++ show countArgs)
            continue
      "register" ->
        case countArgs of
          0 -> do
            (currUsername, currPassword) <- getUserCredentials
            if isAnyBlank [currUsername, currPassword] then do
              displayError "Invalid syntax! Found blank arguments within the user credentials!"
              continue
            else do
              let registeredState = registerRegUser (UserInfo currUsername currPassword) state

              case registeredState of
                Left err -> do
                  displayError $ show err
                  continue
                Right newState -> do
                  displaySuccess "Successfully registered in the system!"
                  identificateWithin newState
          _ -> do
            displayError ("Invalid count of arguments given to command 'register'! Expected zero, but given " ++ show countArgs)
            continue
      "exit" -> do
        case countArgs of
          0 -> do
            displayInfoStatusMessage "Saving the system data..."
            result <- tearDown users tree
            case result of
              Left err ->
                displayError $ show err
              Right _ -> do
                displaySuccess "System is successfully saved."
                putStrLn "Exiting the program..."
          _ -> do
            displayError ("Invalid count of arguments given to command 'exit'! Expected zero, but given " ++ show countArgs)
            continue
      _ -> do
        displayError "Unknown command entered!"
        continue

setup :: IO (Either SystemError SystemState)
setup = do
  restoredUsers <- safeRestoreFromFile usersSerializationFilePath
  case restoredUsers of
    Left err -> 
      return $ Left err
    Right validUsers -> do
      restoredFileTree <- safeRestoreFromFile fileTreeSerializationFilePath
      case restoredFileTree of
        Left err -> 
          return $ Left err
        Right validFileTree ->
          return $ Right $ SystemState validUsers validFileTree

tearDown :: [SystemUser] -> FileTree -> IO (Either SystemError SystemState)
tearDown finalUsers finalFileTree = do
  ensuredUsersDirs <- ensureAllDirectoriesFromPathExist usersSerializationFilePath
  
  case ensuredUsersDirs of
    Left err -> 
      return $ Left err
    Right _ -> do
      savedUsers <- safeSaveToFile usersSerializationFilePath finalUsers

      case savedUsers of
        Left err -> do
          return $ Left err
        Right _ -> do
          ensuredFileTreeDirs <- ensureAllDirectoriesFromPathExist fileTreeSerializationFilePath

          case ensuredFileTreeDirs of
            Left err -> 
              return $ Left err
            Right _ -> do
              savedFileSystem <- safeSaveToFile fileTreeSerializationFilePath finalFileTree

              case savedFileSystem of
                Left err -> 
                  return $ Left err
                Right _ ->                 
                  return $ Right $ SystemState finalUsers finalFileTree

startSystem :: IO ()
startSystem = do
  systemState <- setup

  case systemState of
    Left (InputOutputError (FileNotFound _)) -> do
      displayError "Some of the configure files do not exist!"
      displayInfoStatusMessage "Trying to generate default ones..."
      state <- tearDown [defaultAdmin] root

      case state of
        Left err -> do
          displayError ("Cannot generate default configure files! " ++ show err)
          displayInfoStatusMessage "System shutting down..."
        Right _ -> do
          displaySuccess "Successfully generated default configure files. Now rebooting the system..."
          startSystem
    Left err -> do
      displayError ("The system could not be set up properly! The configuration files could not be restored, nor generated! " ++ show err)
      displayInfoStatusMessage "System shuting down..."
    Right validSystemState -> do
      displaySuccess "System has successfully restored its configuration files."
      identificateWithin validSystemState