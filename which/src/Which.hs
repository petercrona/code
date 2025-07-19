{-# LANGUAGE RecordWildCards #-}

module Which
  ( AppEnv(..)
  , findCommands
  ) where

import Data.Function (on)
import Control.Monad.Reader
import Data.List (find, isSuffixOf, nubBy)
import Data.List.Split (splitOn)
import System.FilePath ((</>))
import Control.Monad (filterM)
import Data.List.NonEmpty (NonEmpty, nonEmpty, head, toList)
import Data.Maybe (catMaybes, mapMaybe)

data AppEnv = AppEnv
  { aeDoesDirectoryExist :: FilePath -> IO Bool
  , aeListDirectory      :: FilePath -> IO [FilePath]
  , aeDoesFileExist      :: FilePath -> IO Bool
  , aeGetEnv             :: String   -> IO String
  }

type AppM = ReaderT AppEnv IO

findCommands :: Maybe (NonEmpty String) -> AppM (Either String (NonEmpty FilePath))
findCommands Nothing = return (Left "Missing arg")
findCommands (Just commands) = do
  systemCommands <- getAllCommands
  let foundCommands = mapMaybe (flip find systemCommands . commandMatch) . toList $ commands
  return $ maybe (Left "Didn't find command") Right (nonEmpty foundCommands)

  where commandMatch command = isSuffixOf ("/" <> command)

getAllCommands :: AppM [FilePath]
getAllCommands = do
  AppEnv{..} <- ask
  path <- liftIO $ aeGetEnv "PATH"
  let paths = splitOn ":" path
  concat <$> traverse listDirectoryAbsolute paths

listDirectoryAbsolute :: FilePath -> AppM [FilePath]
listDirectoryAbsolute dir = do
  AppEnv{..} <- ask
  dirExists  <- liftIO $ aeDoesDirectoryExist dir
  if dirExists
    then do
      files <- liftIO $ aeListDirectory dir
      let absFiles = map (dir </>) files
      filterM (liftIO . aeDoesFileExist) absFiles
    else return []
