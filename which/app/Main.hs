module Main (main) where

import System.Directory
import System.Environment (getArgs, getEnv)
import Control.Monad.Reader
import Which

main :: IO ()
main = do
  args <- getArgs
  let env = AppEnv
        { aeDoesDirectoryExist = doesDirectoryExist
        , aeListDirectory      = listDirectory
        , aeDoesFileExist      = doesFileExist
        , aeGetEnv             = getEnv
        }
  result <- runReaderT (findCommands args) env
  mapM_ putStrLn $ either return id result
