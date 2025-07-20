module Main (main) where

import Control.Monad.Reader
import System.Directory
import System.Environment (getArgs, getEnv)
import Which

main :: IO ()
main = do
  args <- getArgs
  let env =
        AppEnv
          { aeDoesDirectoryExist = doesDirectoryExist,
            aeListDirectory = listDirectory,
            aeDoesFileExist = doesFileExist,
            aeGetEnv = getEnv
          }
  result <- runReaderT (findCommands args) env
  mapM_ putStrLn $ either return id result
