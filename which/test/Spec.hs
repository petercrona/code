{-# LANGUAGE LambdaCase #-}

import Test.HUnit
import Which
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Functor ((<&>))

-- | A fake AppEnv for testing
testEnv :: AppEnv
testEnv = AppEnv
  { aeDoesDirectoryExist = \dir -> return (dir `elem` ["/bin", "/usr/bin"])
  , aeListDirectory      = \case
      "/bin"     -> return ["ls", "cat"]
      "/usr/bin" -> return ["ls", "env"]
      _          -> return []
  , aeDoesFileExist      = \_ -> return True
  , aeGetEnv             = \_ -> return "/bin:/usr/bin:/doesnotexist"
  }

runTest :: [String] -> IO (Either String (NonEmpty FilePath))
runTest args = runReaderT (findCommands args) testEnv

main :: IO ()
main = runTestTTAndExit $ TestList
  [ "Missing arg" ~:
      runTest [] <&> \case
        Left err -> err @?= "Missing arg"
        Right _  -> assertFailure "Expected error for missing arg"

  , "Not found" ~:
      runTest ["foo"] <&> \case
        Left err -> err @?= "Didn't find command"
        Right _  -> assertFailure "Expected not found"

  , "Found ls (should pick /bin/ls, first match)" ~:
      runTest ["ls"] <&> \case
        Right files -> files @?= "/bin/ls" :| []
        Left err    -> assertFailure $ "Expected match, got error: " ++ err

  , "Found cat (should pick /bin/cat)" ~:
      runTest ["cat"] <&> \case
        Right files -> files @?= "/bin/cat" :| []
        Left err    -> assertFailure $ "Expected match, got error: " ++ err

  , "Found env (should pick /usr/bin/env)" ~:
      runTest ["env"] <&> \case
        Right files -> files @?= "/usr/bin/env" :| []
        Left err    -> assertFailure $ "Expected match, got error: " ++ err

  , "Multiple commands (ls and env, order preserved, both found)" ~:
      runTest ["ls", "env"] <&> \case
        Right files -> files @?= "/bin/ls" :| ["/usr/bin/env"]
        Left err    -> assertFailure $ "Expected matches, got error: " ++ err

  , "Multiple commands, one not found (ls and foo)" ~:
      runTest ["ls", "foo"] <&> \case
        Right files -> files @?= "/bin/ls" :| []
        Left err    -> assertFailure $ "Expected one match, got error: " ++ err

  , "Duplicate command names (should still only match first occurrence per command)" ~:
      let envDup = testEnv { aeListDirectory = \case
            "/bin"     -> return ["ls", "cat"]
            "/usr/bin" -> return ["ls", "env"]
            _          -> return []
          }
      in runReaderT (findCommands ["ls"]) envDup <&> \case
            Right files -> files @?= "/bin/ls" :| []
            Left err    -> assertFailure $ "Expected first ls, got error: " ++ err

  , "Nonexistent dir in PATH is ignored" ~:
      let envExtra = testEnv { aeDoesDirectoryExist = \dir ->
                                 return (dir `elem` ["/bin", "/usr/bin"]) }
      in runReaderT (findCommands ["ls"]) envExtra <&> \case
            Right files -> files @?= "/bin/ls" :| []
            Left err    -> assertFailure $ "Expected match, got error: " ++ err
  ]
