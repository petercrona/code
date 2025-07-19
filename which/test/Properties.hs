{-# LANGUAGE LambdaCase #-}

import Which
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

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

prop_missingArg :: Property
prop_missingArg = ioProperty $
  runReaderT (findCommands Nothing) testEnv <&> \case
    Left err -> err == "Missing arg"
    Right _  -> False

prop_notFound :: NonEmpty String -> Property
prop_notFound cmds =
  -- Only test with commands that are not any of ["ls","cat","env"]
  let cmds' = fmap (\s -> if s `elem` ["ls","cat","env"] then s ++ "_noexist" else s) cmds
  in ioProperty $ runReaderT (findCommands (Just cmds')) testEnv <&> \case
      Left err -> err == "Didn't find command"
      Right _  -> False

prop_resultsEndWithCommand :: NonEmpty String -> Property
prop_resultsEndWithCommand cmds =
  ioProperty $ runReaderT (findCommands (Just cmds)) testEnv <&> \case
    Left _      -> True
    Right files -> and $ zipWith (\f c -> ("/" <> c) `isSuffixOf` f) (toList files) (toList cmds)

prop_foundIsNotLeft :: Property
prop_foundIsNotLeft =
  ioProperty $ runReaderT (findCommands (Just ("ls" :| []))) testEnv <&> \case
    Right xs -> "/bin/ls" `elem` xs
    Left _   -> False

main :: IO ()
main = do
  quickCheck prop_missingArg
  quickCheck prop_notFound
  quickCheck prop_resultsEndWithCommand
  quickCheck prop_foundIsNotLeft
