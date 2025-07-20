{-# LANGUAGE LambdaCase #-}

import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Which

testEnv :: AppEnv
testEnv =
  AppEnv
    { aeDoesDirectoryExist = \dir -> return (dir `elem` ["/bin", "/usr/bin"]),
      aeListDirectory = \case
        "/bin" -> return ["ls", "cat"]
        "/usr/bin" -> return ["ls", "env"]
        _ -> return [],
      aeDoesFileExist = \_ -> return True,
      aeGetEnv = \_ -> return "/bin:/usr/bin:/doesnotexist"
    }

prop_missingArg :: Property
prop_missingArg =
  ioProperty $
    runReaderT (findCommands []) testEnv <&> \case
      Left err -> err == "Missing arg"
      Right _ -> False

prop_notFound :: NonEmpty String -> Property
prop_notFound cmds =
  -- Only test with commands that are not any of ["ls","cat","env"]
  let cmds' = fmap (\s -> if s `elem` ["ls", "cat", "env"] then s ++ "_noexist" else s) (toList cmds)
   in ioProperty $
        runReaderT (findCommands cmds') testEnv <&> \case
          Left err -> err == "Didn't find command"
          Right _ -> False

prop_resultsEndWithCommand :: [String] -> Property
prop_resultsEndWithCommand cmds =
  ioProperty $
    runReaderT (findCommands cmds) testEnv <&> \case
      Left _ -> True
      Right files -> and $ zipWith (\f c -> ("/" <> c) `isSuffixOf` f) (toList files) cmds

prop_foundIsNotLeft :: Property
prop_foundIsNotLeft =
  ioProperty $
    runReaderT (findCommands ["ls"]) testEnv <&> \case
      Right xs -> "/bin/ls" `elem` xs
      Left _ -> False

main :: IO ()
main = do
  quickCheck prop_missingArg
  quickCheck prop_notFound
  quickCheck prop_resultsEndWithCommand
  quickCheck prop_foundIsNotLeft
