module EnvSpec (spec) where

import Control.Exception (bracket_)
import Data.Proxy (Proxy (..))
import Env qualified
import System.Environment (setEnv, unsetEnv)
import Test.Hspec (Spec, describe, it, shouldBe)
import Type.Reflection (someTypeRep)

spec :: Spec
spec = do
  it "gets a string value" $ do
    withEnv "WOO" "1" $ do
      res <- Env.get "WOO"
      res `shouldBe` (Right "1" :: Either Env.Error String)

  it "gets an int value" $ do
    withEnv "WOO" "1" $ do
      res <- Env.get "WOO"
      res `shouldBe` (Right 1 :: Either Env.Error Int)

  describe "with a missing env var" $ do
    it "returns an error when getting a string" $ do
      res <- Env.get "WOO"
      res `shouldBe` (Left $ Env.MissingVariable "WOO" :: Either Env.Error String)

    it "returns an error when getting an int" $ do
      res <- Env.get "WOO"
      res `shouldBe` (Left $ Env.MissingVariable "WOO" :: Either Env.Error Int)

  describe "with an invalid value" $ do
    it "returns an error" $ do
      withEnv "WOO" "not an integer" $ do
        res <- Env.get "WOO"
        res `shouldBe` (Left $ Env.InvalidValue (someTypeRep (Proxy :: Proxy Int)) "WOO" "not an integer" :: Either Env.Error Int)

  describe "get with default" $ do
    it "returns the value when set" $ do
      withEnv "WOO" "10" $ do
        res <- Env.getDefault 20 "WOO"
        res `shouldBe` (Right 10 :: Either Env.Error Int)

    it "returns the default when env var is not set" $ do
      res <- Env.getDefault 20 "WOO"
      res `shouldBe` (Right 20 :: Either Env.Error Int)

    it "returns an error if env var is invalid" $ do
      withEnv "WOO" "not an integer" $ do
        res <- Env.getDefault 20 "WOO"
        res `shouldBe` (Left $ Env.InvalidValue (someTypeRep (Proxy :: Proxy Int)) "WOO" "not an integer" :: Either Env.Error Int)

withEnv :: String -> String -> IO () -> IO ()
withEnv name value =
  bracket_
    (setEnv name value)
    (unsetEnv name)
