{-# LANGUAGE QuasiQuotes #-}

module LoggerSpec (spec) where

import Data.Aeson qualified as JSON
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import Logger (appLogger, emptyLogger, (.=))
import Logger qualified
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "sets log fields" $ do
    let baseLogger = emptyLogger
        l =
          Logger.withFields
            baseLogger
            [ "field1" .= (123 :: Int)
            , "field2" .= ("something" :: Text)
            ]

    JSON.toJSON l `shouldBe` [aesonQQ| {field1: 123, field2: "something"} |]

  it "combines log fields" $ do
    let baseLogger = appLogger "test-app"
        l =
          Logger.withFields
            baseLogger
            [ "field1" .= (123 :: Int)
            , "field2" .= ("something" :: Text)
            ]
        l' =
          Logger.withFields
            l
            ["field3" .= ("another thing" :: Text)]
    JSON.toJSON l'
      `shouldBe` [aesonQQ| { app_name: "test-app"
                           , field1: 123
                           , field2: "something"
                           , field3: "another thing"} |]
