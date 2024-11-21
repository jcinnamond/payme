module Logger (
  Logger,
  appLogger,
  withField,
)
where

import Data.Aeson (ToJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as JSON
import Data.Text (Text)

newtype Logger = Logger [JSON.Pair]

instance ToJSON Logger where
  toJSON (Logger l) = JSON.object l

appLogger :: Text -> Logger
appLogger name = Logger [("app_name", JSON.String name)]

withField :: (ToJSON a) => Logger -> Text -> a -> Logger
withField (Logger l) k v = Logger $ (Key.fromText k, JSON.toJSON v) : l