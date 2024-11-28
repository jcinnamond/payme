{-# LANGUAGE AllowAmbiguousTypes #-}

module Logger (
  Logger,
  LogField,
  appLogger,
  withFields,
  emptyLogger,
  (.=),
)
where

import Data.Aeson (ToJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types qualified as JSON
import Data.Text (Text)

newtype Logger = Logger [JSON.Pair]

instance ToJSON Logger where
  toJSON (Logger l) = JSON.object l

emptyLogger :: Logger
emptyLogger = Logger []

appLogger :: Text -> Logger
appLogger name = Logger [("app_name", JSON.String name)]

data LogField = forall a. (ToJSON a) => LogField Text a

withFields :: Logger -> [LogField] -> Logger
withFields (Logger pairs) fields = Logger $ pairs <> (toPair <$> fields)
 where
  toPair :: LogField -> JSON.Pair
  toPair (LogField k v) = (Key.fromText k, JSON.toJSON v)

(.=) :: forall a. (ToJSON a) => Text -> a -> LogField
(.=) = LogField
infixr 8 .=