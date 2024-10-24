{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module Config (
  load,
  Config (..),
  loadTest,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Env qualified

data Config = Config
  { dbConnection :: ByteString
  , dbIdleTime :: Double
  , dbPoolSize :: Int
  }
  deriving (Show, Eq)

data ConfigError = MissingEnvVar Text | InvalidEnvVarType Text
  deriving (Show, Eq)

load :: IO (Either Env.Error Config)
load = do
  dbConnection <- Env.get "PAYME_DB_CONNECTION"
  dbIdleTime <- Env.getDefault 10 "PAYME_DB_IDLE_TIME"
  dbPoolSize <- Env.getDefault 5 "PAYME_DB_POOL_SIZE"
  pure $
    Config
      <$> (BS.pack <$> dbConnection)
      <*> dbIdleTime
      <*> dbPoolSize

loadTest :: IO (Either Env.Error Config)
loadTest = do
  dbConnection <- Env.get "PAYME_TEST_DB_CONNECTION"
  dbIdleTime <- Env.getDefault 10 "PAYME_DB_IDLE_TIME"
  dbPoolSize <- Env.getDefault 5 "PAYME_DB_POOL_SIZE"
  pure $
    Config
      <$> (BS.pack <$> dbConnection)
      <*> dbIdleTime
      <*> dbPoolSize