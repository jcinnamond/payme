{-# LANGUAGE UndecidableInstances #-}

module Env (GetEnv (..), Error (..)) where

import Data.Data (Proxy (..))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)

class GetEnv a where
  get :: String -> IO (Either Error a)
  getDefault :: a -> String -> IO (Either Error a)
  getDefault def name = do
    v <- get name
    pure $ case v of
      Right x -> Right x
      Left (MissingVariable _) -> Right def
      err -> err

data Error
  = MissingVariable String
  | InvalidValue SomeTypeRep String String
  deriving stock (Show, Eq)

instance {-# OVERLAPPING #-} GetEnv String where
  get = get'

instance (Typeable a, Read a) => GetEnv a where
  get name = do
    v <- get' name
    pure $ case v of
      Left err -> Left err
      Right x -> case readMaybe x of
        Just x' -> Right x'
        Nothing -> Left $ InvalidValue (someTypeRep (Proxy :: Proxy a)) name x

get' :: String -> IO (Either Error String)
get' name = do
  v <- lookupEnv name
  pure $ case v of
    Just x -> Right x
    Nothing -> Left $ MissingVariable name

