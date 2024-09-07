{-# LANGUAGE QuasiQuotes #-}

module DB.Accounts (
  get,
) where

import Account (Account (..))
import Data.Profunctor (Profunctor (dimap))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH qualified as TH

get :: Connection -> UUID -> IO (Either Session.SessionError (Maybe Account))
get conn uuid = Session.run (getSession uuid) conn

getSession :: UUID -> Session (Maybe Account)
getSession uuid = Session.statement uuid getStatement

getStatement :: Statement UUID (Maybe Account)
getStatement =
  dimap
    Prelude.id
    (\v -> safeHead $ toAccount <$> v)
    [TH.vectorStatement|
      select 
        id :: uuid, 
        name :: text, 
        created_at :: timestamptz 
      from "accounts"
      where
        id = $1 :: uuid
      |]

safeHead :: Vector Account -> Maybe Account
safeHead v
  | Vector.null v = Nothing
  | otherwise = Just $ Vector.head v

toAccount :: (UUID, Text, UTCTime) -> Account
toAccount (uuid, name, created_at) =
  Account uuid name created_at