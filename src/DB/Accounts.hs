{-# LANGUAGE QuasiQuotes #-}

module DB.Accounts (
  get,
) where

import Account (Account (..))
import Data.Profunctor (Profunctor (dimap))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH qualified as TH

get :: Connection -> UUID -> IO (Either Session.SessionError Account)
get conn uuid = Session.run (getSession uuid) conn

getSession :: UUID -> Session Account
getSession uuid = Session.statement uuid getStatement

getStatement :: Statement UUID Account
getStatement =
  dimap
    Prelude.id
    toAccount
    [TH.singletonStatement|
      select 
        id :: uuid, 
        name :: text, 
        created_at :: timestamptz 
      from "accounts"
      where
        id = $1 :: uuid
      |]

toAccount :: (UUID, Text, UTCTime) -> Account
toAccount (uuid, name, created_at) =
  Account uuid name created_at