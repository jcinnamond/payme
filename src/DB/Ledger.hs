{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Ledger where

import Data.Profunctor (Profunctor (..))
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector qualified as Vector
import GHC.Int (Int64)
import Hasql.Connection (Connection)
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH qualified as TH
import Ledger (LedgerEntry (..))

list :: Connection -> IO (Either Session.SessionError [LedgerEntry])
list = Session.run listSession

listSession :: Session [LedgerEntry]
listSession = Session.statement () listStatement

listStatement :: Statement () [LedgerEntry]
listStatement =
  rmap
    (\v -> Vector.toList $ toLedgerEntry <$> v)
    [TH.vectorStatement|
      select 
        id :: uuid, 
        amount :: int8, 
        datetime :: timestamptz, 
        account_id :: uuid
      from "ledger_entries"
      |]

toLedgerEntry :: (UUID, Int64, UTCTime, UUID) -> LedgerEntry
toLedgerEntry (uuid, amount, datetime, account_id) =
  LedgerEntry uuid amount datetime account_id

create :: Connection -> LedgerEntry -> IO (Either Session.SessionError LedgerEntry)
create conn le = Session.run (createSession le) conn

createSession :: LedgerEntry -> Session LedgerEntry
createSession le = Session.statement le createStatement

createStatement :: Statement LedgerEntry LedgerEntry
createStatement =
  dimap
    (\le -> (le.id, le.amount, le.datetime, le.account_id))
    ( \(uuid, amount, datetime, account_id) ->
        LedgerEntry uuid amount datetime account_id
    )
    [TH.singletonStatement|
      insert into "ledger_entries" 
        values ($1 :: uuid, $2 :: int8, $3 :: timestamptz, $4 :: uuid)
      returning 
        id :: uuid, 
        amount :: int8, 
        datetime :: timestamptz, 
        account_id :: uuid
      |]