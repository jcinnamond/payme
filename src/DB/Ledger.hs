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

list :: Connection -> UUID -> IO (Either Session.SessionError [LedgerEntry])
list conn accountId = Session.run (listSession accountId) conn

listSession :: UUID -> Session [LedgerEntry]
listSession accountId = Session.statement accountId listStatement

listStatement :: Statement UUID [LedgerEntry]
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
      where
        account_id = $1 :: uuid
      |]

toLedgerEntry :: (UUID, Int64, UTCTime, UUID) -> LedgerEntry
toLedgerEntry (uuid, amount, datetime, account_id) =
  LedgerEntry uuid amount datetime account_id

create :: Connection -> LedgerEntry -> IO (Either Session.SessionError ())
create conn le = Session.run (createSession le) conn

createSession :: LedgerEntry -> Session ()
createSession le = Session.statement le createStatement

createStatement :: Statement LedgerEntry ()
createStatement =
  lmap
    (\le -> (le.id, le.amount, le.datetime, le.account_id))
    [TH.resultlessStatement|
      insert into "ledger_entries" 
        values ($1 :: uuid, $2 :: int8, $3 :: timestamptz, $4 :: uuid)
      |]

get :: Connection -> UUID -> IO (Either Session.SessionError LedgerEntry)
get conn ledgerId = Session.run (getSession ledgerId) conn

getSession :: UUID -> Session LedgerEntry
getSession ledgerId = Session.statement ledgerId getStatement

getStatement :: Statement UUID LedgerEntry
getStatement =
  rmap
    toLedgerEntry
    [TH.singletonStatement|
      select 
        id :: uuid, 
        amount :: int8, 
        datetime :: timestamptz, 
        account_id :: uuid
      from "ledger_entries"
      where
        id = $1 :: uuid
      |]
