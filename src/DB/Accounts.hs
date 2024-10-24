{-# LANGUAGE QuasiQuotes #-}

module DB.Accounts (
  get,
  create,
  deposit,
  list,
) where

import Account (Account (..))
import DB.Ledger qualified as LedgerDB
import Data.Int (Int64)
import Data.Profunctor (Profunctor (dimap, rmap), lmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Connection (Connection)
import Hasql.Session (Session, SessionError)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH qualified as TH
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx
import Ledger (LedgerEntry (..))

get :: Connection -> UUID -> IO (Either SessionError Account)
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
        balance :: bigint,
        created_at :: timestamptz 
      from "accounts"
      where
        id = $1 :: uuid
      |]

toAccount :: (UUID, Text, Int64, UTCTime) -> Account
toAccount (uuid, name, balance, created_at) =
  Account uuid name balance created_at

list :: Connection -> IO (Either SessionError (Vector Account))
list = Session.run (Session.statement () listStatement)

listStatement :: Statement () (Vector Account)
listStatement = do
  rmap
    (V.map toAccount)
    [TH.vectorStatement|
      select 
        id :: uuid, 
        name :: text, 
        balance :: bigint,
        created_at :: timestamptz 
      from "accounts"
      |]

create :: Connection -> Account -> IO (Either SessionError ())
create conn account = Session.run (Session.statement account createStatement) conn

createStatement :: Statement Account ()
createStatement =
  lmap
    (\acc -> (acc.id, acc.name, acc.createdAt))
    [TH.resultlessStatement|
      insert into "accounts"
        (id, name, created_at) 
      values
        ($1 :: uuid, $2 :: text, $3 :: timestamptz)
      |]

deposit :: Connection -> Account -> LedgerEntry -> IO (Either SessionError ())
deposit conn account ledgerEntry = do
  let transaction =
        Tx.statement (account, ledgerEntry) depositStatement
          <> Tx.statement ledgerEntry LedgerDB.createStatement
  runTx conn transaction

depositStatement :: Statement (Account, LedgerEntry) ()
depositStatement =
  lmap
    (\(acc, le) -> (le.amount, acc.id))
    [TH.resultlessStatement|
      update "accounts"
      set balance = balance + $1 :: bigint
      where
        id = $2 :: uuid
    |]

runTx :: Connection -> Tx.Transaction a -> IO (Either SessionError a)
runTx conn act = do
  Session.run (Tx.transaction Tx.ReadCommitted Tx.Write act) conn
