{-# LANGUAGE QuasiQuotes #-}

module DB.Accounts (
  get,
  create,
  deposit,
  list,
  AccountDBError (..),
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
import Hasql.Session (Session)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH qualified as TH
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx
import Ledger (LedgerEntry (..))

data AccountDBError
  = SessionError Session.SessionError
  | NotFound
  | InsufficientFunds
  deriving stock (Show, Eq)

get :: Connection -> UUID -> IO (Either AccountDBError Account)
get conn uuid = do
  res <- Session.run (getSession uuid) conn
  pure $ case res of
    Left err
      | isNotFound err -> Left NotFound
      | otherwise -> Left $ SessionError err
    Right acc -> Right acc

isNotFound :: Session.SessionError -> Bool
isNotFound (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 0))) = True
isNotFound _ = False

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

list :: Connection -> IO (Either AccountDBError (Vector Account))
list conn = do
  res <- Session.run (Session.statement () listStatement) conn
  pure $ case res of
    Left err -> Left $ SessionError err
    Right accs -> Right accs

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

create :: Connection -> Account -> IO (Either AccountDBError ())
create conn account = do
  res <- Session.run (Session.statement account createStatement) conn
  pure $ case res of
    Left err -> Left $ SessionError err
    Right () -> Right ()

createStatement :: Statement Account ()
createStatement =
  lmap
    (\acc -> (acc.uuid, acc.name, acc.createdAt))
    [TH.resultlessStatement|
      insert into "accounts"
        (id, name, created_at) 
      values
        ($1 :: uuid, $2 :: text, $3 :: timestamptz)
      |]

deposit :: Connection -> UUID -> LedgerEntry -> IO (Either AccountDBError ())
deposit conn accountId ledgerEntry = do
  let transaction = depositTransaction accountId ledgerEntry
  res <- runTx conn transaction
  case res of
    Left err -> pure $ Left $ SessionError err
    Right (Left err) -> pure $ Left err
    Right (Right ()) -> pure $ Right ()

depositTransaction :: UUID -> LedgerEntry -> Tx.Transaction (Either AccountDBError ())
depositTransaction accountId ledgerEntry = do
  rowsAffected <- Tx.statement (accountId, ledgerEntry) depositStatement
  if rowsAffected > 0
    then do
      Tx.statement ledgerEntry LedgerDB.createStatement
      pure $ Right ()
    else do
      Tx.condemn
      pure $ Left InsufficientFunds

depositStatement :: Statement (UUID, LedgerEntry) Int64
depositStatement =
  lmap
    (\(accountId, le) -> (le.amount, accountId))
    [TH.rowsAffectedStatement|
      update "accounts"
      set balance = balance + $1 :: bigint
      where
        id = $2 :: uuid
        and
        balance + $1 :: bigint >= 0 :: bigint
    |]

runTx :: Connection -> Tx.Transaction a -> IO (Either Session.SessionError a)
runTx conn act = do
  Session.run (Tx.transaction Tx.ReadCommitted Tx.Write act) conn
