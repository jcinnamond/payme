{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BalanceSpec (spec) where

import Account (Account (..))
import Config qualified
import Control.Exception (bracket)
import DB.Accounts qualified as AccountsDB
import DB.Connection qualified as DBConn
import DB.Ledger qualified as LedgerDB
import Data.Int (Int64)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as V4
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Ledger (LedgerEntry (..), mkCredit, mkDebit)
import Migrations qualified
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Prelude hiding (truncate)

spec :: Spec
spec = do
  around withDatabase $ do
    describe "depositing money" $ do
      it "updates the account balance" $ \(conn, acc) -> do
        _ <- deposit conn acc 10
        _ <- deposit conn acc 50

        (Right acc') <- AccountsDB.get conn acc.id
        acc'.balance `shouldBe` 60

      it "creates a ledger entry" $ \(conn, acc) -> do
        uuid <- deposit conn acc 10

        (Right le) <- LedgerDB.get conn uuid
        le.id `shouldBe` uuid
        le.amount `shouldBe` 10
        le.account_id `shouldBe` acc.id

    describe "withdrawing money" $ do
      it "updates the account balance" $ \(conn, acc) -> do
        _ <- deposit conn acc 50
        _ <- withdraw conn acc 10

        (Right acc') <- AccountsDB.get conn acc.id
        acc'.balance `shouldBe` 40

      it "creates a ledger entry" $ \(conn, acc) -> do
        uuid <- withdraw conn acc 10

        (Right le) <- LedgerDB.get conn uuid
        le.id `shouldBe` uuid
        le.amount `shouldBe` -10
        le.account_id `shouldBe` acc.id

withDatabase :: ((Hasql.Connection, Account) -> IO ()) -> IO ()
withDatabase = do
  bracket setupDB cleanupDB

setupDB :: IO (Hasql.Connection, Account)
setupDB = do
  conn <- getDBConnection
  acc <- createAccount conn
  pure (conn, acc)

cleanupDB :: (Hasql.Connection, Account) -> IO ()
cleanupDB (conn, _) = do
  res <-
    flip Session.run conn $
      Session.pipeline $
        sequenceA
          ( simpleStatement "truncate ledger_entries"
          , simpleStatement "truncate accounts cascade"
          )
  case res of
    Left err -> error $ show err
    Right _ ->
      Hasql.release conn
 where
  simpleStatement x =
    Pipeline.statement () (Statement x Encoders.noParams Decoders.noResult True)

createAccount :: Hasql.Connection -> IO Account
createAccount conn = do
  uuid <- V4.nextRandom
  let name = "Test account"
  createdAt <- getCurrentTime
  let acc = Account uuid name 0 createdAt
  (Right ()) <- AccountsDB.create conn acc
  pure acc

getDBConnection :: IO Hasql.Connection
getDBConnection = do
  (Right config) <- Config.loadTest
  res <- DBConn.connection config
  case res of
    Left err -> error $ show err
    Right c -> do
      Migrations.initialize c
      Migrations.migrate c
      pure c

deposit :: Hasql.Connection -> Account -> Int64 -> IO UUID
deposit conn acc amount = do
  ledgerUUID <- V4.nextRandom
  createdAt <- getCurrentTime
  let (Right le) = mkCredit acc ledgerUUID amount createdAt

  depositRes <- AccountsDB.deposit conn acc le
  depositRes `shouldBe` Right ()
  pure ledgerUUID

withdraw :: Hasql.Connection -> Account -> Int64 -> IO UUID
withdraw conn acc amount = do
  ledgerUUID <- V4.nextRandom
  createdAt <- getCurrentTime
  let (Right le) = mkDebit acc ledgerUUID (-amount) createdAt

  depositRes <- AccountsDB.deposit conn acc le
  depositRes `shouldBe` Right ()
  pure ledgerUUID
