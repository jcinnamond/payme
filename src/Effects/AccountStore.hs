{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.AccountStore (
  AccountStore (..),
  getAccount,
  listAccounts,
  deposit,
  runAccountStoreIO,
  Error,
) where

import Account (Account (..))
import AccountWithLedger (AccountWithLedger (..))
import DB.Accounts qualified as AccountDB
import Data.Aeson (ToJSON)
import Data.Int (Int64)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as V4
import Data.Vector (Vector)
import Effectful (Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Effects.LedgerStore (LedgerStore)
import Effects.LedgerStore qualified as LedgerStore
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Ledger (LedgerEntry (..))

data Error
  = SessionError String
  | InsufficientFunds
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AccountStore :: Effect where
  GetAccount :: UUID -> AccountStore m (Either Error (Maybe AccountWithLedger))
  ListAccounts :: AccountStore m (Either Error (Vector Account))
  Deposit :: UUID -> Int64 -> AccountStore m (Either Error ())

makeEffect ''AccountStore

runAccountStoreIO ::
  (IOE :> es, LedgerStore :> es) =>
  Connection ->
  Eff (AccountStore : es) a ->
  Eff es a
runAccountStoreIO conn = interpret $ \_ -> \case
  GetAccount uuid -> do
    account <- liftIO (AccountDB.get conn uuid)
    case account of
      Left AccountDB.NotFound -> pure $ Right Nothing
      Left err -> pure $ Left $ SessionError $ show err
      Right acc -> addLedger acc
  ListAccounts -> do
    account <- liftIO (AccountDB.list conn)
    case account of
      Left err -> pure $ Left $ SessionError $ show err
      Right accs -> pure $ Right accs
  Deposit accountId amount -> do
    convertError
      <$> liftIO
        ( do
            uuid <- V4.nextRandom
            datetime <- getCurrentTime
            let le = LedgerEntry{..}
            AccountDB.deposit conn accountId le
        )

convertError :: Either AccountDB.AccountDBError () -> Either Error ()
convertError (Left err) = Left $ SessionError $ show err
convertError (Right ()) = pure ()

addLedger ::
  (IOE :> es, LedgerStore :> es) =>
  Account ->
  Eff es (Either Error (Maybe AccountWithLedger))
addLedger acc = do
  ledger <- LedgerStore.loadLedger acc.uuid
  pure $
    mapResult
      (SessionError . show)
      (Just . AccountWithLedger acc)
      ledger

mapResult :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapResult f _ (Left x) = Left $ f x
mapResult _ f (Right x) = Right $ f x
