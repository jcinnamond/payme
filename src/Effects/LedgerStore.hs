{-# LANGUAGE LambdaCase #-}

module Effects.LedgerStore (
  LedgerStore (..),
  loadLedger,
  createLedgerEntry,
  runLedgerStoreIO,
  LoadLedgerError,
  CreateLedgerError,
) where

import DB.Ledger qualified as LedgerDB
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Ledger (LedgerEntry)

data LedgerStore :: Effect where
  LoadLedger :: UUID -> LedgerStore m (Either LoadLedgerError [LedgerEntry])
  CreateLedgerEntry :: LedgerEntry -> LedgerStore m (Either CreateLedgerError LedgerEntry)

type instance DispatchOf LedgerStore = Dynamic

loadLedger ::
  (HasCallStack, LedgerStore :> es) =>
  UUID ->
  Eff es (Either LoadLedgerError [LedgerEntry])
loadLedger = send . LoadLedger

createLedgerEntry ::
  (LedgerStore :> es) =>
  LedgerEntry ->
  Eff es (Either CreateLedgerError LedgerEntry)
createLedgerEntry = send . CreateLedgerEntry

newtype LoadLedgerError = LoadLedgerError String
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype CreateLedgerError = CreateLedgerError String
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

runLedgerStoreIO ::
  (IOE :> es) =>
  Connection ->
  Eff (LedgerStore : es) a ->
  Eff es a
runLedgerStoreIO conn = interpret $ \_ -> \case
  LoadLedger accountId ->
    mapSessionError LoadLedgerError
      <$> liftIO (LedgerDB.list conn accountId)
  CreateLedgerEntry ledgerEntry ->
    mapSessionError CreateLedgerError
      <$> liftIO (LedgerDB.create conn ledgerEntry)

mapSessionError :: (Show a) => (String -> c) -> Either a b -> Either c b
mapSessionError f = either (Left . f . show) Right