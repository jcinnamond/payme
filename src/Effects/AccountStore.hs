{-# LANGUAGE LambdaCase #-}

module Effects.AccountStore (
  AccountStore (..),
  getAccount,
  runAccountStoreIO,
  GetAccountError,
) where

import Account (Account (..))
import AccountWithLedger (AccountWithLedger (..))
import DB.Accounts qualified as AccountDB
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import Effects.LedgerStore (LedgerStore)
import Effects.LedgerStore qualified as LedgerStore
import GHC.Generics (Generic)
import Hasql.Connection (Connection)

data AccountStore :: Effect where
  GetAccount :: UUID -> AccountStore m (Either GetAccountError AccountWithLedger)

type instance DispatchOf AccountStore = Dynamic

getAccount ::
  (HasCallStack, AccountStore :> es, LedgerStore :> es) =>
  UUID ->
  Eff es (Either GetAccountError AccountWithLedger)
getAccount = send . GetAccount

newtype GetAccountError = GetAccountError String
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

runAccountStoreIO ::
  (IOE :> es, LedgerStore :> es) =>
  Connection ->
  Eff (AccountStore : es) a ->
  Eff es a
runAccountStoreIO conn = interpret $ \_ -> \case
  GetAccount uuid -> do
    account <- liftIO (AccountDB.get conn uuid)
    case account of
      Left err -> pure $ Left $ GetAccountError $ show err
      Right acc -> addLedger acc

addLedger ::
  (IOE :> es, LedgerStore :> es) =>
  Account ->
  Eff es (Either GetAccountError AccountWithLedger)
addLedger acc = do
  ledger <- LedgerStore.loadLedger acc.id
  pure $
    mapResult
      (GetAccountError . show)
      (AccountWithLedger acc)
      ledger

mapResult :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapResult f _ (Left x) = Left $ f x
mapResult _ f (Right x) = Right $ f x
