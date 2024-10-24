{-# LANGUAGE LambdaCase #-}

module Effects.AccountStore (
  AccountStore (..),
  getAccount,
  listAccounts,
  runAccountStoreIO,
  Error,
) where

import Account (Account (..))
import AccountWithLedger (AccountWithLedger (..))
import DB.Accounts qualified as AccountDB
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import Effects.LedgerStore (LedgerStore)
import Effects.LedgerStore qualified as LedgerStore
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Hasql.Session qualified as HasqlSession

data AccountStore :: Effect where
  GetAccount :: UUID -> AccountStore m (Either Error (Maybe AccountWithLedger))
  ListAccounts :: AccountStore m (Either Error (Vector Account))

type instance DispatchOf AccountStore = Dynamic

getAccount ::
  (HasCallStack, AccountStore :> es, LedgerStore :> es) =>
  UUID ->
  Eff es (Either Error (Maybe AccountWithLedger))
getAccount = send . GetAccount

listAccounts ::
  (HasCallStack, AccountStore :> es) =>
  Eff es (Either Error (Vector Account))
listAccounts = send ListAccounts

newtype Error
  = SessionError String
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
      Left err
        | isNotFound err -> pure $ Right Nothing
        | otherwise -> pure $ Left $ SessionError $ show err
      Right acc -> addLedger acc
  ListAccounts -> do
    account <- liftIO (AccountDB.list conn)
    case account of
      Left err -> pure $ Left $ SessionError $ show err
      Right accs -> pure $ Right accs

isNotFound :: HasqlSession.SessionError -> Bool
isNotFound (HasqlSession.QueryError _ _ (HasqlSession.ResultError (HasqlSession.UnexpectedAmountOfRows 0))) = True
isNotFound _ = False

addLedger ::
  (IOE :> es, LedgerStore :> es) =>
  Account ->
  Eff es (Either Error (Maybe AccountWithLedger))
addLedger acc = do
  ledger <- LedgerStore.loadLedger acc.id
  pure $
    mapResult
      (SessionError . show)
      (Just . AccountWithLedger acc)
      ledger

mapResult :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapResult f _ (Left x) = Left $ f x
mapResult _ f (Right x) = Right $ f x
