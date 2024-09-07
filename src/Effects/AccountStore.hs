{-# LANGUAGE LambdaCase #-}

module Effects.AccountStore (
  AccountStore (..),
  getAccount,
  runAccountStoreIO,
  GetAccountError,
) where

import Account (Account)
import DB.Accounts qualified as AccountDB
import Data.Aeson (ToJSON)
import Data.UUID (UUID)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, liftIO, (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import GHC.Generics (Generic)
import Hasql.Connection (Connection)

data AccountStore :: Effect where
  GetAccount :: UUID -> AccountStore m (Either GetAccountError (Maybe Account))

type instance DispatchOf AccountStore = Dynamic

getAccount ::
  (HasCallStack, AccountStore :> es) =>
  UUID ->
  Eff es (Either GetAccountError (Maybe Account))
getAccount = send . GetAccount

newtype GetAccountError = GetAccountError String
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

runAccountStoreIO ::
  (IOE :> es) =>
  Connection ->
  Eff (AccountStore : es) a ->
  Eff es a
runAccountStoreIO conn = interpret $ \_ -> \case
  GetAccount uuid ->
    mapSessionError GetAccountError
      <$> liftIO (AccountDB.get conn uuid)

mapSessionError :: (Show a) => (String -> c) -> Either a b -> Either c b
mapSessionError f = either (Left . f . show) Right