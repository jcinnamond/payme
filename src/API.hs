module API (run) where

import Account (Account)
import AccountWithLedger (AccountWithLedger (..))
import Control.Monad.Except (ExceptT (..))
import Data.ByteString.Lazy.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.Vector (Vector)
import Effectful (Eff, IOE, runEff)
import Effectful qualified as Eff
import Effectful.Error.Static qualified as Static
import Effects.AccountStore (AccountStore)
import Effects.AccountStore qualified as AccountStore
import Effects.LedgerStore (LedgerStore)
import Effects.LedgerStore qualified as LedgerStore
import Effects.Log (Log)
import Effects.Log qualified as Log
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Logger (Logger, (.=))
import Network.Wai.Handler.Warp qualified as Warp
import Servant (
  Capture,
  Handler (..),
  NamedRoutes,
  Post,
  ServerError (..),
  ServerT,
  err404,
  err500,
  hoistServer,
  serve,
  (:-),
  (:>),
 )
import Servant.API (Get, JSON)
import Servant.Server (Application)

type MyApp =
  Eff
    '[ Static.Error ServerError
     , AccountStore
     , LedgerStore
     , Log
     , IOE
     ]

type API = NamedRoutes AccountAPI

data AccountAPI mode = AccountAPI
  { list :: mode :- Get '[JSON] (Vector Account)
  , get :: mode :- Capture "account id" UUID :> Get '[JSON] AccountWithLedger
  , deposit :: mode :- Capture "account id" UUID :> "deposit" :> Capture "amount" Int64 :> Post '[JSON] AccountWithLedger
  , withdraw :: mode :- Capture "account id" UUID :> "withdraw" :> Capture "amount" Int64 :> Post '[JSON] AccountWithLedger
  }
  deriving stock (Generic)

server ::
  ( Log Eff.:> es
  , AccountStore Eff.:> es
  , LedgerStore Eff.:> es
  , Static.Error ServerError Eff.:> es
  ) =>
  ServerT API (Eff es)
server =
  AccountAPI
    { list = handleListAccounts
    , get = handleGetAccount
    , deposit = handleAccountDeposit
    , withdraw = \accountId amount -> handleAccountDeposit accountId (-amount)
    }

handleAccountDeposit ::
  ( Log Eff.:> es
  , AccountStore Eff.:> es
  , Static.Error ServerError Eff.:> es
  ) =>
  UUID ->
  Int64 ->
  Eff es AccountWithLedger
handleAccountDeposit uuid amount = do
  logger <- Log.withFields ["payment_id" .= uuid]
  Log.info logger "depositing money"

  res <- AccountStore.deposit uuid amount
  case res of
    Left err -> Static.throwError $ err500{errBody = BSC.pack $ show err}
    Right () -> do
      acc <- AccountStore.getAccount uuid
      case acc of
        Left err -> do
          Log.error logger $ T.pack $ show err
          Static.throwError $ err500{errBody = BSC.pack $ show err}
        Right (Just x) -> pure x
        Right Nothing -> Static.throwError err404

handleListAccounts ::
  ( Log Eff.:> es
  , AccountStore Eff.:> es
  , Static.Error ServerError Eff.:> es
  ) =>
  Eff es (Vector Account)
handleListAccounts = do
  logger <- Log.baseLogger
  Log.info logger "listing accounts"

  accs <- AccountStore.listAccounts
  case accs of
    Left err -> Static.throwError $ err500{errBody = BSC.pack $ show err}
    Right x -> pure x

handleGetAccount ::
  ( Log Eff.:> es
  , AccountStore Eff.:> es
  , LedgerStore Eff.:> es
  , Static.Error ServerError Eff.:> es
  ) =>
  UUID ->
  Eff es AccountWithLedger
handleGetAccount uuid = do
  logger <- Log.withFields ["payment_id" .= uuid]
  Log.info logger "getting account"

  acc <- AccountStore.getAccount uuid
  case acc of
    Left err -> Static.throwError $ err500{errBody = BSC.pack $ show err}
    Right (Just x) -> pure x
    Right Nothing -> Static.throwError err404

nt :: Connection -> Logger -> MyApp a -> Handler a
nt conn logger = do
  Handler
    . ExceptT
    . runEff
    . Log.runLog logger
    . LedgerStore.runLedgerStoreIO conn
    . AccountStore.runAccountStoreIO conn
    . Static.runErrorNoCallStack

proxy :: Proxy API
proxy = Proxy

app :: Connection -> Logger -> Application
app connection logger = serve proxy $ hoistServer proxy (nt connection logger) server

run :: Connection -> Logger -> IO ()
run connection logger = Warp.run 8080 (app connection logger)
