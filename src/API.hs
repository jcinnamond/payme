module API (run) where

import AccountWithLedger (AccountWithLedger (..))
import Control.Monad.Except (ExceptT (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Effectful (Eff, IOE, runEff)
import Effectful qualified as Eff
import Effectful.Error.Static qualified as Static
import Effects.AccountStore (AccountStore)
import Effects.AccountStore qualified as AccountStore
import Effects.LedgerStore (LedgerStore)
import Effects.LedgerStore qualified as LedgerStore
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import Logger qualified
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Capture, Handler (..), NamedRoutes, ServerError, ServerT, hoistServer, serve, (:-), (:>))
import Servant.API (Get, JSON)
import Servant.Server (Application)

type MyApp =
  Eff
    '[ Static.Error ServerError
     , AccountStore
     , LedgerStore
     , Logger.Logger
     , IOE
     ]

type API = NamedRoutes AccountAPI

newtype AccountAPI mode = AccountAPI
  { get ::
      mode
        :- Capture "account id" UUID
          :> Get '[JSON] (Either Text AccountWithLedger)
  }
  deriving stock (Generic)

server ::
  ( Logger.Logger Eff.:> es
  , AccountStore Eff.:> es
  , LedgerStore Eff.:> es
  ) =>
  ServerT API (Eff es)
server =
  AccountAPI
    { get = handleGetAccount
    }

handleGetAccount ::
  ( Logger.Logger Eff.:> es
  , AccountStore Eff.:> es
  , LedgerStore Eff.:> es
  ) =>
  UUID ->
  Eff es (Either Text AccountWithLedger)
handleGetAccount uuid = do
  Logger.info "getting account"
  mapLeft tshow <$> AccountStore.getAccount uuid

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

tshow :: (Show a) => a -> Text
tshow = T.pack . show

nt :: Connection -> MyApp a -> Handler a
nt conn = do
  Handler
    . ExceptT
    . runEff
    . Logger.runLogger
    . LedgerStore.runLedgerStoreIO conn
    . AccountStore.runAccountStoreIO conn
    . Static.runErrorNoCallStack

proxy :: Proxy API
proxy = Proxy

app :: Connection -> Application
app connection = serve proxy $ hoistServer proxy (nt connection) server

run :: Connection -> IO ()
run connection = Warp.run 8080 (app connection)
