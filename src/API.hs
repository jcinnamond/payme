module API (run) where

import Account (Account)
import Control.Monad.Except (ExceptT (..))
import Data.Proxy (Proxy (..))
import Data.UUID (UUID)
import Effectful (Eff, IOE, runEff)
import Effectful qualified as Eff
import Effectful.Error.Static qualified as Static
import Effects.AccountStore (AccountStore)
import Effects.AccountStore qualified as AccountStore
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
     , Logger.Logger
     , IOE
     ]

type API = NamedRoutes AccountAPI

newtype AccountAPI mode = AccountAPI
  { get :: mode :- Capture "account id" UUID :> Get '[JSON] (Either AccountStore.GetAccountError (Maybe Account))
  }
  deriving stock (Generic)

server :: (Logger.Logger Eff.:> es, AccountStore Eff.:> es) => ServerT API (Eff es)
server =
  AccountAPI
    { get = handleGetAccount
    }

handleGetAccount ::
  (Logger.Logger Eff.:> es, AccountStore Eff.:> es) =>
  UUID ->
  Eff es (Either AccountStore.GetAccountError (Maybe Account))
handleGetAccount uuid = do
  Logger.info "getting account"
  AccountStore.getAccount uuid

nt :: Connection -> MyApp a -> Handler a
nt conn = do
  Handler
    . ExceptT
    . runEff
    . Logger.runLogger
    . AccountStore.runAccountStoreIO conn
    . Static.runErrorNoCallStack

proxy :: Proxy API
proxy = Proxy

app :: Connection -> Application
app connection = serve proxy $ hoistServer proxy (nt connection) server

run :: Connection -> IO ()
run connection = Warp.run 8080 (app connection)
