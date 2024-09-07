module DB.Connection (connection) where

import Config (Config (..))
import Hasql.Connection (Connection, ConnectionError)
import Hasql.Connection qualified as Connection

connection :: Config -> IO (Either ConnectionError Connection)
connection config = do
  Connection.acquire config.dbConnection
