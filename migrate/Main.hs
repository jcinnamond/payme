module Main where

import Config (loadConfig)
import Control.Exception (Exception (..))
import DB.Connection qualified as DB
import Data.ByteString.Char8 qualified as BS
import Data.Either (lefts)
import Hasql.Connection (Connection)
import Hasql.Migration (MigrationCommand (MigrationInitialization), MigrationError, loadMigrationsFromDirectory, runMigration)
import Hasql.Session (SessionError, run)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  configResult <- loadConfig
  let config = case configResult of
        Left err -> error $ "error loading config: " <> show err
        Right c -> c
  conResult <- DB.connection config
  let con = case conResult of
        Left (Just err) -> error $ "connecting to database: " <> BS.unpack err
        Left Nothing -> error "connecting to database: no further details provided"
        Right c -> c

  case args of
    ["init"] -> initialize con
    ["migrate"] -> migrate con
    ["seed"] -> seed con
    _ -> do
      putStrLn "usage: migrations init | migrate | seed"

runTx :: Connection -> Tx.Transaction a -> IO (Either SessionError a)
runTx con act = do
  run (Tx.transaction Tx.ReadCommitted Tx.Write act) con

initialize :: Connection -> IO ()
initialize con = do
  res <- runTx con $ runMigration MigrationInitialization
  case res of
    Left err -> putStrLn $ "failed to initialize migrations: " <> show err
    Right (Just err) -> putStrLn $ "failed to initialize migrations: " <> show err
    Right Nothing -> putStrLn "migrations initialized"

migrate :: Connection -> IO ()
migrate con = do
  res <- migrateFiles con "./migrations"
  case lefts res of
    [] -> putStrLn "all migrations run"
    errs -> putStrLn $ "failed to run migrations:\n" <> unlines (displayException <$> errs)

seed :: Connection -> IO ()
seed con = do
  res <- migrateFiles con "./seed"
  print res
  case lefts res of
    [] -> putStrLn "all seed data created"
    errs -> putStrLn $ "failed to create seed data:\n" <> unlines (displayException <$> errs)

migrateFiles :: Connection -> FilePath -> IO [Either SessionError (Maybe MigrationError)]
migrateFiles con path = do
  migrationFiles <- loadMigrationsFromDirectory path
  let migrations = runMigration <$> migrationFiles
  mapM (runTx con) migrations