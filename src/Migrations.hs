module Migrations (migrate, seed, initialize)
where

import Control.Exception (Exception (..))
import Data.Either (lefts)
import Hasql.Connection (Connection)
import Hasql.Migration (MigrationCommand (..), MigrationError, loadMigrationsFromDirectory, runMigration)
import Hasql.Session (SessionError, run)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as Tx

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