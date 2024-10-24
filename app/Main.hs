{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import API qualified
import Config (load)
import DB.Connection qualified as DB

main :: IO ()
main = do
  configResult <- load
  let config = case configResult of
        Left err -> error $ "error loading config: " <> show err
        Right c -> c

  hasqlConnection <- DB.connection config
  case hasqlConnection of
    Left err -> print err
    Right conn -> do
      API.run conn

-- TIO.putStrLn $ (T.pack . show) config

-- res <- runEff . runError @Store.LoadLedgerError . Store.runLedgerStoreIO config $ Store.load
-- case res of
--   Left err -> error $ show err
--   Right ledgerEntries -> do
--     forM_ ledgerEntries print
--     putStrLn ""
--     putStrLn $ "total is: " <> show (sumLedger ledgerEntries)
