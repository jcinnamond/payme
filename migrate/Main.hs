module Main where

import Config (load)
import DB.Connection qualified as DB
import Data.ByteString.Char8 qualified as BS
import Migrations qualified
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  configResult <- load
  let config = case configResult of
        Left err -> error $ "error loading config: " <> show err
        Right c -> c
  conResult <- DB.connection config
  let con = case conResult of
        Left (Just err) -> error $ "connecting to database: " <> BS.unpack err
        Left Nothing -> error "connecting to database: no further details provided"
        Right c -> c

  case args of
    ["init"] -> Migrations.initialize con
    ["migrate"] -> Migrations.migrate con
    ["seed"] -> Migrations.seed con
    _ -> do
      putStrLn "usage: migrations init | migrate | seed"
