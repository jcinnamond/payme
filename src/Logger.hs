{-# LANGUAGE LambdaCase #-}

module Logger (
  Logger (..),
  runLogger,
  logStr,
  info,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)

data Logger :: Effect where
  Info :: Text -> Logger m ()
  LogStr :: Text -> Logger m ()

type instance DispatchOf Logger = Dynamic

logStr :: (HasCallStack, Logger :> es) => Text -> Eff es ()
logStr s = send $ LogStr s

info :: (Logger :> es) => Text -> Eff es ()
info = send . Info

runLogger ::
  (IOE :> es) =>
  Eff (Logger : es) a ->
  Eff es a
runLogger = interpret $ \_ -> \case
  LogStr s -> liftIO $ TIO.putStrLn $ "log: " <> s
  Info s ->
    liftIO $
      TIO.putStrLn $
        mkLog
          [ ("level", "info")
          , ("message", s)
          ]

mkLog :: [(Text, Text)] -> Text
mkLog elems = "{" <> Text.intercalate "," (map f elems) <> "}"
 where
  f :: (Text, Text) -> Text
  f (k, v) = quote k <> "=" <> quote v

  quote :: Text -> Text
  quote x = "\"" <> x <> "\""