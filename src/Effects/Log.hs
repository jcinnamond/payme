{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Log (
  Log (..),
  runLog,
  info,
  baseLogger,
  withFields,
  error,
) where

import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BIO
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Effectful (Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Logger (Logger, (.=))
import Logger qualified
import Prelude hiding (error)

data Log :: Effect where
  BaseLogger :: Log m Logger
  WithFields :: [Logger.LogField] -> Log m Logger
  Info :: Logger -> Text -> Log m ()
  Error :: Logger -> Text -> Log m ()

makeEffect ''Log

runLog ::
  (IOE :> es) =>
  Logger ->
  Eff (Log : es) a ->
  Eff es a
runLog logger = interpret $ \_ -> \case
  BaseLogger -> pure logger
  WithFields fields -> pure $ Logger.withFields logger fields
  Info l s -> logLevel "info" l s
  Error l s -> logLevel "error" l s

logLevel :: (IOE :> es) => Text -> Logger -> Text -> Eff es ()
logLevel level logger message = do
  liftIO $ do
    currentTime <- getCurrentTime
    let l =
          Logger.withFields
            logger
            [ "level" .= level
            , "current_time" .= currentTime
            , "message" .= message
            ]
    BIO.putStrLn $ JSON.encode l
