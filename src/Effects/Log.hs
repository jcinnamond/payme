{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Log (
  Log (..),
  runLog,
  info,
) where

import Data.Aeson qualified as JSON
import Data.ByteString.Lazy.Char8 qualified as BIO
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Effectful (Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Logger (Logger, withField)

data Log :: Effect where
  Info :: Text -> Log m ()

makeEffect ''Log

runLog ::
  (IOE :> es) =>
  Logger ->
  Eff (Log : es) a ->
  Eff es a
runLog logger = interpret $ \_ -> \case
  Info s ->
    liftIO $ do
      currentTime <- getCurrentTime
      let l' = Logger.withField logger "level" ("info" :: Text)
          l'' = Logger.withField l' "message" s
          l''' = Logger.withField l'' "current_time" currentTime
      BIO.putStrLn $ JSON.encode l'''
