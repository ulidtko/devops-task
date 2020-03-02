{-# LANGUAGE TypeApplications #-}
module Main where

import           Devops.Api
import           Devops.Config
import           Network.Wai.Handler.Warp
import           System.Log.FastLogger    (LogType (..), defaultBufSize,
                                           newFastLogger, toLogStr)

main :: IO ()
main = do
  config <- createConfig
  logger <- fst <$> newFastLogger (LogStdout defaultBufSize)
  let ctx = AppCtx { _getLogger = logger, _getConfig = config }
  initApp config
  let port = portConfig config
      settings =
        setPort port
        $ setBeforeMainLoop
          (logger $
           toLogStr @String ("Starting Devops API .... \n \
                     \Listening on port " <> show port <> "...\n")) defaultSettings
  runSettings settings (startApp ctx)
