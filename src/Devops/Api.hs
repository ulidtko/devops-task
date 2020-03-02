{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Devops.Api
    ( AppCtx(..)
    , startApp
    , initApp
    ) where

import           Control.Monad.Reader
import           Devops.Api.Security
import           Devops.Config
import           Devops.Lib
import           Devops.Lib.DataAccess.DB
import           Data.Aeson
import           Data.Proxy
import qualified Data.Text                      as T
import           Network.Wai
import           Servant
import qualified Servant.JsonRpc                as J
import           System.Log.FastLogger          (ToLogStr (..), toLogStr)

type CounterAPI =
  "api" :> ReqBody '[JSON] (J.Request Value) :> Post '[JSON] (J.Response Value)

type ProtectedCounterAPI = BasicAuth "" AuthUser :> CounterAPI


methods :: Operations m => J.Methods m
methods = J.fromList listMethods

listMethods
  :: Operations m
  => [J.NamedMethod m]
listMethods = [ J.method "get_raw_transactions" getRawTransactions
              , J.method "get_normalized_balances" getNormalizedBalance ]

withLogging
  :: (MonadIO m, Operations m)
  => J.Request Value
  -> (J.Request Value -> m (J.Response Value))
  -> m (J.Response Value)
withLogging req handler = do
  logger <- asks _getLogger
  result <- handler req
  liftIO
    $ logger
    $ toLogStr @String "Called Operation: "
      <> toLogStr req
      <> toLogStr @String " - Result: "
      <> toLogStr result
      <> toLogStr @String "\n"
  return result

server
  :: (MonadIO m, Operations m)
  => AuthUser
  -> J.Request Value
  -> m (J.Response Value)
server _ req = withLogging req (J.handleRequest methods toMethodError)

toMethodError :: Show e => e -> J.MethodError
toMethodError e = J.MethodError 100 $ T.pack $ show e

counterApi :: Proxy ProtectedCounterAPI
counterApi = Proxy

initApp :: Config -> IO ()
initApp config = initDB $ dbPath config

secureContext :: Proxy '[ BasicAuthCheck AuthUser ]
secureContext = Proxy

startApp :: AppCtx -> Application
startApp ctx =
  serveWithContext counterApi (basicAuthServerContext $ _getConfig ctx) $
  hoistServerWithContext counterApi secureContext (`runReaderT` ctx) server



