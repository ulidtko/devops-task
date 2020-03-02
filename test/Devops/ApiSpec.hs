{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Devops.ApiSpec
  (spec) where

import           Devops.Api
import           Devops.ApiDataFixturesTest        (initData)
import           Devops.ApiFixtures
import           Devops.Config
import           Devops.Lib.DataAccess.DB.Internal (initDBWithData)
import qualified Data.Text                               as T
import           Network.HTTP.Types                      as NT
import           Servant
import           System.IO.Temp
import           System.Log.FastLogger                   (LogType (..),
                                                          newFastLogger)
import           Test.Hspec
import           Test.Hspec.Wai

spec :: Spec
spec = counterpartyApiSpec

counterpartyApiSpec :: Spec
counterpartyApiSpec= with app $ do
  describe "HTTP Errors calling /api" $ do
    it "should return code -32601 in json response if jsonrpc method not found" $
      request methodPost "/api/" headersAuth (encBody methodNotFoundReq)
        `shouldRespondWith`
        expectMethodNotFound { matchStatus = 200 }
    it "should return 400 if jsonrpc request is bad formed" $
      request methodPost "/api/" headersAuth (encBody badReq)
         `shouldRespondWith` 400
    it "should return 400 if body is wrong" $
      request methodPost "/api/" headersAuth (encBody "")
         `shouldRespondWith` 400
    it "should return 415 if body content type wrong" $
      request methodPost "/api/" (hBasicAuth defaultUser defaultPass) (encBody txReq)
         `shouldRespondWith` 415
    it "should return 401 if no Auth header is provide" $
      request methodPost "/api/" hContentTypeJson (encBody txReq)
         `shouldRespondWith` 401
    it "should return 403 if Auth failed" $
      request methodPost "/api/" headersAuthWrong (encBody txReq)
         `shouldRespondWith` 403
    it "should return 404 if wrong path" $ do
      post "/wrong/path" "" `shouldRespondWith` 404
      post "/other/" "" `shouldRespondWith` 404
    it "should return 405 if wrong method" $ do
      get "/api/" `shouldRespondWith` 405
      options "/api/" `shouldRespondWith` 405
      delete "/api/" `shouldRespondWith` 405

  describe "POST /api get_raw_transactions" $ do
    it "should return raw transactions when get_raw_transactions rpc method called properly" $
      request methodPost "/api/" headersAuth (encBody txReq)
        `shouldRespondWith`
        expectTx { matchStatus = 200 }
    it "should return empty raw transactions when get_raw_transactions no transactions found" $
      request methodPost "/api/" headersAuth (encBody txReqNotFound)
        `shouldRespondWith`
        expectTxNotFound { matchStatus = 200 }
    it "should return raw transactions between requested dates and in order" $
      request methodPost "/api/" headersAuth (encBody txBetweenDates)
        `shouldRespondWith`
        expectTxBetweenDates { matchStatus = 200 }
    it "should return raw transactions based on limit" $
      request methodPost "/api/" headersAuth (encBody txWithLimit)
        `shouldRespondWith`
        expectTxWithLimit { matchStatus = 200 }

  describe "POST /api get_normalized_balance" $ do
    it "should return balance when rpc method called properly" $
      request methodPost "/api/" headersAuth (encBody balReq)
        `shouldRespondWith`
        expectBal { matchStatus = 200 }
    it "should return empty no balance found" $
      request methodPost "/api/" headersAuth (encBody balReqNotFound)
        `shouldRespondWith`
        expectBalNotFound { matchStatus = 200 }

app :: IO Application
app = startAppWith defaultUser defaultPass "test.db"

startAppWith :: T.Text -> T.Text -> String -> IO Application
startAppWith user pass dbName = do
  dbPathName <- emptySystemTempFile dbName
  let conf = withConfig user pass dbPathName
  logger <- fst <$> newFastLogger LogNone
  initDBWithData dbPathName initData
  return $ startApp $ AppCtx { _getConfig = conf, _getLogger = logger }

withConfig :: T.Text -> T.Text -> String -> Config
withConfig user pass dbName =
  Config { basicAuthConf = BasicAuthConfig user pass
         , dbPath = dbName
         , serverConf = createServerConfig 3000 }
