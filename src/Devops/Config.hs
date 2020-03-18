{-# LANGUAGE OverloadedStrings #-}

module Devops.Config
  ( createConfig
  , portConfig
  , createServerConfig
  , Config(..)
  , BasicAuthConfig(..)
  ) where

import           Control.Applicative
import           Data.Maybe          (fromMaybe)
import           Data.Text
import           System.Environment  (lookupEnv)
import           System.IO.Temp

data BasicAuthConfig = BasicAuthConfig
  { userName :: Text
  , password :: Text
  }

newtype ServerConfig = ServerConfig { port :: Int }

data Config = Config
  { basicAuthConf :: BasicAuthConfig
  , serverConf    :: ServerConfig
  , dbPath        :: FilePath
  }

portConfig :: Config -> Int
portConfig = port . serverConf

createServerConfig :: Int -> ServerConfig
createServerConfig = ServerConfig

createConfig :: IO Config
createConfig =
  Config
  <$> getBasicAuthConf
  <*> getServerConf
  <*> getDbConf

getServerConf  :: IO ServerConfig
getServerConf  = ServerConfig <$> envOrDefault "APP_API_PORT" "3000" read

getBasicAuthConf :: IO BasicAuthConfig
getBasicAuthConf = BasicAuthConfig
  <$> envOrDefault "APP_API_BASIC_USER_NAME" "user" pack
  <*> envOrDefault "APP_API_BASIC_PASSWORD" "pass" pack

envOrDefault :: String -> String -> (String -> b) -> IO b
envOrDefault key def fn = fromMaybe def <$> lookupEnv key <**> pure fn

getDbConf :: IO FilePath
getDbConf =
  lookupEnv "APP_DB_FILE" >>=
    maybe (emptySystemTempFile "counterparty.db") return
