{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}

module Devops.Lib
  ( AppCtx(..)
  , Operations(..)
  , AppError(..)
  ) where

import           Control.Concurrent.MVar        (newMVar, withMVar)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Devops.Config
import           Devops.Lib.Data.Model
import           Devops.Lib.DataAccess.DB
import           Data.Bifunctor                 (first)
import           Data.Either                    (either)
import           Data.Time
import           Database.SQLite.SimpleErrors
import           Servant
import           System.Log.FastLogger          (FastLogger, ToLogStr, toLogStr)

data AppCtx = AppCtx { _getConfig :: Config
                     , _getLogger :: FastLogger }

type AppM = ReaderT AppCtx Handler

data AppError = TransactionError | BalanceError
  deriving (Show, Eq)

type OperationResponse a = Either AppError a

class MonadReader AppCtx m => Operations m where
  getRawTransactions :: RawTxRequest -> m (OperationResponse [Transaction])
  getNormalizedBalance :: Addresses -> m (OperationResponse [Balance])

instance ToLogStr (DatabaseResponse r) where
  toLogStr result =
    toLogStr
    $ either (\e -> "Error: " <> show e) (const "Operation Success") result

instance Operations AppM where
  getRawTransactions txReq = do
    logger <- asks _getLogger
    dbFile <- asks (dbPath . _getConfig)
    newTxReq <- updateTxReq txReq <$> liftIO getCurrentTimeInSec
    dbTx   <- liftIO $ newMVar ()
    txs    <- liftIO $ withMVar dbTx $ \() ->
      getTransactionByTxRequest newTxReq dbFile
    liftIO
      $ logger
      $ toLogStr @String "Get Transactions from DB Result: "
        <> toLogStr txs
        <> toLogStr @String "\n"
    return $ first (const TransactionError) txs

  getNormalizedBalance balReq = do
    logger <- asks _getLogger
    dbFile <- asks (dbPath . _getConfig)
    dbTx   <- liftIO $ newMVar ()
    txs    <- liftIO $ withMVar dbTx $ \() -> getBalancesByReq balReq dbFile
    liftIO
      $ logger
      $ toLogStr @String "Get Balances from DB Result: "
        <> toLogStr txs
        <> toLogStr @String "\n"
    return $ first (const TransactionError) txs


getCurrentTimeInSec :: IO Integer
getCurrentTimeInSec =
  read @Integer . formatTime defaultTimeLocale "%s" <$> getCurrentTime
