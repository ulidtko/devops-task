{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Devops.Lib.DataAccess.DB
  ( initDB
  , getTransactionByTxRequest
  , getBalancesByReq ) where

import           Control.Lens
import           Devops.Lib.Data.Model
import           Devops.Lib.DataAccess.DB.Internal (initDB)
import           Data.List
import           Data.Text                               hiding (intercalate,
                                                          map)
import           Database.SQLite.Simple
import           Database.SQLite.SimpleErrors
import           Text.RawString.QQ

getBalancesByReq :: Addresses -> FilePath -> IO (DatabaseResponse [Balance])
getBalancesByReq addReq dbFile = runDBAction $
  withConnection dbFile $ \conn ->
    query_ conn
    $ Query
    $ pack
    $ "SELECT * FROM balances WHERE address in ("
    ++ intercalate "," (map show (addresses addReq)) ++ ")"

getTransactionByTxRequest
  :: RawTxRequest
  -> FilePath
  -> IO (DatabaseResponse [Transaction])
getTransactionByTxRequest txReq dbFile = runDBAction $
  withConnection dbFile $ \conn ->
  queryNamed
    conn
    [r|
      SELECT tx_index, tx_hash, block_index, block_time,
      source, destination, fee, quantity, asset_name,
      asset_longname, command, category FROM
      (SELECT *
        FROM transactions t
          INNER JOIN balances b ON b.address = t.destination
          INNER JOIN assets a ON a.asset_name = b.asset
          INNER JOIN messages m ON m.block_index = t.block_index
        WHERE t.destination = :address
        and (t.block_time between :from and :to)
      UNION
      SELECT *
        FROM transactions t
          INNER JOIN balances b ON b.address = t.source
          INNER JOIN assets a ON a.asset_name = b.asset
          INNER JOIN messages m ON m.block_index = t.block_index
        WHERE t.source = :address
        and (t.block_time between :from and :to))
        ORDER BY block_time desc
        LIMIT :lim |]
   [ ":address" := txReq^.address
    , ":lim" :=  txReq^.limit
    , ":from" := _startTs txReq
    , ":to" := _endTs txReq ]
  :: IO [Transaction]
