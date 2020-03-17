{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Devops.Lib.DataAccess.DB.Internal
  ( initDB
  , initDBWithData
  ) where

import           Control.Monad (when)
import           Devops.Lib.Data.Model
import           Devops.Lib.DataAccess.DB.MockData
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Text.RawString.QQ
import           System.Directory (doesFileExist)

instance FromRow Transaction where
  fromRow = Transaction
    <$> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field <*> field
    <*> pure "" <*> pure "" <*> pure "" <*> pure True <*> pure 0
    <*> pure "" <*> pure ""

instance FromField Asset where
  fromField (Field (SQLText "XCP") _) = Ok XCP
  fromField (Field (SQLText "BTC") _) = Ok BTC
  fromField f                         =
    returnError ConversionFailed f "expected XCP | BTC"

instance FromRow Balance where
  fromRow = Balance <$> field <*> field <*> field <*> pure True

initDB :: FilePath -> IO ()
initDB dbFile = do
    needsInit <- not <$> doesFileExist dbFile
    when needsInit $
        initDBWithData dbFile initData

initDBWithData :: FilePath -> (Connection -> IO ()) -> IO ()
initDBWithData dbFile initFn = withConnection dbFile $ \conn -> do
  mapM_ (execute_ conn) createTables
  initFn conn

createTables :: [Query]
createTables = [ createBlocks
               , createBlockIndex
               , createBlockHashIndex
               , createTransactions
               , createTxIndex
               , createTxHashIndex
               , createTxBlockIndex
               , createTxBlockTxIdxIndex
               , createTxHashBlockIndex
               , createDebits
               , createDebitAddIdx
               , createDebitAssetIdx
               , createCredits
               , createCreditsAddIdx
               , createCreditsAssetIdx
               , createBalances
               , createBalAddIdx
               , createBalAssetIdx
               , createBalAddAssetIdx
               , createAssets
               , createAssetsIdIdx
               , createAssetsNameIdx
               , createAssetsLongNameIdx
               , createAddresses
               , createAddressesIdx
               , createMessages
               , createMessagesIdx
               , createMessageBlockIdx ]

createBlocks :: Query
createBlocks = [r|
    CREATE TABLE IF NOT EXISTS blocks(
      block_index INTEGER UNIQUE,
      block_hash TEXT UNIQUE,
      block_time INTEGER,
      previous_block_hash TEXT UNIQUE,
      difficulty INTEGER,
      PRIMARY KEY (block_index, block_hash)) |]

createBlockIndex :: Query
createBlockIndex =
  [r| CREATE INDEX IF NOT EXISTS block_index_idx ON blocks (block_index) |]

createBlockHashIndex :: Query
createBlockHashIndex =
  [r| CREATE INDEX IF NOT EXISTS index_hash_idx ON blocks (block_index, block_hash)|]

createTransactions :: Query
createTransactions = [r|
    CREATE TABLE IF NOT EXISTS transactions(
       tx_index INTEGER UNIQUE,
       tx_hash TEXT UNIQUE,
       block_index INTEGER,
       block_hash TEXT,
       block_time INTEGER,
       source TEXT,
       destination TEXT,
       btc_amount INTEGER,
       fee INTEGER,
       data BLOB,
       supported BOOL DEFAULT 1,
       FOREIGN KEY (block_index, block_hash) REFERENCES blocks(block_index, block_hash),
       PRIMARY KEY (tx_index, tx_hash, block_index))|]

createTxBlockIndex :: Query
createTxBlockIndex =
  [r| CREATE INDEX IF NOT EXISTS block_index_idx ON transactions (block_index) |]

createTxIndex :: Query
createTxIndex =
  [r| CREATE INDEX IF NOT EXISTS tx_index_idx ON transactions (tx_index) |]

createTxHashIndex :: Query
createTxHashIndex =
  [r| CREATE INDEX IF NOT EXISTS tx_hash_idx ON transactions (tx_hash) |]

createTxBlockTxIdxIndex :: Query
createTxBlockTxIdxIndex =
  [r| CREATE INDEX IF NOT EXISTS
    index_index_idx ON transactions (block_index, tx_index) |]

createTxHashBlockIndex :: Query
createTxHashBlockIndex =
  [r| CREATE INDEX IF NOT EXISTS
    index_hash_index_idx ON transactions (tx_index, tx_hash, block_index) |]

createBalances :: Query
createBalances = [r|
    CREATE TABLE IF NOT EXISTS balances(
       address TEXT,
       asset TEXT,
       quantity INTEGER)
  |]

createBalAddAssetIdx :: Query
createBalAddAssetIdx =
  [r| CREATE INDEX IF NOT EXISTS address_asset_idx ON balances (address, asset) |]

createBalAddIdx :: Query
createBalAddIdx =
  [r| CREATE INDEX IF NOT EXISTS address_idx ON balances (address) |]

createBalAssetIdx :: Query
createBalAssetIdx =
  [r| CREATE INDEX IF NOT EXISTS asset_idx ON balances (asset) |]

createDebits :: Query
createDebits = [r|
    CREATE TABLE IF NOT EXISTS debits(
                      block_index INTEGER,
                      address TEXT,
                      asset TEXT,
                      quantity INTEGER,
                      action TEXT,
                      event TEXT,
                      FOREIGN KEY (block_index) REFERENCES blocks(block_index))|]

createDebitAddIdx :: Query
createDebitAddIdx =
  [r| CREATE INDEX IF NOT EXISTS address_idx ON debits (address)|]

createDebitAssetIdx :: Query
createDebitAssetIdx =
  [r| CREATE INDEX IF NOT EXISTS asset_idx ON debits (asset)|]

createCredits :: Query
createCredits = [r|
   CREATE TABLE IF NOT EXISTS credits(
                      block_index INTEGER,
                      address TEXT,
                      asset TEXT,
                      quantity INTEGER,
                      calling_function TEXT,
                      event TEXT,
                      FOREIGN KEY (block_index) REFERENCES blocks(block_index))|]

createCreditsAddIdx :: Query
createCreditsAddIdx =
  [r| CREATE INDEX IF NOT EXISTS address_idx ON credits (address) |]

createCreditsAssetIdx :: Query
createCreditsAssetIdx =
  [r| CREATE INDEX IF NOT EXISTS asset_idx ON credits (asset)|]

createAssets :: Query
createAssets = [r|
   CREATE TABLE IF NOT EXISTS assets(
                      asset_id TEXT UNIQUE,
                      asset_name TEXT UNIQUE,
                      block_index INTEGER,
                      asset_longname TEXT)|]

createAssetsNameIdx :: Query
createAssetsNameIdx =
  [r| CREATE INDEX IF NOT EXISTS name_idx ON assets (asset_name)|]

createAssetsIdIdx :: Query
createAssetsIdIdx = [r| CREATE INDEX IF NOT EXISTS id_idx ON assets (asset_id)|]

createAssetsLongNameIdx :: Query
createAssetsLongNameIdx =
  [r|CREATE UNIQUE INDEX IF NOT EXISTS
    asset_longname_idx ON assets(asset_longname)|]


createAddresses :: Query
createAddresses = [r|
    CREATE TABLE IF NOT EXISTS addresses(
                      address TEXT UNIQUE,
                      options INTEGER,
                      block_index INTEGER)|]

createAddressesIdx :: Query
createAddressesIdx =
  [r| CREATE INDEX IF NOT EXISTS addresses_idx ON addresses (address)|]

createMessages :: Query
createMessages = [r|
                   CREATE TABLE IF NOT EXISTS messages(
                      message_index INTEGER PRIMARY KEY,
                      block_index INTEGER,
                      command TEXT,
                      category TEXT,
                      bindings TEXT,
                      timestamp INTEGER)
                      |]

createMessagesIdx :: Query
createMessagesIdx =
  [r| CREATE INDEX IF NOT EXISTS block_index_idx ON messages (block_index)|]

createMessageBlockIdx :: Query
createMessageBlockIdx =
  [r| CREATE INDEX IF NOT EXISTS
    block_index_message_index_idx ON messages (block_index, message_index)|]
