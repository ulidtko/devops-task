{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Devops.ApiDataFixturesTest
  (initData) where

import           Database.SQLite.Simple
import           Text.RawString.QQ

initData :: Connection -> IO ()
initData conn = mapM_ (execute_ conn) insertMockData

insertMockData :: [Query]
insertMockData = [
    [r|INSERT INTO assets VALUES ('0', 'BTC', 0, 'Bitcoin')|]
  , [r|INSERT INTO assets VALUES ('1', 'XCP', 0, 'Devops')|]

  , [r|INSERT INTO addresses VALUES ('ADDRESS_B', 1, 'ZZZZZZA')|]
  , [r|INSERT INTO addresses VALUES ('ADDRESS_C', 2, 'ZZZZZZB')|]
  , [r|INSERT INTO addresses VALUES ('ADDRESS_D', 3, 'ZZZZZZC')|]
  , [r|INSERT INTO addresses VALUES ('ADDRESS_E', 4, 'ZZZZZZD')|]
  , [r|INSERT INTO addresses VALUES ('ADDRESS_F', 5, 'ZZZZZZE')|]

  , [r|INSERT INTO balances VALUES ('ADDRESS_B', 'BTC', 1232131)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_B', 'XCP', 432434)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_C', 'BTC', 654654564)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_C', 'XCP', 7657567)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_D', 'BTC', 765765)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_D', 'XCP', 17765756)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_E', 'BTC', 234232)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_E', 'XCP', 7543774)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_F', 'BTC', 113451)|]
  , [r|INSERT INTO balances VALUES ('ADDRESS_F', 'XCP', 65465)|]

  , [r|INSERT INTO blocks VALUES (1, 'BLOCKHASH_B', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (2, 'BLOCKHASH_C', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (3, 'BLOCKHASH_D', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (4, 'BLOCKHASH_E', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (5, 'BLOCKHASH_F', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (6, 'BLOCKHASH_G', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (7, 'BLOCKHASH_H', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (8, 'BLOCKHASH_I', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (9, 'BLOCKHASH_J', 1000011, null, 1000)|]
  , [r|INSERT INTO blocks VALUES (10, 'BLOCKHASH_K', 1000011, null, 1000)|]

  , [r|INSERT INTO debits VALUES (1, 'ADDRESS_B', 'BTC', 100, 'bet', 'SOMEEVENT')|]
  , [r|INSERT INTO debits VALUES (2, 'ADDRESS_C', 'XCP', 100, 'burn', 'SOMEEVENT')|]
  , [r|INSERT INTO debits VALUES (4, 'ADDRESS_D', 'BTC', 100, 'send', 'SOMEEVENT')|]
  , [r|INSERT INTO debits VALUES (5, 'ADDRESS_E', 'XCP', 100, 'dividend', 'SOMEEVENT')|]
  , [r|INSERT INTO debits VALUES (6, 'ADDRESS_F', 'BTC', 100, 'bet', 'SOMEEVENT')|]
  , [r|INSERT INTO credits VALUES (1, 'ADDRESS_B', 'XCP', 100, 'bet', 'SOMEEVENT')|]
  , [r|INSERT INTO credits VALUES (3, 'ADDRESS_C', 'BTC', 100, 'burn', 'SOMEEVENT')|]
  , [r|INSERT INTO credits VALUES (2, 'ADDRESS_D', 'XCP', 100, 'bet', 'SOMEEVENT')|]
  , [r|INSERT INTO credits VALUES (5, 'ADDRESS_E', 'BTC', 100, 'dividend', 'SOMEEVENT')|]
  , [r|INSERT INTO credits VALUES (6, 'ADDRESS_F', 'XCP', 100, 'bet', 'SOMEEVENT')|]

  , [r|INSERT INTO transactions VALUES (1, "TX_HASH_A", 1, 'BLOCKHASH_B', 10000001, 'ADDRESS_B', 'ADDRESS_C', 123, 2, null, 1)|]
  , [r|INSERT INTO transactions VALUES (2, "TX_HASH_B", 2, 'BLOCKHASH_C', 1000011, 'ADDRESS_C', 'ADDRESS_D', 432432, 1, null, 1)|]
  , [r|INSERT INTO transactions VALUES (3, "TX_HASH_C", 3, 'BLOCKHASH_D', 1000011, 'ADDRESS_E', 'ADDRESS_F', 54354, 3, null, 1)|]
  , [r|INSERT INTO transactions VALUES (4, "TX_HASH_D", 4, 'BLOCKHASH_E', 11005231, 'ADDRESS_B', 'ADDRESS_E', 65465, 1, null, 1)|]
  , [r|INSERT INTO transactions VALUES (5, "TX_HASH_E", 5, 'BLOCKHASH_F', 1000011, 'ADDRESS_G', 'ADDRESS_D', 321, 24, null, 1)|]
  , [r|INSERT INTO transactions VALUES (6, "TX_HASH_F", 6, 'BLOCKHASH_G', 1000011, 'ADDRESS_D', 'ADDRESS_F', 121, 5, null, 1)|]
  , [r|INSERT INTO transactions VALUES (7, "TX_HASH_G", 7, 'BLOCKHASH_H', 10543234, 'ADDRESS_F', 'ADDRESS_B', 432, 42, null, 1)|]
  , [r|INSERT INTO transactions VALUES (8, "TX_HASH_H", 8, 'BLOCKHASH_I', 1000011, 'ADDRESS_E', 'ADDRESS_C', 4444, 2, null, 1)|]
  , [r|INSERT INTO transactions VALUES (9, "TX_HASH_I", 9, 'BLOCKHASH_J', 1000011, 'ADDRESS_C', 'ADDRESS_F', 554, 12, null, 1)|]
  , [r|INSERT INTO transactions VALUES (10, "TX_HASH_J", 10, 'BLOCKHASH_K', 1000011, 'ADDRESS_G', 'ADDRESS_E', 545, 5, null, 1)|]

  , [r|INSERT INTO messages VALUES (1, 1, 'command1', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (2, 2, 'command2', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (3, 3, 'command3', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (4, 4, 'command4', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (5, 5, 'command5', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (6, 6, 'command6', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (7, 7, 'command7', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (8, 8, 'command8', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (9, 9, 'command9', 'sends', 'bindings', 1000011)|]
  , [r|INSERT INTO messages VALUES (10, 10, 'command10', 'sends', 'bindings', 1000011)|]
    ]
