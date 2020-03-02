{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NumericUnderscores #-}

module Devops.ApiFixtures where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as LB
import           Data.ByteString.Lazy.Char8 as L
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Types         as NT
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

methodNotFoundReq :: ByteString
methodNotFoundReq = [json|
                         { id: 0,
                           jsonrpc: "2.0",
                           method: "method_not_found",
                           params: { address: "ADDRESS_B" }
                         }
                    |]

badReq :: ByteString
badReq = [json|
             { id: 0,
               method: "get_raw_transactions",
               params: { address: "ADDRESS_X" }
             }
        |]

txBetweenDates :: ByteString
txBetweenDates = [json|
                      { jsonrpc:"2.0",
                      id: 0,
                      method: "get_raw_transactions",
                      params: { address: "ADDRESS_B",
                      start_ts: 10000000,
                      end_ts: 12000000 }
                      }
                 |]

txWithLimit :: ByteString
txWithLimit = [json|
                      { jsonrpc:"2.0",
                      id: 0,
                      method: "get_raw_transactions",
                      params: { address: "ADDRESS_B",
                      limit: 2,
                      end_ts: 12000000 }
                      }
                 |]

balReq :: ByteString
balReq = [json|
             { jsonrpc:"2.0",
               id: 0,
               method: "get_normalized_balances",
               params: { addresses: ["ADDRESS_B"] }
             }
            |]

txReq :: ByteString
txReq = [json|
             { jsonrpc:"2.0",
               id: 0,
               method: "get_raw_transactions",
               params: { address: "ADDRESS_E", end_ts: 1000020 }
             }
            |]

txReqNotFound :: ByteString
txReqNotFound = [json|
             { jsonrpc:"2.0",
               id: 0,
               method: "get_raw_transactions",
               params: { address: "NOT_FOUND" }
             }
            |]

balReqNotFound :: ByteString
balReqNotFound = [json|
             { jsonrpc:"2.0",
               id: 0,
               method: "get_normalized_balances",
               params: { addresses: ["NOT_FOUND"] }
             }
            |]


expectMethodNotFound :: ResponseMatcher
expectMethodNotFound = [json|
                            { error:
                              { message: "Method not found",
                                code: -32601
                              },
                              jsonrpc:"2.0",
                              id:0
                            }
                       |]


expectBalNotFound :: ResponseMatcher
expectBalNotFound = expectTxNotFound

expectTxNotFound :: ResponseMatcher
expectTxNotFound = [json|
                            { result: [],
                             jsonrpc:"2.0",
                              id:0
                            }
                       |]

expectBal :: ResponseMatcher
expectBal = [json|
{
  "result": [
    {
      "asset": "BTC",
      "quantity": 1232131,
      "address": "ADDRESS_B",
      "owner": true,
      "normalized_quantity": 1232131
    },
    {
      "asset": "XCP",
      "quantity": 432434,
      "address": "ADDRESS_B",
      "owner": true,
      "normalized_quantity": 432434
    }
  ],
  "jsonrpc": "2.0",
  "id": 0
}
|]

expectTxWithLimit :: ResponseMatcher
expectTxWithLimit = [json|
{
  "result": [
    {
      "destination": "ADDRESS_E",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 4,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command4",
      "quantity": 1232131,
      "_asset_divisible": true,
      "_block_time": 11005231,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 4,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_B",
      "pb_btc_fee": 1,
      "tx_hash": "TX_HASH_D"
    },
    {
      "destination": "ADDRESS_E",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 4,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command4",
      "quantity": 432434,
      "_asset_divisible": true,
      "_block_time": 11005231,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 1,
      "_tx_index": 4,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_B",
      "tx_hash": "TX_HASH_D"
    }
  ],
  "jsonrpc": "2.0",
  "id": 0
}
|]

expectTxBetweenDates :: ResponseMatcher
expectTxBetweenDates = [json|
{
  "result": [
    {
      "destination": "ADDRESS_E",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 4,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command4",
      "quantity": 1232131,
      "_asset_divisible": true,
      "_block_time": 11005231,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 4,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_B",
      "pb_btc_fee": 1,
      "tx_hash": "TX_HASH_D"
    },
    {
      "destination": "ADDRESS_E",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 4,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command4",
      "quantity": 432434,
      "_asset_divisible": true,
      "_block_time": 11005231,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 1,
      "_tx_index": 4,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_B",
      "tx_hash": "TX_HASH_D"
    },
    {
      "destination": "ADDRESS_B",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 7,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command7",
      "quantity": 1232131,
      "_asset_divisible": true,
      "_block_time": 10543234,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 7,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_F",
      "pb_btc_fee": 42,
      "tx_hash": "TX_HASH_G"
    },
    {
      "destination": "ADDRESS_B",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 7,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command7",
      "quantity": 432434,
      "_asset_divisible": true,
      "_block_time": 10543234,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 42,
      "_tx_index": 7,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_F",
      "tx_hash": "TX_HASH_G"
    },
    {
      "destination": "ADDRESS_C",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 1,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command1",
      "quantity": 1232131,
      "_asset_divisible": true,
      "_block_time": 10000001,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 1,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_B",
      "pb_btc_fee": 2,
      "tx_hash": "TX_HASH_A"
    },
    {
      "destination": "ADDRESS_C",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 1,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command1",
      "quantity": 432434,
      "_asset_divisible": true,
      "_block_time": 10000001,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 2,
      "_tx_index": 1,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_B",
      "tx_hash": "TX_HASH_A"
    }
  ],
  "jsonrpc": "2.0",
  "id": 0
}
|]

expectTx :: ResponseMatcher
expectTx = [json|
{
  "result": [
    {
      "destination": "ADDRESS_F",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 3,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command3",
      "quantity": 234232,
      "_asset_divisible": true,
      "_block_time": 1000011,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 3,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_E",
      "pb_btc_fee": 3,
      "tx_hash": "TX_HASH_C"
    },
    {
      "destination": "ADDRESS_F",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 3,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command3",
      "quantity": 7543774,
      "_asset_divisible": true,
      "_block_time": 1000011,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 3,
      "_tx_index": 3,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_E",
      "tx_hash": "TX_HASH_C"
    },
    {
      "destination": "ADDRESS_C",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 8,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command8",
      "quantity": 234232,
      "_asset_divisible": true,
      "_block_time": 1000011,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 8,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_E",
      "pb_btc_fee": 2,
      "tx_hash": "TX_HASH_H"
    },
    {
      "destination": "ADDRESS_C",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 8,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command8",
      "quantity": 7543774,
      "_asset_divisible": true,
      "_block_time": 1000011,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 2,
      "_tx_index": 8,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_E",
      "tx_hash": "TX_HASH_H"
    },
    {
      "destination": "ADDRESS_E",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 10,
      "memo_hex": "",
      "asset": "btc",
      "_command": "command10",
      "quantity": 234232,
      "_asset_divisible": true,
      "_block_time": 1000011,
      "pb_deadline": 0,
      "_category": "sends",
      "_tx_index": 10,
      "_asset_longname": "Bitcoin",
      "chain": "",
      "source": "ADDRESS_G",
      "pb_btc_fee": 5,
      "tx_hash": "TX_HASH_J"
    },
    {
      "destination": "ADDRESS_E",
      "status": "",
      "memo": "",
      "pb_broadcaster": "",
      "block_index": 10,
      "memo_hex": "",
      "asset": "xcp",
      "_command": "command10",
      "quantity": 7543774,
      "_asset_divisible": true,
      "_block_time": 1000011,
      "pb_deadline": 0,
      "_category": "sends",
      "pb_xcp_fee": 5,
      "_tx_index": 10,
      "_asset_longname": "Devops",
      "chain": "",
      "source": "ADDRESS_G",
      "tx_hash": "TX_HASH_J"
    }
  ],
  "jsonrpc": "2.0",
  "id": 0
}
|]

encBody :: ByteString -> ByteString
encBody = LB.fromStrict . L.toStrict

headersAuth :: [NT.Header]
headersAuth = hContentTypeJson ++ hBasicAuth defaultUser defaultPass

headersAuthWrong :: [NT.Header]
headersAuthWrong = hContentTypeJson ++ hBasicAuth "wrongUser" "wrongPass"

hContentTypeJson :: [NT.Header]
hContentTypeJson = [(hContentType, "application/json")]

hBasicAuth :: T.Text -> T.Text -> [NT.Header]
hBasicAuth user pass = [(hAuthorization, hBasicEnc user pass)]

hBasicEnc :: T.Text -> T.Text -> B.ByteString
hBasicEnc user pass = "Basic " <> B64.encode (T.encodeUtf8 $ user <> ":" <> pass)

defaultUser :: T.Text
defaultUser = "testUser"
defaultPass :: T.Text
defaultPass = "testPass"
