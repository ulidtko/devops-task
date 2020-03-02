{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Devops.Lib.Data.Model
  ( Addresses(..)
  , RawTxRequest(..)
  , Balance(..)
  , address
  , limit
  , endTs
  , startTs
  , Transaction(..)
  , Asset(..)
  , updateTxReq
  ) where

import           Control.Lens  hiding ((.=))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Text     hiding (map, toLower)

newtype Addresses = Addresses { addresses :: [String] } deriving (Eq, Show)

data RawTxRequest = RawTxRequest { _address :: String
                                 , _startTs :: Maybe Integer
                                 , _endTs   :: Maybe Integer
                                 , _limit   :: Integer } deriving (Ord, Eq, Show)

makeLenses ''RawTxRequest

instance FromJSON RawTxRequest where
  parseJSON (Object v) = do
    _address <- v .: "address"
    _endTs <- v .:? "end_ts"
    _startTs <- v .:? "start_ts"
    _limit <- v .:? "limit" .!= 10000
    return RawTxRequest{..}
  parseJSON _ = mempty

updateTxReq :: RawTxRequest -> Integer -> RawTxRequest
updateTxReq txReq currentTime =
  let overEnd = over endTs (updateEnd currentTime) txReq
      overStart = over startTs (updateStart $ overEnd^.endTs) overEnd
      in overStart

sixtyDayInSeconds :: Integer
sixtyDayInSeconds = 60*24*60*60

updateStart :: Maybe Integer -> Maybe Integer -> Maybe Integer
updateStart endTsTime Nothing = flip (-) sixtyDayInSeconds <$> endTsTime
updateStart _ previous        = previous

updateEnd :: Integer -> Maybe Integer -> Maybe Integer
updateEnd time Nothing = Just time
updateEnd _ previous   = previous

data Asset = XCP | BTC
  deriving (Eq, Show)

$(deriveJSON
  defaultOptions { constructorTagModifier = map toLower }
  ''Asset
 )

data Transaction = Transaction
  { txIndex       :: Integer
  , hash          :: Text
  , blockIndex    :: Integer
  , blockTime     :: Integer
  , source        :: Text
  , destination   :: Text
  , fee           :: Integer
  , quantity      :: Integer
  , asset         :: Asset
  , assetLongName :: Text
  , command       :: Text
  , category      :: Text
  , broadcaster   :: Text -- empty for the moment. Not found where is being taken
  , chain         :: Text -- empty for the moment. Need to find out how to calculate it
  , status        :: Text -- cannot find status. In fact there is a strange comment here https://github.com/DevopsXCP/counterparty-lib/blob/e875c45c586cc4fc840a59540a6d44bd03ee698b/counterpartylib/lib/blocks.py#L438
  , divisible     :: Bool -- always true. Not found where is being taken
  , deadline      :: Integer -- always 0. Not found where is being taken
  , memoHex       :: Text -- always null. Not found where is being taken
  , memo          :: Text -- always null. Not found where is being taken
  } deriving (Eq, Show)

instance ToJSON Transaction where
  toJSON tx =
    let pbFee = pack $ "pb_" <> map toLower (show $ asset tx) <> "_fee"
        in object [ "tx_hash" .= hash tx
           , "destination" .= destination tx
           , "source" .= source tx
           , "block_index" .= blockIndex tx
           , "pb_deadline" .= deadline tx
           , "_asset_longname" .= assetLongName tx
           , "_asset_divisible" .= divisible tx
           , "_category" .= category tx
           , "quantity" .= quantity tx
           , pbFee .= fee tx
           , "_tx_index" .=  txIndex tx
           , "asset" .=  asset tx
           , "status" .=  status tx
           , "chain" .=  chain tx
           , "pb_broadcaster" .=  broadcaster tx
           , "_command" .=  command tx
           , "_block_time" .=  blockTime tx
           , "memo_hex" .= memoHex tx
           , "memo" .= memo tx ]


$(deriveJSON defaultOptions ''Addresses)

data Balance = Balance { addressBal  :: Text
                       , assetBal    :: Text
                       , quantityBal :: Integer
                       , owner       :: Bool -- Default true. Havent found how to get this
                       } deriving (Eq, Show)

instance ToJSON Balance where
  toJSON bal = object [ "address" .= addressBal bal
                      , "quantity" .= quantityBal bal
                      , "owner" .= owner bal
                      , "asset" .= assetBal bal
                      , "normalized_quantity" .= quantityBal bal ]


