{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Servant.JsonRpc
  ( Request(..)
  , Response(..)
  , Methods(..)
  , MethodError(..)
  , Method(..)
  , NamedMethod
  , method
  , fromList
  , handleRequest
  )
where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson             hiding (Error)
import           Data.Aeson.Types       (emptyArray, parseMaybe)
import           Data.Bifunctor         (first)
import qualified Data.Map               as S
import           Data.Monoid            as M (mempty)
import           Data.Text              (Text, unpack)
import           GHC.Generics
import           Prelude                hiding (lookup)
import           System.Log.FastLogger (ToLogStr (..))

data Request a = Request { reqMethod :: Text
                         , reqParams :: a
                         , reqId     :: Value }
                deriving (Eq, Show, Generic)

instance ToLogStr (Request Value) where
  toLogStr r = toLogStr $ "method_requested: " ++ (unpack . reqMethod) r

instance FromJSON (Request Value) where
  parseJSON (Object v) = do
    version <- v .: "jsonrpc"
    guard (version == ("2.0" :: Text))
    Request <$> v .:  "method" <*>
                (v .:? "params") .!= emptyArray <*>
                v .:  "id"
  parseJSON _ = M.mempty

instance ToJSON a => ToJSON (Request a) where
  toJSON (Request m ps requestId) =
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method"  .= m
           , "params"  .= toJSON ps
           , "id"      .= requestId ]

data Response a = Result { result   :: a
                         , resultId :: Value }
                | Error  { errCode  :: Int
                         , errMsg   :: Text
                         , errRefId :: Maybe Value }
  deriving (Show)

instance ToLogStr (Response Value) where
  toLogStr (Result _ _) = toLogStr @String "Success"
  toLogStr (Error errC errM _) =
    toLogStr @String
    ("Error: [code: " <> show errC <> " - msg: " <> unpack errM <> "]")

instance FromJSON a => FromJSON (Response a) where
  parseJSON (Object v) = do
    version <- v .: "jsonrpc"
    guard (version == ("2.0" :: Text))
    fromResult <|> fromError

    where
      fromResult = Result <$> (v .: "result" >>= parseJSON)
                          <*> v .: "id"

      fromError = do
        err <- v .: "error"
        Error <$> err .: "code"
              <*> err .: "message"
              <*> v   .: "id"

  parseJSON _ = mempty

instance ToJSON (Response Value) where
  toJSON (Result x resId) = object [ "jsonrpc" .= ("2.0" :: Text)
                                   , "result"  .= x
                                   , "id"      .= resId ]
  toJSON (Error code msg errId) =
    let err = object [ "code"    .= code
                     , "message" .= msg ]
    in object [ "jsonrpc" .= ("2.0" :: Text)
              , "error"   .= err
              , "id"      .= errId ]


{-|
A wrapper over a monadic function that can either succeed or fail with a
'MethodError'.
Hides the input and output types.
-}
data Method m where
  Method :: forall i m e o. (FromJSON i, ToJSON o, Show e)
         => (i -> m (Either e o)) -> Method m

-- | Represents an error with an integer code and a textual message.
data MethodError = MethodError !Int !Text
  deriving (Eq, Show)


-- | A 'Method' with a name.
newtype NamedMethod m = NamedMethod { unWrap :: (Text, Method m) }

{-|
Builds a 'NamedMethod' given its name and function.
Useful in conjuction with 'fromList'.
-}
method :: forall i m e o. (FromJSON i, ToJSON o, Show e)
       => Text
       -> (i -> m (Either e o))
       -> NamedMethod m
method name f = NamedMethod (name, Method f)

-- | Collection of 'NamedMethod's.
newtype Methods m = Methods (S.Map Text (Method m))

-- | Builds a collection from a list of 'NamedMethod's.
fromList :: [NamedMethod m] -> Methods m
fromList = Methods . S.fromList . map unWrap

-- | Looks up the method corresponding to the given name.
lookup :: Methods m -> Text -> Maybe (Method m)
lookup (Methods m) name = S.lookup name m

handleRequest :: Monad m
              => Methods m
              -> (forall e. Show e => e -> MethodError)
              -> Request Value
              -> m (Response Value)
handleRequest methods toError request =
  case lookup methods (reqMethod request) of
    Nothing -> methodNotFound (reqId request)
    Just m  -> runMethod m toError request

runMethod :: (Applicative m, Monad m)
          => Method m
          -> (forall e. Show e => e -> MethodError)
          -> Request Value
          -> m (Response Value)
runMethod (Method f) mapError request = do
  let ri = reqId request
  case parseMaybe parseJSON (reqParams request) of
    Nothing -> invalidParams ri
    Just ps -> processResult ri . first mapError <$> f ps

processResult :: ToJSON a => Value -> Either MethodError a -> Response Value
processResult requestId (Left (MethodError code msg)) =
  Error code msg (Just requestId)
processResult requestId (Right res)                   =
  Result (toJSON res) requestId


methodNotFound :: (Monad m) => Value -> m (Response Value)
methodNotFound = mkError (-32601) "Method not found" . Just

invalidParams :: (Monad m) => Value -> m (Response Value)
invalidParams = mkError (-32602) "Invalid params" . Just

mkError :: (Monad m) => Int -> Text -> Maybe Value -> m (Response Value)
mkError code msg idErr = return (Error code msg idErr)

