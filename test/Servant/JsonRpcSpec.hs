{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Servant.JsonRpcSpec
  (spec) where

import           Control.Monad.Identity
import           Data.Aeson             hiding (Error)
import           Data.Text
import           Servant.JsonRpc
import           Test.Hspec

spec :: Spec
spec =
  describe "Servant JsonRpc module" $ do
    it "run success method if found" $
      executeFoundMethod `shouldSatisfy` isSuccessResult
    it "method not found if there is no method with that name" $
      executeNotDeclaredMethod `shouldSatisfy` notFoundMethod
    it "invalid params if does not match with requested" $
      executeWrongParams `shouldSatisfy` invalidParams
    it "map Error if method return Left" $
      executeMethodMapError `shouldSatisfy` mappedError


methods :: Methods Identity
methods = fromList [ method "found_method" foundMethod
                   , method "mapped_error" errorMethod ]

foundMethod :: Int -> Identity (Either String String)
foundMethod _ = Identity $ Right "successResult"

errorMethod :: Int -> Identity (Either String String)
errorMethod _ = Identity $ Left "Error"

executeMethod :: Text -> Value -> Response Value
executeMethod name params = runIdentity $ handlerMethod request
  where
    handlerMethod = handleRequest methods (\_ -> MethodError 1 "")
    request = Request { reqMethod = name
                      , reqParams = params
                      , reqId = toJSON @Int 1 }

executeFoundMethod :: Response Value
executeFoundMethod = executeMethod "found_method" $ toJSON @Int 1

isSuccessResult :: Response Value -> Bool
isSuccessResult = (toJSON @String "successResult" ==) . toJSON . result

executeNotDeclaredMethod :: Response Value
executeNotDeclaredMethod = executeMethod "not_found" $ toJSON @Int 1

checkErrorCode :: Int -> Response Value -> Bool
checkErrorCode expectedCode (Error code _ _) = code == expectedCode
checkErrorCode _ _                           = False

notFoundMethod :: Response Value -> Bool
notFoundMethod = checkErrorCode (-32601)

executeWrongParams :: Response Value
executeWrongParams = executeMethod "found_method" $ toJSON @String "params"

invalidParams :: Response Value -> Bool
invalidParams = checkErrorCode (-32602)

executeMethodMapError :: Response Value
executeMethodMapError = executeMethod "mapped_error" $ toJSON @Int 1

mappedError :: Response Value -> Bool
mappedError = checkErrorCode 1
