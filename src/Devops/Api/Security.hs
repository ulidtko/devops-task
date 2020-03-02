{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Devops.Api.Security
  (basicAuthServerContext
  , AuthUser
  ) where

import           Devops.Config
import           Data.Text
import           Data.Text.Encoding
import           Servant
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))

newtype AuthUser = AuthUser Text

authCheck :: Config -> BasicAuthCheck AuthUser
authCheck conf = BasicAuthCheck checkUser
  where
    checkUser requestedUser = do
      let confAuthUser = basicAuthConf conf
      if validateUser confAuthUser requestedUser
        then return $ Authorized (AuthUser $ userName confAuthUser)
        else return Unauthorized

validateUser :: BasicAuthConfig -> BasicAuthData -> Bool
validateUser (BasicAuthConfig user pass) (BasicAuthData user' pass') =
  user == decodeUtf8 user' && pass == decodeUtf8 pass'

basicAuthServerContext :: Config -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext conf = authCheck conf :. EmptyContext

