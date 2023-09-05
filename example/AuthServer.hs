-- | This is the example of how to use generalised authentication from Servant
--   with Polysemy in mind.
--   See here for more info: https://docs.servant.dev/en/stable/tutorial/Authentication.html
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Version (Version, showVersion)
import Network.Wai
import Paths_servant_polysemy as Paths
import Polysemy
import Polysemy.Error
import Servant
import Servant.API.Generic (ToServantApi, (:-), Generic)
import Servant.Polysemy.Server
import Servant.Polysemy.Server.Auth
import Servant.Server.Experimental.Auth
import Servant.Server.Generic (AsServerT)

newtype Token = Token String

type TokenProtect = AuthProtect "token"

type instance AuthServerData TokenProtect = Token

-- This example is of the same server as defined in Server.hs, but it uses Servant.API.Generic to define the path.

-- This is the Servant.API.Generic style  of creating an API.
data Routes route = Routes
  { _version :: route :- "api" :> "v1" :> "version" :> Get '[JSON] Version
  , _secret :: route :- TokenProtect :> "api" :> "v1" :> "secret" :> Get '[JSON] String
  } deriving (Generic)

type MyApi = ToServantApi Routes

-- This is the endpoint, which responds to a GET at /api/v1/version with the server's version in JSON format.
handleVersion :: Member (Embed IO) r => Sem r Version
handleVersion = do
  embed $ putStrLn $ "Returning version " <> showVersion Paths.version
  pure Paths.version

-- This is the endpoint, which requires authentication to access.
handleSecret :: Member (Embed IO) r => Token -> Sem r String
handleSecret (Token tok) = do
  embed $ putStrLn $ "Supplied with token " <> tok
  return "Ok"

routes :: Member (Embed IO) r => Routes (AsServerT (Sem (Error ServerError ': r)))
routes = Routes
  { _version = handleVersion
  , _secret = handleSecret
  }

authHandler :: Members '[Embed IO, Error ServerError] r
            => Request -> Sem r Token
authHandler req = case token of
                    Nothing -> embed (putStrLn "Authentication failed: no token provided") >> throw401 "No token provided"
                    Just t | t == "magictoken" -> Token "magictoken" <$ embed (putStrLn "Authentication successful")
                           | otherwise -> embed (putStrLn "Authentication failed: token invalid") >> throw401 "Invalid token"
  where throw401 msg = throw (ServerError 401 msg mempty [])
        token = lookup "X-Token" $ requestHeaders req

myServer :: Member (Embed IO) r => ServerT MyApi (Sem (Error ServerError ': r))
myServer = toServant routes

-- This runs Warp (a Haskell web server), serving up our API.
main :: IO ()
main = runM
     $ withAuthHandler authHandler $ \ahdl ->
       let ctx :: Context (AuthHandler Request Token ': '[])
           ctx = ahdl :. EmptyContext
       in runWarpServerCtx @MyApi 8080 True ctx myServer


