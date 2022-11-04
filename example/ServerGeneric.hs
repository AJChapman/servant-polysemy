{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Data.Function ((&))
import Data.Version (Version, showVersion)
import Paths_servant_polysemy as Paths
import Polysemy
import Polysemy.Error
import Servant
import Servant.API.Generic
import Servant.Polysemy.Server
import Servant.Server.Generic

{-
This example is of the same server as defined in Server.hs, but it uses Servant.API.Generic to define the path.
-}

-- This is the Servant.API.Generic style  of creating an API.
data Routes route = Routes
  { _version :: route :- "api" :> "v1" :> "version" :> Get '[JSON] Version
  } deriving (Generic)

-- This is the endpoint, which responds to a GET at /api/v1/version with the server's version in JSON format.
type MyApi = ToServantApi Routes

-- The handler for the route
versionHandler :: Members [Embed IO, Error ServerError] r => Sem r Version
versionHandler = do
  embed (putStrLn $ "Returning version " <> showVersion Paths.version)
  pure Paths.version

routes :: Members [Embed IO, Error ServerError] r => Routes (AsServerT (Sem r))
routes = Routes { _version = versionHandler }

myServer :: Member (Embed IO) r => ServerT MyApi (Sem (Error ServerError ': r))
myServer = toServant routes

-- This runs Warp (a Haskell web server), serving up our API.
main :: IO ()
main = runWarpServer @MyApi 8080 True myServer
     & runM


