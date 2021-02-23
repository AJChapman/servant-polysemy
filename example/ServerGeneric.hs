{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Control.Lens.Operators
import Data.Version (Version, showVersion)
import Paths_servant_polysemy as Paths
import Polysemy
import Polysemy.Error
import Servant
import Servant.Polysemy.Server
import Servant.API.Generic (ToServantApi, (:-), Generic)

{-
This example is of the same server as defined in Server.hs, but it uses Servant.API.Generic to define the path.
-}

-- This is the Servant.API.Generic style  of creating an API.
data Routes route = Routes
  { _version :: route :- "api" :> "v1" :> "version" :> Get '[JSON] Version
  } deriving (Generic)

-- This is the endpoint, which responds to a GET at /api/v1/version with the server's version in JSON format.
type MyApi = ToServantApi Routes

-- This is the implementation of the API.
-- It also does some IO to print to the terminal.
myServer :: Member (Embed IO) r => ServerT MyApi (Sem (Error ServerError ': r))
myServer = do
  embed $ putStrLn $ "Returning version " <> showVersion Paths.version
  pure Paths.version

-- This runs Warp (a Haskell web server), serving up our API.
main :: IO ()
main =
  runWarpServer @MyApi 8080 True myServer
    & runM


