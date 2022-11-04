{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Function ((&))
import Data.Version (Version, showVersion)
import Paths_servant_polysemy as Paths
import Polysemy
import Polysemy.Error
import Servant
import Servant.Polysemy.Server

{-
This is a very simple server with only one endpoint.
-}

-- This is the endpoint, which responds to a GET at /api/v1/version with the server's version in JSON format.
type MyApi = "api" :> "v1" :> "version" :> Get '[JSON] Version

-- This is the implementation of the API.
-- It also does some IO to print to the terminal.
myServer :: Member (Embed IO) r => ServerT MyApi (Sem (Error ServerError ': r))
myServer = do
  embed $ putStrLn $ "Returning version " <> showVersion Paths.version
  pure Paths.version

-- This runs Warp (a Haskell web server), serving up our API.
main :: IO ()
main = runWarpServer @MyApi 8080 True myServer
     & runM

