{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Version (Version, showVersion)
import Paths_servant_polysemy as Paths
import Network.Wai (mapResponseHeaders)
import Network.Wai.Handler.Warp
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

addHeader1 :: Application -> Application
addHeader1 app request resp = app request (resp . mapResponseHeaders (hdr:))
  where hdr = ("Header1", "DERP")

addHeader2 :: Middleware r
addHeader2 = Middleware $ \app req resp -> app req (resp . mapResponseHeaders (hdr:))
  where hdr = ("Header2", "HERP")

-- This runs Warp (a Haskell web server), serving up our API.
main :: IO ()
main = runM
     $ runWarpServerEx @MyApi s myServer
  where s = defaultServerSettings { waiMiddleware = [addHeader1]
                                  , middleware = [addHeader2]
                                  , warpSettings = setPort 8080
                                                 $ setOnExceptionResponse exceptionResponseForDebug
                                                 $ defaultSettings
                                  }

