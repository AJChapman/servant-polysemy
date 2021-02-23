{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Function ((&))
import Data.Version (Version, showVersion)
import Polysemy
import Polysemy.Error
import Servant
import Servant.Client.Streaming (ClientM, client)
import Servant.Polysemy.Client (ServantClient, runClient, runServantClient)

{-
This is an example client for connecting to the API defined in each of Server.hs, ServerGeneric.hs, and ServerWithSwagger.hs.
-}

-- This is the same path defined in the server.
-- Typically you would have this in a common place rather than reproducing it here.
type MyApi = "api" :> "v1" :> "version" :> Get '[JSON] Version

-- The client is automatically generated from the API type.
versionClient :: ClientM Version
versionClient = client (Proxy @MyApi)

-- This program runs the client and then handles the response by printing info to stdout.
program :: Members '[ServantClient, Embed IO] r => Sem r ()
program = do
  ev <- runClient versionClient & runError
  case ev of
    Left err ->
      embed $ putStrLn $ "Client failed: " <> show err
    Right v ->
      embed $ putStrLn $ "Received version: " <> showVersion v

-- This runs the client, connecting to localhost:8080.
main :: IO ()
main = program
  & runServantClient "localhost:8080"
  & runM
