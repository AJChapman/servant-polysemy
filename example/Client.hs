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

type MyApi = "api" :> "v1" :> "version" :> Get '[JSON] Version

versionClient :: ClientM Version
versionClient = client (Proxy @MyApi)

program :: Members '[ServantClient, Embed IO] r => Sem r ()
program = do
  ev <- runClient versionClient & runError
  case ev of
    Left err ->
      embed $ putStrLn $ "Client failed: " <> show err
    Right v ->
      embed $ putStrLn $ "Received version: " <> showVersion v

main :: IO ()
main = program
  & runServantClient "localhost:8080"
  & runM
