{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Servant.Polysemy.Client
Description : A Polysemy effect for running Servant commands (ClientM).
|-}
module Servant.Polysemy.Client
  ( ServantClient
  , runClient'
  , runClient
  , runServantClientUrl
  , runServantClient
  , ClientError
  , ServantClientStreaming
  , runClientStreaming
  , runServantClientStreaming
  ) where

import Control.DeepSeq (NFData)
import Control.Monad ((>=>))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Polysemy
import Polysemy.Cont
import Polysemy.Error
import Servant.Client.Streaming
       ( BaseUrl
       , ClientError
       , ClientM
       , mkClientEnv
       , parseBaseUrl
       , runClientM
       , withClientM
       )

data ServantClient m a where
  RunClient' :: NFData o => ClientM o -> ServantClient m (Either ClientError o)

makeSem ''ServantClient

runClient
  :: (Members '[ServantClient, Error ClientError] r, NFData o)
  => ClientM o -> Sem r o
runClient = runClient' >=> fromEither

runServantClientUrl
  :: Member (Embed IO) r
  => BaseUrl -> Sem (ServantClient ': r) a -> Sem r a
runServantClientUrl server m = do
  manager <- embed $ newManager tlsManagerSettings
  let env = mkClientEnv manager server
  interpret (\case
    RunClient' client ->
      embed $ runClientM client env
    ) m

runServantClient
  :: Member (Embed IO) r
  => String -> Sem (ServantClient ': r) a -> Sem r a
runServantClient server m = do
  server' <- embed $ parseBaseUrl server
  runServantClientUrl server' m

data ServantClientStreaming m a where
  RunClientStreaming :: ClientM o -> ServantClientStreaming m o

makeSem ''ServantClientStreaming

runServantClientStreaming
  :: Members
    '[ Cont ref
     , Embed IO
     , Error ClientError
     ] r
  => BaseUrl -> Sem (ServantClientStreaming ': r) a -> Sem r a
runServantClientStreaming server m = do
  manager <- embed $ newManager tlsManagerSettings
  let env = mkClientEnv manager server
  interpret (\case
    RunClientStreaming client ->
      subst (\continue ->
        withLowerToIO $ \unliftIO _ ->
          withClientM client env (unliftIO . jump continue)
        ) fromEither
    ) m

