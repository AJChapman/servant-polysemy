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
  , runServantClientStreamingUrl
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

-- | The 'ServantClient' effect allows you to run a 'ClientM' as automatically generated for your API by the servant-client package.
data ServantClient m a where
  RunClient' :: NFData o => ClientM o -> ServantClient m (Either ClientError o)

makeSem ''ServantClient

-- | Run this 'ClientM' in the 'Sem' monad.
runClient
  :: (Members '[ServantClient, Error ClientError] r, NFData o)
  => ClientM o -> Sem r o
runClient = runClient' >=> fromEither

-- | Interpret the 'ServantClient' effect by running any calls to 'RunClient'' against the given 'BaseUrl'.
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

-- | Parse the given string as a URL and then behave as 'runServantClientUrl' does.
runServantClient
  :: Member (Embed IO) r
  => String -> Sem (ServantClient ': r) a -> Sem r a
runServantClient server m = do
  server' <- embed $ parseBaseUrl server
  runServantClientUrl server' m

-- | The 'ServantClientStreaming' effect is just like the 'ServantClient' effect,
-- but allows streaming connections.
data ServantClientStreaming m a where
  RunClientStreaming :: ClientM o -> ServantClientStreaming m o

makeSem ''ServantClientStreaming

-- | Interpret the 'ServantClientStreaming' effect by running any calls to 'RunClientStreaming' against the given URL.
runServantClientStreamingUrl
  :: Members
    '[ Cont ref
     , Embed IO
     , Error ClientError
     ] r
  => BaseUrl -> Sem (ServantClientStreaming ': r) a -> Sem r a
runServantClientStreamingUrl server m = do
  manager <- embed $ newManager tlsManagerSettings
  let env = mkClientEnv manager server
  interpret (\case
    RunClientStreaming client ->
      subst (\continue ->
        withLowerToIO $ \unliftIO _ ->
          withClientM client env (unliftIO . jump continue)
        ) fromEither
    ) m

-- | Parse the given string as a URL and then behave as 'runServantClientStreamingUrl'.
runServantClientStreaming
 :: Members
    '[ Cont ref
     , Embed IO
     , Error ClientError
     ] r
  => String -> Sem (ServantClientStreaming ': r) a -> Sem r a
runServantClientStreaming server m = do
  server' <- embed $ parseBaseUrl server
  runServantClientStreamingUrl server' m
