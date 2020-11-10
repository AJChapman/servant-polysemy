{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Servant.Polysemy.Server
Description : Utilities for running a Servant server in a polysemy stack using Warp.
-}
module Servant.Polysemy.Server
  ( liftHandler
  , hoistServerIntoSem
  , semHandler
  , serveSem
  , runWarpServer
  , runWarpServerSettings
  , Redirect
  , redirect
  ) where

import Control.Monad.Except (ExceptT(..))
import Data.Function ((&))
import GHC.TypeLits (Nat)
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy
import Polysemy.Error
import Servant

liftHandler :: Members '[Error ServerError, Embed IO] r => Handler a -> Sem r a
liftHandler handler =
  embed (runHandler handler) >>= fromEither

hoistServerIntoSem
  :: forall api r
   . ( HasServer api '[]
     , Members '[Error ServerError, Embed IO] r
     )
  => Server api -> ServerT api (Sem r)
hoistServerIntoSem =
  hoistServer (Proxy @api) (liftHandler @r)

semHandler
  :: (forall x. Sem r x -> IO x)
  -> Sem (Error ServerError ': r) a
  -> Handler a
semHandler lowerToIO =
  Handler . ExceptT . lowerToIO . runError

serveSem
  :: forall api r
   . HasServer api '[]
  => (forall x. Sem r x -> IO x)
  -> ServerT api (Sem (Error ServerError ': r))
  -> Application
serveSem lowerToIO m = let api = Proxy @api
  in serve api (hoistServer api (semHandler lowerToIO) m)

-- | Run the given server on the given port, possibly showing exceptions in the responses.
runWarpServer
  :: forall api r
   . ( HasServer api '[]
     , Member (Embed IO) r
     )
  => Warp.Port -- ^ The port to listen on, e.g. '8080'
  -> Bool -- ^ Whether to show exceptions in the http response (good for debugging but potentially a security risk)
  -> ServerT api (Sem (Error ServerError ': r)) -> Sem r ()
runWarpServer port showExceptionResponse server =
  let warpSettings = Warp.defaultSettings
        & Warp.setPort port
        & if showExceptionResponse
            then Warp.setOnExceptionResponse Warp.exceptionResponseForDebug
            else id
  in
    runWarpServerSettings @api warpSettings server

-- | Run the given server with these Warp settings.
runWarpServerSettings
  :: forall api r
   . ( HasServer api '[]
     , Member (Embed IO) r
     )
  => Warp.Settings -> ServerT api (Sem (Error ServerError ': r)) -> Sem r ()
runWarpServerSettings settings server = withLowerToIO $ \lowerToIO finished -> do
  Warp.runSettings settings (serveSem @api lowerToIO server)
  finished

-- | A redirect response with the given code, the new location given in the given type, e.g:
-- > Redirect 302 Text
-- This will return a '302 Found' response, and we will use 'Text' in the server to say where it will redirect to.
type Redirect (code :: Nat) loc
  = Verb 'GET code '[JSON] (Headers '[Header "Location" loc] NoContent)

-- | Serve a redirect response to the given location, e.g:
-- > redirect "/api/v1"
redirect :: ToHttpApiData a => a -> Sem r (Headers '[Header "Location" a] NoContent)
redirect a = pure $ addHeader a NoContent

