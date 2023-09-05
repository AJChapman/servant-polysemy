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
Copyright   : (c) 2020 Alex Chapman
License     : BSD3
Maintainer  : alex@farfromthere.net
Stability   : experimental
Portability : GHC
Description : Utilities for running a Servant server in a polysemy stack using Warp.

A simple usage scenario is that you create your API,
then implement a server for it in a 'ServerT api (Sem (Error ServerError ': r))' monad (where 'api' is your API type),
then run it with 'runWarpServer'.
See <example/Server.hs> for a trivial example of this.

If you need to take your Servant-Polysemy server and run it in an ordinary Servant server then you can use 'hoistServerIntoSem'.
This can be used to e.g. add Swagger docs to your server, as in <example/ServerWithSwagger.hs>.
-}
module Servant.Polysemy.Server
  ( ServerSettings(..)
  , defaultServerSettings
  , Middleware(..)
  -- * Use ordinary Servant code in a Polysemy 'Sem'
  , hoistServerIntoSem
  , liftHandler

  -- * Use Servant-Polysemy code in an ordinary Servant/WAI system
  , serveSem
  , serveSemWithContext
  , semHandler

  -- * Use Warp to serve a Servant-Polysemy API in a 'Sem' stack.
  , runWarpServer
  , runWarpServerSettings

  -- * Context-aware variants of the above
  , runWarpServerCtx
  , runWarpServerSettingsCtx

  , runWarpServerEx
  , withHandler

  -- * Redirect paths in a Servant-Polysemy API
  , Redirect
  , redirect
  ) where

import Control.Monad.Except (ExceptT(..))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat)
import Network.Wai (Request, Response, ResponseReceived)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Polysemy
import Polysemy.Error
import Servant
       ( Application
       , Context
       , Handler(..)
       , HasServer
       , Header
       , Headers
       , JSON
       , NoContent(..)
       , Server
       , ServerContext
       , ServerError
       , ServerT
       , StdMethod(GET)
       , ToHttpApiData
       , Verb
       , addHeader
       , hoistServer
       , hoistServerWithContext
       , runHandler
       , serve
       , serveWithContext
       )
import Servant.Server (Context(..))

-- Types for this module
data ServerSettings ctx r = ServerSettings { warpSettings :: Warp.Settings
                                           , waiMiddleware :: [Wai.Middleware]
                                           , middleware :: [Middleware r]
                                           , context :: Context ctx
                                           }

defaultServerSettings :: ServerSettings '[] r
defaultServerSettings = ServerSettings Warp.defaultSettings [] [] EmptyContext

-- | Make a Servant 'Handler' run in a Polysemy 'Sem' instead.
liftHandler :: Members '[Error ServerError, Embed IO] r => Handler a -> Sem r a
liftHandler handler =
  embed (runHandler handler) >>= fromEither

-- | Hoist an ordinary Servant 'Server' into a 'ServerT' whose monad is 'Sem',
-- so that it can be used with 'serveSem'.
hoistServerIntoSem
  :: forall api r
   . ( HasServer api '[]
     , Members '[Error ServerError, Embed IO] r
     )
  => Server api -> ServerT api (Sem r)
hoistServerIntoSem =
  hoistServer (Proxy @api) (liftHandler @r)

-- | Turn a 'Sem' that can throw 'ServerError's into a Servant 'Handler'.
semHandler
  :: (forall x. Sem r x -> IO x)
  -> Sem (Error ServerError ': r) a
  -> Handler a
semHandler lowerToIO =
  Handler . ExceptT . lowerToIO . runError

-- | Turn a 'ServerT' that contains a 'Sem' (as returned by 'hoistServerIntoSem') into a WAI 'Application'.
serveSem
  :: forall api r
   . HasServer api '[]
  => (forall x. Sem r x -> IO x)
  -> ServerT api (Sem (Error ServerError ': r))
  -> Application
serveSem lowerToIO m = serve api (hoistServer api (semHandler lowerToIO) m)
  where api = Proxy @api

-- | Turn a 'ServerT' that contains a 'Sem' (as returned by 'hoistServerIntoSem') into a WAI 'Application'.
serveSemWithContext
  :: forall api r ctx
   . (HasServer api ctx, ServerContext ctx)
  => (forall x. Sem r x -> IO x)
  -> Context ctx
  -> ServerT api (Sem (Error ServerError ': r))
  -> Application
serveSemWithContext lowerToIO ctx m =
  serveWithContext api ctx (hoistServerWithContext api ctxp (semHandler lowerToIO) m)
  where api = Proxy @api
        ctxp = Proxy @ctx

-- | Lifted version of WAI 'Application'
type App r = Request -> (Response -> Sem r ResponseReceived) -> Sem r ResponseReceived

-- | Lifted version of WAI Middleware
--   Note that this is a newtype for the sole reason of having Semigroup and Monoid instances.
newtype Middleware r = Middleware { runMiddleware :: App r -> App r}

instance Semigroup (Middleware r) where
  Middleware f <> Middleware f' = Middleware (f . f')

instance Monoid (Middleware r) where
  mempty = Middleware id

liftApp :: Member (Embed IO) r =>(forall a. Sem r a -> IO a) -> Application -> App r
liftApp lowerToIO app request respond = embed (app request (lowerToIO . respond))

appToIO :: Member (Embed IO) r => (forall a. Sem r a -> IO a) -> App r -> Application
appToIO lowerToIO app request respond = lowerToIO (app request (embed . respond))

-- | Convert WAI Middleware type to our lifted version.
unwrapMiddleware :: Member (Embed IO) r => (forall a. Sem r a -> IO a) -> Middleware r -> Application -> Application
unwrapMiddleware lowerToIO (Middleware mw) app = appToIO lowerToIO (mw (liftApp lowerToIO app))

-- | Run the given server on the given port, possibly showing exceptions in the responses.
runWarpServer
  :: forall api r
   . ( HasServer api '[]
     , Member (Embed IO) r
     )
  => Warp.Port -- ^ The port to listen on, e.g. '8080'
  -> Bool -- ^ Whether to show exceptions in the http response (good for debugging but potentially a security risk)
  -> ServerT api (Sem (Error ServerError ': r)) -- ^ The server to run. You can create one of these with 'hoistServerIntoSem'.
  -> Sem r ()
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
  => Warp.Settings
  -> ServerT api (Sem (Error ServerError ': r))
  -> Sem r ()
runWarpServerSettings settings server = withLowerToIO $ \lowerToIO finished -> do
  Warp.runSettings settings (serveSem @api lowerToIO server)
  finished

-- | Run the given server on the given port, possibly showing exceptions in the responses.
runWarpServerCtx
  :: forall api r ctx
   . ( HasServer api ctx
     , ServerContext ctx
     , Member (Embed IO) r
     )
  => Warp.Port -- ^ The port to listen on, e.g. '8080'
  -> Bool -- ^ Whether to show exceptions in the http response (good for debugging but potentially a security risk)
  -> Context ctx
  -> ServerT api (Sem (Error ServerError ': r)) -- ^ The server to run. You can create one of these with 'hoistServerIntoSem'.
  -> Sem r ()
runWarpServerCtx port showExceptionResponse ctx server =
  let warpSettings = Warp.defaultSettings
        & Warp.setPort port
        & if showExceptionResponse
            then Warp.setOnExceptionResponse Warp.exceptionResponseForDebug
            else id
  in
    runWarpServerSettingsCtx @api warpSettings ctx server

-- | Run the given server with these Warp settings and Servant context.
runWarpServerSettingsCtx
  :: forall api r ctx
   . ( HasServer api ctx
     , ServerContext ctx
     , Member (Embed IO) r
     )
  => Warp.Settings
  -> Context ctx
  -> ServerT api (Sem (Error ServerError ': r))
  -> Sem r ()
runWarpServerSettingsCtx settings ctx server =
  withLowerToIO $ \lowerToIO finished -> do
    Warp.runSettings settings (serveSemWithContext @api lowerToIO ctx server)
    finished

runWarpServerEx :: forall api r ctx
                 . ( HasServer api ctx
                   , ServerContext ctx
                   , Member (Embed IO) r
                   )
                => ServerSettings ctx r
                -> ServerT api (Sem (Error ServerError ': r))
                -> Sem r ()
runWarpServerEx s srv =
  withLowerToIO $ \lowerToIO finish ->
    let mw = unwrapMiddleware lowerToIO (fold (middleware s)) . foldMiddleware (waiMiddleware s)
    in  Warp.runSettings (warpSettings s) (mw $ server lowerToIO) >> finish
  where server :: (forall x. Sem r x -> IO x) -> Application
        server lowerToIO = serveSemWithContext @api lowerToIO (context s) srv

foldMiddleware :: [Application -> Application] -> Application -> Application
foldMiddleware = foldr (.) id

withHandler :: forall handler req usr r a
             . Member (Embed IO) r
            => ((req -> Handler usr) -> handler req usr)
            -> (req -> Sem (Error ServerError ': r) usr)
            -> (handler req usr -> Sem r a)
            -> Sem r a
withHandler mkHandler f g = withLowerToIO $ \lowerToIO finish ->
  let h = mkHandlerSem lowerToIO f
  in  lowerToIO (g h) <*  finish
  where mkHandlerSem :: (forall z. Sem r z -> IO z)
                     -> (req -> Sem (Error ServerError ': r) usr)
                     -> handler req usr
        mkHandlerSem lowerToIO hdl = mkHandler (semHandler lowerToIO . hdl)

-- | A redirect response with the given code, the new location given in the given type, e.g:
-- > Redirect 302 Text
-- This will return a '302 Found' response, and we will use 'Text' in the server to say where it will redirect to.
type Redirect (code :: Nat) loc
  = Verb 'GET code '[JSON] (Headers '[Header "Location" loc] NoContent)

-- | Serve a redirect response to the given location, e.g:
-- > redirect "/api/v1"
redirect :: ToHttpApiData a => a -> Sem r (Headers '[Header "Location" a] NoContent)
redirect a = pure $ addHeader a NoContent

