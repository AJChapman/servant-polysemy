{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Polysemy.Server.Auth (withAuthHandler) where

import Servant.Polysemy.Server

import Polysemy
import Polysemy.Error
import Servant.Server.Experimental.Auth
import Servant (ServerError)

-- | Create an authentication handler and submit it to a callback function.
--   'withAuthHandler' uses 'withLowerToIO' under the hood, so all limitations
--   of that function are present here. Notably, you should compile your code with
--   "-threaded" GHC flag.
--   Use example:
--   @
--     authHandler :: Members '[Embed IO, Error ServerError] r
--                 => Request -> Sem r Token
--     authHandler req = <...>  -- Validate the request and extract the token
--
--     main :: IO ()
--     main = runM
--          $ withAuthHandler authHandler $ \ahdl ->
--            let ctx :: Context (AuthHandler Request Token ': '[])
--                ctx = ahdl :. EmptyContext
--            in runWarpServerCtx @MyApi 8080 True ctx myServer
--    @
--    See servant documentation here for more details: https://docs.servant.dev/en/stable/tutorial/Authentication.html
--    Also, see the example in 'examples/AuthServer.hs'.

withAuthHandler :: Member (Embed IO) r
                => (auth -> Sem (Error ServerError ': r) usr)
                -> (AuthHandler auth usr -> Sem r a)
                -> Sem r a
withAuthHandler = withHandler mkAuthHandler
