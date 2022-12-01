{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Polysemy.Server.Auth (mkAuthHandlerSem, withAuthHandler) where

import Servant.Polysemy.Server

import Polysemy
import Polysemy.Error
import Servant.Server.Experimental.Auth
import Servant (ServerError)

-- | Create an authentication handler from a supplied effectful function.
--   The first argument is supposed to be obtained from 'withLowerToIO'.
--   This is a low-level function and you probably shouldn't use it, unless
--   you're absolutely sure of what you're doing. For a more user-friendly version
--   see 'withAuthHandler'.
mkAuthHandlerSem :: (forall x. Sem r x -> IO x) -> (auth -> Sem (Error ServerError ': r) usr) -> AuthHandler auth usr
mkAuthHandlerSem lowerToIO hdl = mkAuthHandler (semHandler lowerToIO . hdl)

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
withAuthHandler f g = withLowerToIO $ \lowerToIO finish ->
  let hdl = mkAuthHandlerSem lowerToIO f
  in lowerToIO (g hdl) <* finish
