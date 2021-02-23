{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Lens.Operators
import Data.Swagger (Swagger, description, host, info, license, title, version)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Paths_servant_polysemy as Paths
import Polysemy
import Polysemy.Error
import Servant
import Servant.Polysemy.Server
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

{-
This is the same server as in Server.hs, but is self-documenting.
-}

-- The same API type as in Server.hs.
type MyApi = "api" :> "v1" :> "version" :> Get '[JSON] Version

-- This API has the swagger docs at /api/v1/swagger-ui
type SwaggerDocs = "api" :> "v1" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

-- Now we combine the two APIs into one, and also add redirects:
--   /       -> /api/v1/swagger-ui
--   /api    -> /api/v1/swagger-ui
--   /api/v1 -> /api/v1/swagger-ui
-- The redirects are just to help people find the documentation.
type MyApiWithSwagger =
  MyApi
  :<|> SwaggerDocs
  :<|> "api" :> "v1" :> Redirect 302 Text -- Redirect /api/v1 to the swagger docs
  :<|> "api" :> Redirect 302 Text -- Redirect /api to the swagger docs
  :<|> Redirect 302 Text -- Redirect / to the swagger docs

-- The same server from Server.hs
myServer :: Member (Embed IO) r => ServerT MyApi (Sem (Error ServerError ': r))
myServer = do
  embed $ putStrLn $ "Returning version " <> showVersion Paths.version
  pure Paths.version

-- The swagger endpoint implementation.
mySwagger :: Swagger
mySwagger = toSwagger (Proxy @MyApi)
  & info.title       .~ "My API"
  & info.version     .~ (T.pack . showVersion) Paths.version
  & info.description ?~ "This is just an example API."
  & info.license     ?~ "Public Domain"
  & host             ?~ "localhost:8080"

-- This server adds the Swagger docs and redirects to the 'myServer' implementation.
mySwaggerServer :: Member (Embed IO) r => ServerT MyApiWithSwagger (Sem (Error ServerError ': r))
mySwaggerServer =
  myServer
  :<|> hoistServerIntoSem @SwaggerDocs (swaggerSchemaUIServer mySwagger)
  :<|> redirect "/api/v1/swagger-ui"
  :<|> redirect "/api/v1/swagger-ui"
  :<|> redirect "/api/v1/swagger-ui"

-- Run 'mySwaggerServer' on port 8080.
main :: IO ()
main =
  runWarpServer @MyApiWithSwagger 8080 True mySwaggerServer
    & runM
