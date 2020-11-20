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

type MyApi = "api" :> "v1" :> "version" :> Get '[JSON] Version

type SwaggerDocs = "api" :> "v1" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

type MyApiWithSwagger =
  MyApi
  :<|> SwaggerDocs
  :<|> "api" :> "v1" :> Redirect 302 Text -- Redirect /api/v1 to the swagger docs
  :<|> "api" :> Redirect 302 Text -- Redirect /api to the swagger docs
  :<|> Redirect 302 Text -- Redirect / to the swagger docs

myServer :: Member (Embed IO) r => ServerT MyApi (Sem (Error ServerError ': r))
myServer = do
  embed $ putStrLn $ "Returning version " <> showVersion Paths.version
  pure Paths.version

mySwagger :: Swagger
mySwagger = toSwagger (Proxy @MyApi)
  & info.title       .~ "My API"
  & info.version     .~ (T.pack . showVersion) Paths.version
  & info.description ?~ "This is just an example API."
  & info.license     ?~ "Public Domain"
  & host             ?~ "localhost:8080"

mySwaggerServer :: Member (Embed IO) r => ServerT MyApiWithSwagger (Sem (Error ServerError ': r))
mySwaggerServer =
  myServer
  :<|> hoistServerIntoSem @SwaggerDocs (swaggerSchemaUIServer mySwagger)
  :<|> redirect "/api/v1/swagger-ui"
  :<|> redirect "/api/v1/swagger-ui"
  :<|> redirect "/api/v1/swagger-ui"

main :: IO ()
main =
  runWarpServer @MyApiWithSwagger 8080 True mySwaggerServer
    & runM
