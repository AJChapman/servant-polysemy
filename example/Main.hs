{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Lens.Operators
import Data.Version (Version)
import Paths_servant_polysemy as Paths
import Polysemy
import Polysemy.Error
import Servant
import Servant.Polysemy.Server

type MyApi = "api" :> "v1" :> "version" :> Get '[JSON] Version

myServer :: ServerT MyApi (Sem (Error ServerError ': r))
myServer = pure Paths.version

main :: IO ()
main =
  runWarpServer @MyApi 8080 True myServer
    & runM

