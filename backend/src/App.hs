{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Debug.Trace
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Requests
import Servant
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe

data Payload = Payload
  { mapName :: String,
    mapDiff :: String,
    mapper :: String,
    starRating :: Float,
    drainTime :: Float,
    ar :: Float,
    od :: Float,
    hp :: Float
  }
  deriving (Generic, Show)

instance ToJSON Payload

instance FromJSON Payload

-- * api

type Api = "api" :> QueryParam "id" Int :> Get '[JSON] Payload

-- * app

run :: IO ()
run = do
  let port = 5000
      settings =
        setPort port $
          setBeforeMainLoop
            (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
  runSettings settings mkApp

api :: Proxy Api
api = Proxy

mkApp :: Application
mkApp = serve api handler

handler :: Maybe Int -> Handler Payload
handler Nothing = return $ Payload {}
handler (Just mapId) = do
  mapInfo <- liftIO $ getMapInfo (show mapId)
  case mapInfo of
    Left err -> return $ Payload {}
    Right mapInfo ->
      return $
        Payload
          { mapName = title mapInfo,
            mapDiff = version mapInfo,
            mapper = creator mapInfo,
            starRating = read $ difficultyrating mapInfo,
            drainTime = read $ hit_length mapInfo,
            ar = read $ diff_approach mapInfo,
            od = read $ diff_overall mapInfo,
            hp = read $ diff_drain mapInfo
          }