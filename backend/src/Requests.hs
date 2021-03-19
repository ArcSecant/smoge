{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Requests where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT (foreach)

type Result a = Either String a

data MapInfo = MapInfo
  { creator :: String,
    difficultyrating :: String,
    diff_size :: String,
    diff_overall :: String,
    diff_approach :: String,
    diff_drain :: String,
    hit_length :: String,
    title :: String,
    version :: String,
    max_combo :: String
  }
  deriving (Show, Generic)

instance ToJSON MapInfo

instance FromJSON MapInfo

type ApiReq = QueryParam "k" String :> QueryParam "b" String :> Get '[JSON] [MapInfo]

apiReq :: Proxy ApiReq
apiReq = Proxy

mapInfo :: Maybe String -> Maybe String -> ClientM [MapInfo]
mapInfo = client apiReq

getMapInfo :: String -> IO (Result MapInfo)
getMapInfo id = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Https "osu.ppy.sh" 443 "/api/get_beatmaps"))
  case res of
    Left err -> return $ Left ("Error: " ++ show err)
    Right m -> if null m then return $ Left "Error: No Map Found" else return $ Right (head m)
  where
    queries = mapInfo (Just "0fbc401dd8466ad9901c5b465b217b3f074f374d") (Just id)