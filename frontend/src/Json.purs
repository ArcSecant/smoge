module Json where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either)

type MapInfo
  = { mapName :: String
    , mapDiff :: String
    , mapper :: String
    , starRating :: String
    , drainTime :: String
    , ar :: String
    , od :: String
    , hp :: String
    }

emptyMapInfo :: MapInfo
emptyMapInfo =
  { mapName: ""
  , mapDiff: ""
  , mapper: ""
  , starRating: ""
  , drainTime: ""
  , ar: ""
  , od: ""
  , hp: ""
  }

mapInfoToJson :: MapInfo -> Json
mapInfoToJson = encodeJson

mapInfoFromJson :: Json -> Either JsonDecodeError MapInfo
mapInfoFromJson = decodeJson
