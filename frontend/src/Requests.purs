module Requests where

import Affjax
import Data.Either
import Data.Maybe
import Prelude
import Affjax as AX
import Affjax.RequestBody as AXRB
import Affjax.ResponseFormat as AXR
import Data.Argonaut.Core (stringify, fromString)
import Data.Argonaut.Core as JSON
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.Class (liftAff)
import Halogen as H
import Halogen.Aff as HA

sendRequest :: String -> Aff String
sendRequest mapId = do
  response <- AX.get AXR.string ("http://localhost:3000?id=" <> mapId)
  case response of
    Right success -> pure $ success.body
    Left err -> pure $ AX.printError err
