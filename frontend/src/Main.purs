module Main where

import Data.Array
import Data.Either
import Data.Maybe
import Json
import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXR
import Data.Argonaut (printJsonDecodeError)
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Requests (sendRequest)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State
  = { mapId :: String
    , mapInfo :: Array MapInfo
    }

data Action
  = SendReq String
  | HandleInput String

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { mapId: "", mapInfo: [] }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    -- [ HH.div_ $ map (\x -> HH.text x.mapName) state.mapInfo
    [ HH.text $ show $ state.mapInfo
    , HH.div_
        [ HH.input
            [ HP.value state.mapId
            , HE.onValueInput HandleInput
            ]
        , HH.button
            [ HP.title "Get Map Info"
            , HE.onClick \_ -> SendReq state.mapId
            ]
            [ HH.text "Get Map Info" ]
        ]
    ]

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction (HandleInput val) = do
  H.modify_ (_ { mapId = val })

handleAction (SendReq mapId) = do
  response <- H.liftAff $ AX.request (AX.defaultRequest { url = ("/api/?id=" <> mapId), method = Left GET, responseFormat = AXR.json })
  case response of
    Left err -> H.modify_ \st -> st { mapInfo = emptyMapInfo : st.mapInfo }
    Right success -> case mapInfoFromJson success.body of
      Left err -> H.modify_ \st -> st { mapInfo = emptyMapInfo : st.mapInfo }
      Right m -> H.modify_ \st -> st { mapInfo = m : st.mapInfo }
