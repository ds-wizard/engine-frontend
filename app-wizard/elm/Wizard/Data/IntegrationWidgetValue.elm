module Wizard.Data.IntegrationWidgetValue exposing
    ( IntegrationWidgetValue
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias IntegrationWidgetValue =
    { path : String
    , id : String
    , value : String
    }


decoder : Decoder IntegrationWidgetValue
decoder =
    D.succeed IntegrationWidgetValue
        |> D.required "path" D.string
        |> D.required "id" D.string
        |> D.required "value" D.string
