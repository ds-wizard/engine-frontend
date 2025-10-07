module Wizard.Api.Models.KnowledgeModel.Integration.WidgetIntegrationData exposing
    ( WidgetIntegrationData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias WidgetIntegrationData =
    { widgetUrl : String
    }


decoder : Decoder WidgetIntegrationData
decoder =
    D.succeed WidgetIntegrationData
        |> D.required "widgetUrl" D.string
