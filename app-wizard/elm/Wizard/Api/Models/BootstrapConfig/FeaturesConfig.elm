module Wizard.Api.Models.BootstrapConfig.FeaturesConfig exposing
    ( FeaturesConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias FeaturesConfig =
    { aiAssistantEnabled : Bool
    , toursEnabled : Bool
    }


default : FeaturesConfig
default =
    { aiAssistantEnabled = False
    , toursEnabled = True
    }


decoder : Decoder FeaturesConfig
decoder =
    D.succeed FeaturesConfig
        |> D.required "aiAssistantEnabled" D.bool
        |> D.required "toursEnabled" D.bool
