module Wizard.Common.Config.GeneralConfig exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias GeneralConfig =
    { levelsEnabled : Bool
    , publicQuestionnaireEnabled : Bool
    , questionnaireAccessibilityEnabled : Bool
    , registrationEnabled : Bool
    }


decoder : Decoder GeneralConfig
decoder =
    D.succeed GeneralConfig
        |> D.optional "levelsEnabled" D.bool True
        |> D.optional "publicQuestionnaireEnabled" D.bool True
        |> D.optional "questionnaireAccessibilityEnabled" D.bool True
        |> D.optional "registrationEnabled" D.bool True
