module Wizard.Settings.Common.EditableInfoConfig exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias EditableInfoConfig =
    { welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , loginInfo : Maybe String
    }


decoder : Decoder EditableInfoConfig
decoder =
    D.succeed EditableInfoConfig
        |> D.required "welcomeInfo" (D.maybe D.string)
        |> D.required "welcomeWarning" (D.maybe D.string)
        |> D.required "loginInfo" (D.maybe D.string)


encode : EditableInfoConfig -> E.Value
encode config =
    E.object
        [ ( "welcomeInfo", E.maybe E.string config.welcomeInfo )
        , ( "welcomeWarning", E.maybe E.string config.welcomeWarning )
        , ( "loginInfo", E.maybe E.string config.loginInfo )
        ]
