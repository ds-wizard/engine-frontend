module Wizard.Common.Config.InfoConfig exposing
    ( InfoConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias InfoConfig =
    { welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , loginInfo : Maybe String
    }


decoder : Decoder InfoConfig
decoder =
    D.succeed InfoConfig
        |> D.optional "welcomeInfo" (D.maybe D.string) Nothing
        |> D.optional "welcomeWarning" (D.maybe D.string) Nothing
        |> D.optional "loginInfo" (D.maybe D.string) Nothing


default : InfoConfig
default =
    { welcomeInfo = Nothing
    , welcomeWarning = Nothing
    , loginInfo = Nothing
    }
