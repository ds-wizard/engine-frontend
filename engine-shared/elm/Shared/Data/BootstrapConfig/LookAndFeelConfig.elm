module Shared.Data.BootstrapConfig.LookAndFeelConfig exposing
    ( LookAndFeelConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias LookAndFeelConfig =
    { appTitle : String
    , loginInfo : Maybe String
    }


default : LookAndFeelConfig
default =
    { appTitle = "Data Stewardship Wizard"
    , loginInfo = Nothing
    }


decoder : Decoder LookAndFeelConfig
decoder =
    D.succeed LookAndFeelConfig
        |> D.optional "appTitle" D.string "DS Wizard"
        |> D.optional "loginInfo" (D.maybe D.string) Nothing
