module Shared.Data.BootstrapConfig.LookAndFeelConfig exposing
    ( LookAndFeelConfig
    , decoder
    , default
    , defaultAppTitle
    , defaultAppTitleShort
    , getAppTitle
    , getAppTitleShort
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)


type alias LookAndFeelConfig =
    { appTitle : Maybe String
    , appTitleShort : Maybe String
    , customMenuLinks : List CustomMenuLink
    , loginInfo : Maybe String
    }


default : LookAndFeelConfig
default =
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = []
    , loginInfo = Nothing
    }


defaultAppTitle : String
defaultAppTitle =
    "{defaultAppTitle}"


defaultAppTitleShort : String
defaultAppTitleShort =
    "{defaultAppTitleShort}"


getAppTitle : LookAndFeelConfig -> String
getAppTitle config =
    Maybe.withDefault defaultAppTitle config.appTitle


getAppTitleShort : LookAndFeelConfig -> String
getAppTitleShort config =
    Maybe.withDefault defaultAppTitleShort config.appTitleShort



-- JSON


decoder : Decoder LookAndFeelConfig
decoder =
    D.succeed LookAndFeelConfig
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "appTitleShort" (D.maybe D.string)
        |> D.required "customMenuLinks" (D.list CustomMenuLink.decoder)
        |> D.required "loginInfo" (D.maybe D.string)
