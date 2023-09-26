module Shared.Data.BootstrapConfig.LookAndFeelConfig exposing
    ( LookAndFeelConfig
    , anyColorSet
    , decoder
    , default
    , defaultAppTitle
    , defaultAppTitleShort
    , defaultLogoUrl
    , getAppTitle
    , getAppTitleShort
    , getIllustrationsColor
    , getLogoUrl
    , getPrimaryColor
    )

import Color exposing (Color)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Color as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)


type alias LookAndFeelConfig =
    { appTitle : Maybe String
    , appTitleShort : Maybe String
    , customMenuLinks : List CustomMenuLink
    , illustrationsColor : Maybe Color
    , logoUrl : Maybe String
    , primaryColor : Maybe Color
    }


default : LookAndFeelConfig
default =
    { appTitle = Nothing
    , appTitleShort = Nothing
    , customMenuLinks = []
    , illustrationsColor = Nothing
    , logoUrl = Nothing
    , primaryColor = Nothing
    }


defaultAppTitle : String
defaultAppTitle =
    "{defaultAppTitle}"


defaultAppTitleShort : String
defaultAppTitleShort =
    "{defaultAppTitleShort}"


defaultPrimaryColor : Color
defaultPrimaryColor =
    Color.rgb255 0 51 170


defaultIllustrationsColor : Color
defaultIllustrationsColor =
    Color.rgb255 0 51 170


defaultLogoUrl : String
defaultLogoUrl =
    "/wizard/img/logo.svg"


getAppTitle : LookAndFeelConfig -> String
getAppTitle config =
    Maybe.withDefault defaultAppTitle config.appTitle


getAppTitleShort : LookAndFeelConfig -> String
getAppTitleShort config =
    Maybe.withDefault defaultAppTitleShort config.appTitleShort


getPrimaryColor : LookAndFeelConfig -> Color
getPrimaryColor config =
    Maybe.withDefault defaultPrimaryColor config.primaryColor


getIllustrationsColor : LookAndFeelConfig -> Color
getIllustrationsColor config =
    Maybe.withDefault defaultIllustrationsColor config.illustrationsColor


anyColorSet : LookAndFeelConfig -> Bool
anyColorSet config =
    Maybe.isJust config.primaryColor || Maybe.isJust config.illustrationsColor


getLogoUrl : LookAndFeelConfig -> String
getLogoUrl config =
    Maybe.withDefault defaultLogoUrl config.logoUrl



-- JSON


decoder : Decoder LookAndFeelConfig
decoder =
    D.succeed LookAndFeelConfig
        |> D.required "appTitle" (D.maybe D.string)
        |> D.required "appTitleShort" (D.maybe D.string)
        |> D.required "customMenuLinks" (D.list CustomMenuLink.decoder)
        |> D.required "illustrationsColor" (D.maybe D.hexColor)
        |> D.required "logoUrl" (D.maybe D.string)
        |> D.required "primaryColor" (D.maybe D.hexColor)
