module Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig exposing
    ( LookAndFeelConfig
    , decoder
    , default
    , defaultAppTitle
    , defaultAppTitleShort
    , defaultLogoUrl
    , defaultMenuTitle
    , defaultRegistryName
    , defaultRegistryUrl
    , getAppTitle
    , getAppTitleShort
    , getLogoUrl
    , getTheme
    , isCustomTheme
    )

import Color exposing (Color)
import Color.Convert as Convert
import Json.Decode as D exposing (Decoder)
import Json.Decode.Color as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.Utils.Theme exposing (Theme)
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)


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


defaultPrimaryColorString : String
defaultPrimaryColorString =
    "{defaultPrimaryColor}"


defaultPrimaryColor : Color
defaultPrimaryColor =
    Color.rgb255 0 51 170


defaultIllustrationsColorString : String
defaultIllustrationsColorString =
    "{defaultIllustrationsColor}"


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
    config.primaryColor
        |> Maybe.orElse (Result.toMaybe (Convert.hexToColor defaultPrimaryColorString))
        |> Maybe.withDefault defaultPrimaryColor


getIllustrationsColor : LookAndFeelConfig -> Color
getIllustrationsColor config =
    config.illustrationsColor
        |> Maybe.orElse (Result.toMaybe (Convert.hexToColor defaultIllustrationsColorString))
        |> Maybe.withDefault defaultIllustrationsColor


isCustomTheme : LookAndFeelConfig -> Bool
isCustomTheme config =
    let
        isDefaultPrimaryColor =
            getPrimaryColor config == defaultPrimaryColor

        isDefaultIllustrationsColor =
            getIllustrationsColor config == defaultIllustrationsColor
    in
    not (isDefaultPrimaryColor && isDefaultIllustrationsColor)


getTheme : LookAndFeelConfig -> Theme
getTheme config =
    Theme
        (getPrimaryColor config)
        (getIllustrationsColor config)


getLogoUrl : LookAndFeelConfig -> String
getLogoUrl config =
    Maybe.withDefault defaultLogoUrl config.logoUrl


defaultRegistryName : String
defaultRegistryName =
    "{defaultRegistryName}"


defaultRegistryUrl : String
defaultRegistryUrl =
    "{defaultRegistryUrl}"


defaultMenuTitle : String
defaultMenuTitle =
    "{defaultMenuTitle}"



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
