module Common.Api.Models.BootstrapConfig.LookAndFeelConfig exposing
    ( LookAndFeelConfig
    , decoder
    , default
    , defaultLogoUrl
    , getAppTitle
    , getLogoUrl
    , getPrimaryColor
    , isCustomTheme
    , withDefaultAppTitle
    )

import Color exposing (Color)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Color as D
import Json.Decode.Pipeline as D


type alias LookAndFeelConfig =
    { appTitle : Maybe String
    , logoUrl : Maybe String
    , primaryColor : Maybe Color

    -- What the instance is called when it has no title of its own. It is the
    -- application's to say (`withDefaultAppTitle`), not the server's.
    , defaultAppTitle : String
    }


default : LookAndFeelConfig
default =
    { appTitle = Nothing
    , logoUrl = Nothing
    , primaryColor = Nothing
    , defaultAppTitle = ""
    }


withDefaultAppTitle : String -> LookAndFeelConfig -> LookAndFeelConfig
withDefaultAppTitle defaultAppTitle config =
    { config | defaultAppTitle = defaultAppTitle }


defaultPrimaryColor : Color
defaultPrimaryColor =
    Color.rgb255 0 51 170


defaultLogoUrl : String
defaultLogoUrl =
    "/assets/logo.svg"


getAppTitle : LookAndFeelConfig -> String
getAppTitle config =
    Maybe.withDefault config.defaultAppTitle config.appTitle


getPrimaryColor : LookAndFeelConfig -> Color
getPrimaryColor config =
    Maybe.withDefault defaultPrimaryColor config.primaryColor


{-| A color of the tenant's own, to derive the theme's roles from. The
illustrations no longer follow it: a drawing is a file with its colors in it
(`Ui.Illustration`), so only the primary color is left to be custom.
-}
isCustomTheme : LookAndFeelConfig -> Bool
isCustomTheme config =
    getPrimaryColor config /= defaultPrimaryColor


getLogoUrl : LookAndFeelConfig -> String
getLogoUrl config =
    Maybe.withDefault defaultLogoUrl config.logoUrl



-- JSON


decoder : Decoder LookAndFeelConfig
decoder =
    D.succeed LookAndFeelConfig
        |> D.required "appTitle" (D.nullable D.string)
        |> D.required "logoUrl" (D.nullable D.string)
        |> D.required "primaryColor" (D.nullable D.hexColor)
        |> D.hardcoded ""
