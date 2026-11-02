module Common.Data.Flags exposing
    ( Flags
    , decoder
    , default
    )

import Common.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.BootstrapConfig.PrivacyConfig as PrivacyConfig exposing (PrivacyConfig)
import Common.Data.Navigator as Navigator exposing (Navigator)
import Common.Data.Session as Session exposing (Session)
import Common.Plugins.Plugin as Plugin exposing (Plugin)
import Common.Plugins.PluginMetadata as PluginMetadata exposing (PluginMetadata)
import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)
import Gettext
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe


type alias Flags =
    { session : Maybe Session
    , seed : Int
    , apiUrl : String
    , clientUrl : String
    , webSocketThrottleDelay : Maybe Float
    , config : BootstrapConfig
    , navigator : Navigator
    , locale : Gettext.Locale
    , guideLinks : GuideLinks
    , maxUploadFileSize : Maybe Int
    , urlCheckerUrl : Maybe String
    , newsUrl : Maybe String
    , pluginMetadata : List PluginMetadata
    , plugins : List Plugin
    , sideNavWidth : Maybe Float
    , uiMode : Maybe String
    , success : Bool
    }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "session" (D.nullable Session.decoder)
        |> D.required "seed" D.int
        |> D.required "apiUrl" D.string
        |> D.required "clientUrl" D.string
        |> D.optional "webSocketThrottleDelay" (D.maybe D.float) Nothing
        |> D.custom configDecoder
        |> D.required "navigator" Navigator.decoder
        |> D.optional "locale" Gettext.localeDecoder Gettext.defaultLocale
        |> D.required "guideLinks" GuideLinks.decoder
        |> D.optional "maxUploadFileSize" (D.maybe D.int) Nothing
        |> D.required "urlCheckerUrl" (D.maybe D.string)
        |> D.required "newsUrl" (D.maybe D.string)
        |> D.required "plugins" (D.listIgnoreInvalid PluginMetadata.decoder)
        |> D.required "plugins" (D.listIgnoreInvalid Plugin.decoder)
        |> D.optional "sideNavWidth" (D.maybe D.float) Nothing
        |> D.optional "uiMode" (D.maybe D.string) Nothing
        |> D.hardcoded True


{-| The configuration, with its second part laid over it when the start-up had
it at hand (`workspaceConfig`). A second part that is there has to be read: one
that cannot be is a configuration that cannot be, not one without it.

The privacy policy and the terms of service the instance does not name are the
deployment's own (`privacyPolicyUrl`, `termsOfServiceUrl`), where it has them.

-}
configDecoder : Decoder BootstrapConfig
configDecoder =
    let
        build config mbWorkspace privacyFallback =
            let
                withPrivacy =
                    { config | privacy = PrivacyConfig.withFallback privacyFallback config.privacy }
            in
            Maybe.unwrap withPrivacy (\workspace -> BootstrapConfig.withWorkspace workspace withPrivacy) mbWorkspace
    in
    D.succeed build
        |> D.required "config" BootstrapConfig.decoder
        |> D.optional "workspaceConfig" (D.nullable BootstrapConfig.workspaceDecoder) Nothing
        |> D.custom privacyFallbackDecoder


privacyFallbackDecoder : Decoder PrivacyConfig
privacyFallbackDecoder =
    D.succeed PrivacyConfig
        |> D.required "privacyPolicyUrl" (D.nullable D.string)
        |> D.required "termsOfServiceUrl" (D.nullable D.string)


default : Flags
default =
    { session = Nothing
    , seed = 0
    , apiUrl = ""
    , clientUrl = ""
    , webSocketThrottleDelay = Nothing
    , config = BootstrapConfig.default
    , navigator = Navigator.default
    , locale = Gettext.defaultLocale
    , guideLinks = GuideLinks.fromList []
    , maxUploadFileSize = Nothing
    , urlCheckerUrl = Nothing
    , newsUrl = Nothing
    , pluginMetadata = []
    , plugins = []
    , sideNavWidth = Nothing
    , uiMode = Nothing
    , success = False
    }
