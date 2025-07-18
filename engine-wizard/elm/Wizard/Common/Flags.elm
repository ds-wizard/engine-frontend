module Wizard.Common.Flags exposing
    ( Flags
    , decoder
    , default
    )

import Gettext
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Auth.Session as Session exposing (Session)
import Shared.Common.Navigator as Navigator exposing (Navigator)
import Wizard.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Wizard.Common.GuideLinks as GuideLinks exposing (GuideLinks)


type alias Flags =
    { session : Maybe Session
    , seed : Int
    , apiUrl : String
    , clientUrl : String
    , webSocketThrottleDelay : Maybe Float
    , config : BootstrapConfig
    , navigator : Navigator
    , gaEnabled : Bool
    , cookieConsent : Bool
    , locale : Gettext.Locale
    , guideLinks : GuideLinks
    , maxUploadFileSize : Maybe Int
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
        |> D.required "config" BootstrapConfig.decoder
        |> D.required "navigator" Navigator.decoder
        |> D.required "gaEnabled" D.bool
        |> D.required "cookieConsent" D.bool
        |> D.optional "locale" Gettext.localeDecoder Gettext.defaultLocale
        |> D.required "guideLinks" GuideLinks.decoder
        |> D.optional "maxUploadFileSize" (D.maybe D.int) Nothing
        |> D.hardcoded True


default : Flags
default =
    { session = Nothing
    , seed = 0
    , apiUrl = ""
    , clientUrl = ""
    , webSocketThrottleDelay = Nothing
    , config = BootstrapConfig.default
    , navigator = Navigator.default
    , gaEnabled = False
    , cookieConsent = False
    , locale = Gettext.defaultLocale
    , guideLinks = GuideLinks.default
    , maxUploadFileSize = Nothing
    , success = False
    }
