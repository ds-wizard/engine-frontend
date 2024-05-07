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
import Shared.Data.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Shared.Provisioning as Provisioning exposing (Provisioning)
import Wizard.Common.GuideLinks as GuideLinks exposing (GuideLinks)


type alias Flags =
    { session : Maybe Session
    , seed : Int
    , apiUrl : String
    , clientUrl : String
    , webSocketThrottleDelay : Maybe Float
    , config : BootstrapConfig
    , provisioning : Provisioning
    , localProvisioning : Provisioning
    , navigator : Navigator
    , gaEnabled : Bool
    , cookieConsent : Bool
    , locale : Gettext.Locale
    , selectedLocale : Maybe String
    , guideLinks : GuideLinks
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
        |> D.optional "provisioning" Provisioning.decoder Provisioning.default
        |> D.optional "localProvisioning" Provisioning.decoder Provisioning.default
        |> D.required "navigator" Navigator.decoder
        |> D.required "gaEnabled" D.bool
        |> D.required "cookieConsent" D.bool
        |> D.optional "locale" Gettext.localeDecoder Gettext.defaultLocale
        |> D.required "selectedLocale" (D.nullable D.string)
        |> D.required "guideLinks" GuideLinks.decoder
        |> D.hardcoded True


default : Flags
default =
    { session = Nothing
    , seed = 0
    , apiUrl = ""
    , clientUrl = ""
    , webSocketThrottleDelay = Nothing
    , config = BootstrapConfig.default
    , provisioning = Provisioning.default
    , localProvisioning = Provisioning.default
    , navigator = Navigator.default
    , gaEnabled = False
    , cookieConsent = False
    , locale = Gettext.defaultLocale
    , selectedLocale = Nothing
    , guideLinks = GuideLinks.default
    , success = False
    }
