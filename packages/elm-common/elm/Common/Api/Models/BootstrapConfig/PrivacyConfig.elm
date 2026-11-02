module Common.Api.Models.BootstrapConfig.PrivacyConfig exposing
    ( PrivacyConfig
    , decoder
    , default
    , withFallback
    )

{-| Where the privacy policy and the terms of service of the instance are. They
are the instance's, not something set in the application. The deployment may
name its own (`withFallback`), for an instance that names neither.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe


type alias PrivacyConfig =
    { privacyUrl : Maybe String
    , termsOfServiceUrl : Maybe String
    }


default : PrivacyConfig
default =
    { privacyUrl = Nothing
    , termsOfServiceUrl = Nothing
    }


{-| Each document the instance does not name, taken from `fallback`.
-}
withFallback : PrivacyConfig -> PrivacyConfig -> PrivacyConfig
withFallback fallback config =
    { privacyUrl = Maybe.or config.privacyUrl fallback.privacyUrl
    , termsOfServiceUrl = Maybe.or config.termsOfServiceUrl fallback.termsOfServiceUrl
    }


decoder : Decoder PrivacyConfig
decoder =
    D.succeed PrivacyConfig
        |> D.required "privacyUrl" (D.nullable D.string)
        |> D.required "termsOfServiceUrl" (D.nullable D.string)
