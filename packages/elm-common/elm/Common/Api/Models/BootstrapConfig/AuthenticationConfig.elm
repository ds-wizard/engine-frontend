module Common.Api.Models.BootstrapConfig.AuthenticationConfig exposing
    ( AuthenticationConfig
    , decoder
    , default
    )

{-| How somebody signs in, as it is known before they do: the fields of the
`authentication` settings the login and signup pages need (`internal`, as the
section has them) and the OpenID services to sign in through (`openId`).
-}

import Common.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig as OpenIDServiceConfig exposing (OpenIDServiceConfig)
import Common.Api.Models.BootstrapConfig.AuthenticationConfig.TwoFactorAuthConfig as TwoFactorAuthConfig exposing (TwoFactorAuthConfig)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AuthenticationConfig =
    { registrationEnabled : Bool
    , nonAdminLoginEnabled : Bool
    , twoFactorAuth : TwoFactorAuthConfig
    , services : List OpenIDServiceConfig
    }


default : AuthenticationConfig
default =
    { registrationEnabled = True
    , nonAdminLoginEnabled = True
    , twoFactorAuth = TwoFactorAuthConfig.default
    , services = []
    }


decoder : Decoder AuthenticationConfig
decoder =
    D.succeed AuthenticationConfig
        |> D.requiredAt [ "internal", "registrationEnabled" ] D.bool
        |> D.requiredAt [ "internal", "nonAdminLoginEnabled" ] D.bool
        |> D.requiredAt [ "internal", "twoFactorAuth" ] TwoFactorAuthConfig.decoder
        |> D.required "openId" (D.list OpenIDServiceConfig.decoder)
