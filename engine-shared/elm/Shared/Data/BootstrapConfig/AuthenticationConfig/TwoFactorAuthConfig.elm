module Shared.Data.BootstrapConfig.AuthenticationConfig.TwoFactorAuthConfig exposing
    ( TwoFactorAuthConfig(..)
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)


type TwoFactorAuthConfig
    = TwoFactorAuthConfigEnabled
    | TwoFactorAuthConfigDisabled


default : TwoFactorAuthConfig
default =
    TwoFactorAuthConfigDisabled


decoder : Decoder TwoFactorAuthConfig
decoder =
    D.field "enabled" D.bool
        |> D.map
            (\enabled ->
                if enabled then
                    TwoFactorAuthConfigEnabled

                else
                    TwoFactorAuthConfigDisabled
            )
