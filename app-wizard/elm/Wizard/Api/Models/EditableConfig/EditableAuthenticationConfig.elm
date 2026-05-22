module Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig exposing
    ( EditableAuthenticationConfig
    , Internal
    , decoder
    , encode
    )

import Common.Data.Role as Role exposing (Role)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Api.Models.EditableConfig.EditableTwoFactorAuthConfig as EditableTwoFactorAuthConfig exposing (EditableTwoFactorAuthConfig)


type alias EditableAuthenticationConfig =
    { defaultRole : Role
    , internal : Internal
    }


type alias Internal =
    { registration : SimpleFeatureConfig
    , twoFactorAuth : EditableTwoFactorAuthConfig
    , nonAdminLoginEnabled : Bool
    , sessionExpiration : Int
    , userEmailLinkExpiration : Int
    }


decoder : Decoder EditableAuthenticationConfig
decoder =
    D.succeed EditableAuthenticationConfig
        |> D.required "defaultRole" Role.decoder
        |> D.required "internal" internalDecoder


internalDecoder : Decoder Internal
internalDecoder =
    D.succeed Internal
        |> D.required "registration" SimpleFeatureConfig.decoder
        |> D.required "twoFactorAuth" EditableTwoFactorAuthConfig.decoder
        |> D.required "nonAdminLoginEnabled" D.bool
        |> D.required "sessionExpiration" D.int
        |> D.required "userEmailLinkExpiration" D.int


encode : EditableAuthenticationConfig -> E.Value
encode config =
    E.object
        [ ( "defaultRole", Role.encode config.defaultRole )
        , ( "internal", encodeInternal config.internal )
        ]


encodeInternal : Internal -> E.Value
encodeInternal internal =
    E.object
        [ ( "registration", SimpleFeatureConfig.encode internal.registration )
        , ( "twoFactorAuth", EditableTwoFactorAuthConfig.encode internal.twoFactorAuth )
        , ( "nonAdminLoginEnabled", E.bool internal.nonAdminLoginEnabled )
        , ( "sessionExpiration", E.int internal.sessionExpiration )
        , ( "userEmailLinkExpiration", E.int internal.userEmailLinkExpiration )
        ]
