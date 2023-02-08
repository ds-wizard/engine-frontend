module Shared.Data.EditableConfig.EditableAuthenticationConfig exposing
    ( EditableAuthenticationConfig
    , External
    , Internal
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig as EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Data.EditableConfig.EditableTwoFactorAuthConfig as EditableTwoFactorAuthConfig exposing (EditableTwoFactorAuthConfig)


type alias EditableAuthenticationConfig =
    { defaultRole : String
    , external : External
    , internal : Internal
    }


type alias External =
    { services : List EditableOpenIDServiceConfig }


type alias Internal =
    { registration : SimpleFeatureConfig
    , twoFactorAuth : EditableTwoFactorAuthConfig
    }


decoder : Decoder EditableAuthenticationConfig
decoder =
    D.succeed EditableAuthenticationConfig
        |> D.required "defaultRole" D.string
        |> D.required "external" externalDecoder
        |> D.required "internal" internalDecoder


externalDecoder : Decoder External
externalDecoder =
    D.succeed External
        |> D.required "services" (D.list EditableOpenIDServiceConfig.decoder)


internalDecoder : Decoder Internal
internalDecoder =
    D.succeed Internal
        |> D.required "registration" SimpleFeatureConfig.decoder
        |> D.required "twoFactorAuth" EditableTwoFactorAuthConfig.decoder


encode : EditableAuthenticationConfig -> E.Value
encode config =
    E.object
        [ ( "defaultRole", E.string config.defaultRole )
        , ( "external", encodeExternal config.external )
        , ( "internal", encodeInternal config.internal )
        ]


encodeExternal : External -> E.Value
encodeExternal external =
    E.object
        [ ( "services", E.list EditableOpenIDServiceConfig.encode external.services ) ]


encodeInternal : Internal -> E.Value
encodeInternal internal =
    E.object
        [ ( "registration", SimpleFeatureConfig.encode internal.registration )
        , ( "twoFactorAuth", EditableTwoFactorAuthConfig.encode internal.twoFactorAuth )
        ]
