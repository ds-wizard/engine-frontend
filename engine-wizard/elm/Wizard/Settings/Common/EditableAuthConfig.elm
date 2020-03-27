module Wizard.Settings.Common.EditableAuthConfig exposing (EditableAuthConfig, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Common.Config.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Settings.Common.EditableAuthServiceConfig as EditableAuthServiceConfig exposing (EditableAuthServiceConfig)


type alias EditableAuthConfig =
    { external : External
    , internal : Internal
    }


type alias External =
    { services : List EditableAuthServiceConfig }


type alias Internal =
    { registration : SimpleFeatureConfig }


decoder : Decoder EditableAuthConfig
decoder =
    D.succeed EditableAuthConfig
        |> D.required "external" externalDecoder
        |> D.required "internal" internalDecoder


externalDecoder : Decoder External
externalDecoder =
    D.succeed External
        |> D.required "services" (D.list EditableAuthServiceConfig.decoder)


internalDecoder : Decoder Internal
internalDecoder =
    D.succeed Internal
        |> D.required "registration" SimpleFeatureConfig.decoder


encode : EditableAuthConfig -> E.Value
encode config =
    E.object
        [ ( "external", encodeExternal config.external )
        , ( "internal", encodeInternal config.internal )
        ]


encodeExternal : External -> E.Value
encodeExternal external =
    E.object
        [ ( "services", E.list EditableAuthServiceConfig.encode external.services ) ]


encodeInternal : Internal -> E.Value
encodeInternal internal =
    E.object
        [ ( "registration", SimpleFeatureConfig.encode internal.registration ) ]
