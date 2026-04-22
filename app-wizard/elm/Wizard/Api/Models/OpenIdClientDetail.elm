module Wizard.Api.Models.OpenIdClientDetail exposing
    ( OpenIdClientDetail
    , Parameter
    , decoder
    , encode
    , encodeNew
    , prefabDecoder
    )

import Common.Api.Models.AuthServiceProviderButtonStyle as AuthServiceProviderButtonStyle exposing (AuthServiceProviderButtonStyle)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias OpenIdClientDetail =
    { uuid : Uuid
    , name : String
    , url : String
    , clientId : String
    , clientSecret : String
    , parameters : List Parameter
    , registrationEnabled : Bool
    , scopeEmail : Bool
    , scopeProfile : Bool
    , style : AuthServiceProviderButtonStyle
    }


type alias Parameter =
    { name : String
    , value : String
    }


decoder : Decoder OpenIdClientDetail
decoder =
    D.succeed OpenIdClientDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "url" D.string
        |> D.required "clientId" D.string
        |> D.required "clientSecret" D.string
        |> D.required "parameters" (D.list parameterDecoder)
        |> D.required "registrationEnabled" D.bool
        |> D.required "scopeEmail" D.bool
        |> D.required "scopeProfile" D.bool
        |> D.required "style" AuthServiceProviderButtonStyle.decoder


prefabDecoder : Decoder OpenIdClientDetail
prefabDecoder =
    D.succeed OpenIdClientDetail
        |> D.hardcoded Uuid.nil
        |> D.required "name" D.string
        |> D.required "url" D.string
        |> D.required "clientId" D.string
        |> D.required "clientSecret" D.string
        |> D.required "parameters" (D.list parameterDecoder)
        |> D.required "registrationEnabled" D.bool
        |> D.required "scopeEmail" D.bool
        |> D.required "scopeProfile" D.bool
        |> D.required "style" AuthServiceProviderButtonStyle.decoder


parameterDecoder : Decoder Parameter
parameterDecoder =
    D.succeed Parameter
        |> D.required "name" D.string
        |> D.required "value" D.string


encode : OpenIdClientDetail -> E.Value
encode config =
    E.object
        [ ( "uuid", Uuid.encode config.uuid )
        , ( "name", E.string config.name )
        , ( "url", E.string config.url )
        , ( "clientId", E.string config.clientId )
        , ( "clientSecret", E.string config.clientSecret )
        , ( "parameters", E.list encodeParameter config.parameters )
        , ( "registrationEnabled", E.bool config.registrationEnabled )
        , ( "scopeEmail", E.bool config.scopeEmail )
        , ( "scopeProfile", E.bool config.scopeProfile )
        , ( "style", AuthServiceProviderButtonStyle.encode config.style )
        ]


encodeNew : OpenIdClientDetail -> E.Value
encodeNew config =
    E.object
        [ ( "name", E.string config.name )
        , ( "url", E.string config.url )
        , ( "clientId", E.string config.clientId )
        , ( "clientSecret", E.string config.clientSecret )
        , ( "registrationEnabled", E.bool config.registrationEnabled )
        , ( "scopeEmail", E.bool config.scopeEmail )
        , ( "scopeProfile", E.bool config.scopeProfile )
        , ( "parameters", E.list encodeParameter config.parameters )
        , ( "style", AuthServiceProviderButtonStyle.encode config.style )
        ]


encodeParameter : Parameter -> E.Value
encodeParameter parameter =
    E.object
        [ ( "name", E.string parameter.name )
        , ( "value", E.string parameter.value )
        ]
