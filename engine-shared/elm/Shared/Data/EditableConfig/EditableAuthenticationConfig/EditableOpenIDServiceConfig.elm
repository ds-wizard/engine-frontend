module Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing
    ( EditableOpenIDServiceConfig
    , Parameter
    , Style
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias EditableOpenIDServiceConfig =
    { id : String
    , name : String
    , url : String
    , clientId : String
    , clientSecret : String
    , parameters : List Parameter
    , style : Style
    }


type alias Parameter =
    { name : String
    , value : String
    }


type alias Style =
    { background : Maybe String
    , color : Maybe String
    , icon : Maybe String
    }


decoder : Decoder EditableOpenIDServiceConfig
decoder =
    D.succeed EditableOpenIDServiceConfig
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "url" D.string
        |> D.required "clientId" D.string
        |> D.required "clientSecret" D.string
        |> D.required "parameteres" (D.list parameterDecoder)
        |> D.required "style" styleDecoder


parameterDecoder : Decoder Parameter
parameterDecoder =
    D.succeed Parameter
        |> D.required "name" D.string
        |> D.required "value" D.string


styleDecoder : Decoder Style
styleDecoder =
    D.succeed Style
        |> D.required "background" (D.maybe D.string)
        |> D.required "color" (D.maybe D.string)
        |> D.required "icon" (D.maybe D.string)


encode : EditableOpenIDServiceConfig -> E.Value
encode config =
    E.object
        [ ( "id", E.string config.id )
        , ( "name", E.string config.name )
        , ( "url", E.string config.url )
        , ( "clientId", E.string config.clientId )
        , ( "clientSecret", E.string config.clientSecret )
        , ( "parameteres", E.list encodeParameter config.parameters )
        , ( "style", encodeStyle config.style )
        ]


encodeParameter : Parameter -> E.Value
encodeParameter parameter =
    E.object
        [ ( "name", E.string parameter.name )
        , ( "value", E.string parameter.value )
        ]


encodeStyle : Style -> E.Value
encodeStyle style =
    E.object
        [ ( "background", E.maybe E.string style.background )
        , ( "color", E.maybe E.string style.color )
        , ( "icon", E.maybe E.string style.icon )
        ]
