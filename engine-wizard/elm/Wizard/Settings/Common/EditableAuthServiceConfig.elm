module Wizard.Settings.Common.EditableAuthServiceConfig exposing
    ( EditableAuthServiceConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias EditableAuthServiceConfig =
    { id : String
    , name : String
    , url : String
    , clientId : String
    , clientSecret : String
    , style : ButtonStyle
    }


type alias ButtonStyle =
    { background : Maybe String
    , color : Maybe String
    , icon : Maybe String
    }


decoder : Decoder EditableAuthServiceConfig
decoder =
    D.succeed EditableAuthServiceConfig
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "url" D.string
        |> D.required "clientId" D.string
        |> D.required "clientSecret" D.string
        |> D.required "style" buttonStyleDecoder


buttonStyleDecoder : Decoder ButtonStyle
buttonStyleDecoder =
    D.succeed ButtonStyle
        |> D.required "background" (D.maybe D.string)
        |> D.required "color" (D.maybe D.string)
        |> D.required "icon" (D.maybe D.string)


encode : EditableAuthServiceConfig -> E.Value
encode config =
    E.object
        [ ( "id", E.string config.id )
        , ( "name", E.string config.name )
        , ( "url", E.string config.url )
        , ( "clientId", E.string config.clientId )
        , ( "clientSecret", E.string config.clientSecret )
        , ( "parameters", E.list E.string [] )
        , ( "style", encodeButtonStyle config.style )
        ]


encodeButtonStyle : ButtonStyle -> E.Value
encodeButtonStyle style =
    E.object
        [ ( "background", E.maybe E.string style.background )
        , ( "color", E.maybe E.string style.color )
        , ( "icon", E.maybe E.string style.icon )
        ]
