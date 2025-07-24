module Wizard.Api.Models.TypeHintTestResponse exposing
    ( ErrorData
    , Request
    , Response(..)
    , ResponseData
    , TypeHintTestResponse
    , decoder
    , encode
    , getSuggestedItemProperties
    , getSuggestedListFieldProperties
    , supportedContentType
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Json.Value as JsonValue exposing (JsonValue)
import Maybe.Extra as Maybe
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)


type alias TypeHintTestResponse =
    { request : Request
    , response : Response
    }


decoder : Decoder TypeHintTestResponse
decoder =
    D.succeed TypeHintTestResponse
        |> D.required "request" decodeRequest
        |> D.required "response" decodeResponse


encode : TypeHintTestResponse -> E.Value
encode { request, response } =
    E.object
        [ ( "request", encodeRequest request )
        , ( "response", encodeResponse response )
        ]


type alias Request =
    { method : String
    , url : String
    , headers : List KeyValuePair
    , body : Maybe String
    }


decodeRequest : Decoder Request
decodeRequest =
    D.succeed Request
        |> D.required "method" D.string
        |> D.required "url" D.string
        |> D.required "headers" (D.list KeyValuePair.decoder)
        |> D.required "body" (D.maybe D.string)


encodeRequest : Request -> E.Value
encodeRequest { method, url, headers, body } =
    E.object
        [ ( "method", E.string method )
        , ( "url", E.string url )
        , ( "headers", E.list KeyValuePair.encode headers )
        , ( "body", E.maybe E.string body )
        ]


type Response
    = SuccessTypeHintResponse ResponseData
    | RemoteErrorTypeHintResponse ResponseData
    | RequestFailedTypeHintResponse ErrorData


decodeResponse : Decoder Response
decodeResponse =
    let
        decodeType =
            D.field "responseType" D.string
    in
    D.oneOf
        [ D.when decodeType ((==) "SuccessTypeHintResponse") (D.map SuccessTypeHintResponse decodeResponseData)
        , D.when decodeType ((==) "RemoteErrorTypeHintResponse") (D.map RemoteErrorTypeHintResponse decodeResponseData)
        , D.when decodeType ((==) "RequestFailedTypeHintResponse") (D.map RequestFailedTypeHintResponse decodeErrorData)
        ]


encodeResponse : Response -> E.Value
encodeResponse response =
    case response of
        SuccessTypeHintResponse data ->
            E.object <|
                ( "responseType", E.string "SuccessTypeHintResponse" )
                    :: encodeResponseData data

        RemoteErrorTypeHintResponse data ->
            E.object <|
                ( "responseType", E.string "RemoteErrorTypeHintResponse" )
                    :: encodeResponseData data

        RequestFailedTypeHintResponse errorData ->
            E.object <|
                ( "responseType", E.string "RequestFailedTypeHintResponse" )
                    :: encodeErrorData errorData


type alias ResponseData =
    { status : Int
    , contentType : String
    , body : String
    , bodyJson : Maybe JsonValue
    }


decodeResponseData : Decoder ResponseData
decodeResponseData =
    let
        jsonDecoder =
            D.map (Result.toMaybe << D.decodeString JsonValue.decoder) D.string
    in
    D.succeed ResponseData
        |> D.required "status" D.int
        |> D.required "contentType" D.string
        |> D.required "body" D.string
        |> D.required "body" jsonDecoder


encodeResponseData : ResponseData -> List ( String, E.Value )
encodeResponseData { status, contentType, body } =
    [ ( "status", E.int status )
    , ( "contentType", E.string contentType )
    , ( "body", E.string body )
    ]


supportedContentType : ResponseData -> Bool
supportedContentType { contentType } =
    String.contains "application/json" contentType || String.contains "+json" contentType


getSuggestedListFieldProperties : ResponseData -> List String
getSuggestedListFieldProperties { bodyJson } =
    let
        -- Accumulate the path as a list of segments.
        go : List String -> JsonValue -> List String
        go prefix json =
            case json of
                JsonValue.ObjectValue fields ->
                    fields
                        |> List.concatMap
                            (\( key, value ) ->
                                let
                                    path =
                                        prefix ++ [ key ]
                                in
                                case value of
                                    -- Emit the full dotted path when we reach an array
                                    JsonValue.ArrayValue _ ->
                                        [ String.join "." path ]

                                    -- Otherwise keep walking
                                    _ ->
                                        go path value
                            )

                -- Non-objects can't contain arrays under named keys
                _ ->
                    []
    in
    List.sort (Maybe.unwrap [] (go []) bodyJson)


getSuggestedItemProperties : String -> ResponseData -> List String
getSuggestedItemProperties itemListField { bodyJson } =
    case bodyJson of
        Just jsonValue ->
            let
                path =
                    if String.isEmpty itemListField then
                        []

                    else
                        String.split "." itemListField
            in
            case JsonValue.getIn (path ++ [ "0" ]) jsonValue of
                Ok (JsonValue.ObjectValue object) ->
                    List.map Tuple.first object

                _ ->
                    []

        Nothing ->
            []


type alias ErrorData =
    { message : String
    }


decodeErrorData : Decoder ErrorData
decodeErrorData =
    D.succeed ErrorData
        |> D.required "message" D.string


encodeErrorData : ErrorData -> List ( String, E.Value )
encodeErrorData { message } =
    [ ( "message", E.string message )
    ]
