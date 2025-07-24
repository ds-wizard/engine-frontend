module Wizard.Api.Models.TypeHintTestResponse exposing
    ( ErrorData
    , Request
    , Response
    , ResponseData
    , TypeHintTestResponse
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
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
            D.field "type" D.string
    in
    D.oneOf
        [ D.when decodeType ((==) "SuccessTypehintResponse") (D.map SuccessTypeHintResponse decodeResponseData)
        , D.when decodeType ((==) "RemoteErrorTypehintResponse") (D.map RemoteErrorTypeHintResponse decodeResponseData)
        , D.when decodeType ((==) "RequestFailedTypehintResponse") (D.map RequestFailedTypeHintResponse decodeErrorData)
        ]


encodeResponse : Response -> E.Value
encodeResponse response =
    case response of
        SuccessTypeHintResponse data ->
            E.object
                [ ( "type", E.string "SuccessTypehintResponse" )
                , ( "data", encodeResponseData data )
                ]

        RemoteErrorTypeHintResponse data ->
            E.object
                [ ( "type", E.string "RemoteErrorTypehintResponse" )
                , ( "data", encodeResponseData data )
                ]

        RequestFailedTypeHintResponse errorData ->
            E.object
                [ ( "type", E.string "RequestFailedTypehintResponse" )
                , ( "error", encodeErrorData errorData )
                ]


type alias ResponseData =
    { status : Int
    , contentType : String
    , body : String
    }


decodeResponseData : Decoder ResponseData
decodeResponseData =
    D.succeed ResponseData
        |> D.required "status" D.int
        |> D.required "contentType" D.string
        |> D.required "body" D.string


encodeResponseData : ResponseData -> E.Value
encodeResponseData { status, contentType, body } =
    E.object
        [ ( "status", E.int status )
        , ( "contentType", E.string contentType )
        , ( "body", E.string body )
        ]


type alias ErrorData =
    { error : String
    }


decodeErrorData : Decoder ErrorData
decodeErrorData =
    D.succeed ErrorData
        |> D.required "error" D.string


encodeErrorData : ErrorData -> E.Value
encodeErrorData { error } =
    E.object
        [ ( "error", E.string error )
        ]
