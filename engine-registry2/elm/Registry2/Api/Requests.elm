module Registry2.Api.Requests exposing (get, postWhatever, put, putEmpty)

import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Registry2.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)
import Shared.Error.ApiError as ApiError exposing (ApiError)


get : AppState -> String -> Decoder a -> ToMsg a msg -> Cmd msg
get appState url decoder toMsg =
    createRequest "GET"
        appState
        { url = url
        , expect = expectJson toMsg decoder
        , body = Http.emptyBody
        }


postWhatever : AppState -> String -> E.Value -> ToMsg () msg -> Cmd msg
postWhatever appState url body toMsg =
    createRequest "POST"
        appState
        { url = url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


put : AppState -> String -> Decoder a -> E.Value -> ToMsg a msg -> Cmd msg
put appState url decoder body toMsg =
    createRequest "PUT"
        appState
        { url = url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


putEmpty : AppState -> String -> Decoder a -> ToMsg a msg -> Cmd msg
putEmpty appState url decoder toMsg =
    createRequest "PUT"
        appState
        { url = url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


createRequest : String -> AppState -> { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
createRequest method appState { url, body, expect } =
    let
        headers =
            case appState.session of
                Just session ->
                    [ Http.header "Authorization" ("Bearer " ++ session.token) ]

                Nothing ->
                    []

        options =
            { method = method
            , headers = headers
            , url = appState.apiUrl ++ url
            , body = body
            , expect = expect
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request options


expectJson : ToMsg a msg -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        resolve <|
            \string ->
                Result.mapError D.errorToString (D.decodeString decoder string)


expectWhatever : ToMsg () msg -> Http.Expect msg
expectWhatever toMsg =
    Http.expectStringResponse toMsg <|
        resolve <|
            \_ -> Ok ()


resolve : (String -> Result String a) -> Http.Response String -> Result ApiError a
resolve toResult response =
    case response of
        Http.BadUrl_ _ ->
            Err ApiError.OtherError

        Http.Timeout_ ->
            Err ApiError.Timeout

        Http.NetworkError_ ->
            Err ApiError.NetworkError

        Http.BadStatus_ metadata body ->
            Err (ApiError.BadStatus metadata.statusCode body)

        Http.GoodStatus_ _ body ->
            Result.mapError (always ApiError.OtherError) (toResult body)
