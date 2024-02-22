module Registry2.Api.Requests exposing (get)

import Http
import Json.Decode as D exposing (Decoder)
import Registry2.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)
import Shared.Error.ApiError as ApiError exposing (ApiError)


get : AppState -> String -> Decoder a -> ToMsg a msg -> Cmd msg
get serverInfo url decoder toMsg =
    createRequest "GET"
        serverInfo
        { url = url
        , expect = expectJson toMsg decoder
        , body = Http.emptyBody
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
