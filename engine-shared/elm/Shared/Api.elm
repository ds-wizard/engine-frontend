module Shared.Api exposing
    ( ToMsg
    , authorizedUrl
    , httpFetch
    , httpGet
    , httpPost
    , httpPut
    , jwtDelete
    , jwtFetch
    , jwtFetchEmpty
    , jwtFetchPut
    , jwtGet
    , jwtOrHttpFetch
    , jwtOrHttpGet
    , jwtOrHttpHead
    , jwtOrHttpPut
    , jwtPost
    , jwtPostEmpty
    , jwtPostFile
    , jwtPostString
    , jwtPut
    , wsUrl
    )

import File exposing (File)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Jwt.Http
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Error.ApiError exposing (ApiError(..))


type alias ToMsg a msg =
    Result ApiError a -> msg


jwtOrHttpGet : String -> Decoder a -> AbstractAppState b -> ToMsg a msg -> Cmd msg
jwtOrHttpGet url decoder appState =
    jwtOrHttp appState jwtGet httpGet url decoder appState


jwtOrHttpPut : String -> E.Value -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtOrHttpPut url body appState =
    jwtOrHttp appState jwtPut httpPut url body appState


jwtOrHttpFetch : String -> Decoder a -> E.Value -> AbstractAppState b -> ToMsg a msg -> Cmd msg
jwtOrHttpFetch url decoder body appState =
    jwtOrHttp appState jwtFetch httpFetch url decoder body appState


jwtOrHttpHead : String -> AbstractAppState b -> ToMsg Http.Metadata msg -> Cmd msg
jwtOrHttpHead url appState toMsg =
    let
        headers =
            if not <| String.isEmpty appState.session.token.token then
                [ Http.header "Authorization" ("Bearer " ++ appState.session.token.token) ]

            else
                []
    in
    Http.request
        { method = "HEAD"
        , headers = headers
        , url = appState.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectMetadata toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


jwtGet : String -> Decoder a -> AbstractAppState b -> ToMsg a msg -> Cmd msg
jwtGet url decoder appState toMsg =
    Jwt.Http.get appState.session.token.token
        { url = appState.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


jwtPost : String -> E.Value -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtPost url body appState toMsg =
    Jwt.Http.post appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


jwtPostString : String -> String -> String -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtPostString url mime contents appState toMsg =
    Jwt.Http.post appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.stringBody mime contents
        , expect = expectWhatever toMsg
        }


jwtPostFile : String -> File -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtPostFile url file appState toMsg =
    Jwt.Http.post appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.multipartBody [ Http.filePart "file" file ]
        , expect = expectWhatever toMsg
        }


jwtPostEmpty : String -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtPostEmpty url appState toMsg =
    Jwt.Http.post appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectWhatever toMsg
        }


jwtFetch : String -> Decoder a -> E.Value -> AbstractAppState b -> ToMsg a msg -> Cmd msg
jwtFetch url decoder body appState toMsg =
    Jwt.Http.post appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


jwtFetchEmpty : String -> Decoder a -> AbstractAppState b -> ToMsg a msg -> Cmd msg
jwtFetchEmpty url decoder appState toMsg =
    Jwt.Http.post appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


jwtPut : String -> E.Value -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtPut url body appState toMsg =
    Jwt.Http.put appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


jwtFetchPut : String -> Decoder a -> E.Value -> AbstractAppState b -> ToMsg a msg -> Cmd msg
jwtFetchPut url decoder body appState toMsg =
    Jwt.Http.put appState.session.token.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


jwtDelete : String -> AbstractAppState b -> ToMsg () msg -> Cmd msg
jwtDelete url appState toMsg =
    Jwt.Http.delete appState.session.token.token
        { url = appState.apiUrl ++ url
        , expect = expectWhatever toMsg
        }


httpGet : String -> Decoder a -> AbstractAppState b -> ToMsg a msg -> Cmd msg
httpGet url decoder appState toMsg =
    Http.get
        { url = appState.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


httpPost : String -> E.Value -> AbstractAppState b -> ToMsg () msg -> Cmd msg
httpPost url body appState toMsg =
    Http.post
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


httpFetch : String -> Decoder a -> E.Value -> AbstractAppState b -> ToMsg a msg -> Cmd msg
httpFetch url decoder body appState toMsg =
    Http.post
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


httpPut : String -> E.Value -> AbstractAppState b -> ToMsg () msg -> Cmd msg
httpPut url body appState toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


wsUrl : String -> AbstractAppState b -> String
wsUrl url appState =
    String.replace "http" "ws" <| authorizedUrl url appState


authorizedUrl : String -> AbstractAppState b -> String
authorizedUrl url appState =
    let
        token =
            if not <| String.isEmpty appState.session.token.token then
                "?" ++ "Authorization=Bearer%20" ++ appState.session.token.token

            else
                ""
    in
    appState.apiUrl ++ url ++ token


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


expectMetadata : ToMsg Http.Metadata msg -> Http.Expect msg
expectMetadata toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata.statusCode body)

                Http.GoodStatus_ metadata _ ->
                    Ok metadata


resolve : (String -> Result String a) -> Http.Response String -> Result ApiError a
resolve toResult response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata.statusCode body)

        Http.GoodStatus_ _ body ->
            Result.mapError BadBody (toResult body)


jwtOrHttp : AbstractAppState b -> a -> a -> a
jwtOrHttp appState jwtMethod httpMethod =
    if Session.exists appState.session then
        jwtMethod

    else
        httpMethod
