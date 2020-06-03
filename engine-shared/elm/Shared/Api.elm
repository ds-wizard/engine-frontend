module Shared.Api exposing
    ( ApiConfig
    , AppStateLike
    , ToMsg
    , httpFetch
    , httpGet
    , httpPost
    , httpPut
    , jwtDelete
    , jwtFetch
    , jwtFetchEmpty
    , jwtGet
    , jwtPost
    , jwtPostEmpty
    , jwtPostString
    , jwtPut
    )

import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Jwt.Http
import Shared.Error.ApiError exposing (ApiError(..))


type alias ToMsg a msg =
    Result ApiError a -> msg


type alias ApiConfig =
    { apiUrl : String
    , token : String
    }


type alias AppStateLike b =
    { b | apiConfig : ApiConfig }


jwtGet : String -> Decoder a -> AppStateLike b -> ToMsg a msg -> Cmd msg
jwtGet url decoder appState toMsg =
    Jwt.Http.get appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


jwtPost : String -> E.Value -> AppStateLike b -> ToMsg () msg -> Cmd msg
jwtPost url body appState toMsg =
    Jwt.Http.post appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


jwtPostString : String -> String -> String -> AppStateLike b -> ToMsg () msg -> Cmd msg
jwtPostString url mime contents appState toMsg =
    Jwt.Http.post appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.stringBody mime contents
        , expect = expectWhatever toMsg
        }


jwtPostEmpty : String -> AppStateLike b -> ToMsg () msg -> Cmd msg
jwtPostEmpty url appState toMsg =
    Jwt.Http.post appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectWhatever toMsg
        }


jwtFetch : String -> Decoder a -> E.Value -> AppStateLike b -> ToMsg a msg -> Cmd msg
jwtFetch url decoder body appState toMsg =
    Jwt.Http.post appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


jwtFetchEmpty : String -> Decoder a -> AppStateLike b -> ToMsg a msg -> Cmd msg
jwtFetchEmpty url decoder appState toMsg =
    Jwt.Http.post appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


jwtPut : String -> E.Value -> AppStateLike b -> ToMsg () msg -> Cmd msg
jwtPut url body appState toMsg =
    Jwt.Http.put appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


jwtDelete : String -> AppStateLike b -> ToMsg () msg -> Cmd msg
jwtDelete url appState toMsg =
    Jwt.Http.delete appState.apiConfig.token
        { url = appState.apiConfig.apiUrl ++ url
        , expect = expectWhatever toMsg
        }


httpGet : String -> Decoder a -> AppStateLike b -> ToMsg a msg -> Cmd msg
httpGet url decoder appState toMsg =
    Http.get
        { url = appState.apiConfig.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


httpPost : String -> E.Value -> AppStateLike b -> ToMsg () msg -> Cmd msg
httpPost url body appState toMsg =
    Http.post
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


httpFetch : String -> Decoder a -> E.Value -> AppStateLike b -> ToMsg a msg -> Cmd msg
httpFetch url decoder body appState toMsg =
    Http.post
        { url = appState.apiConfig.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


httpPut : String -> E.Value -> AppStateLike b -> ToMsg () msg -> Cmd msg
httpPut url body appState toMsg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = appState.apiConfig.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


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
