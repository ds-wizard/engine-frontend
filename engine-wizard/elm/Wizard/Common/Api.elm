module Wizard.Common.Api exposing
    ( ToMsg
    , applyResult
    , applyResultCmd
    , applyResultTransform
    , applyResultTransformCmd
    , getResultCmd
    , httpFetch
    , httpGet
    , httpPost
    , httpPut
    , jwtDelete
    , jwtFetch
    , jwtFetchEmpty
    , jwtGet
    , jwtGetWithTracker
    , jwtPost
    , jwtPostEmpty
    , jwtPostString
    , jwtPut
    )

import ActionResult exposing (ActionResult(..))
import Http
import Json.Decode as Decode exposing (..)
import Jwt.Http
import Shared.Error.ApiError as ApiError exposing (ApiError(..))
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Utils exposing (dispatch)


type alias ToMsg a msg =
    Result ApiError a -> msg


jwtGet : String -> Decoder a -> AppState -> ToMsg a msg -> Cmd msg
jwtGet url decoder appState toMsg =
    Jwt.Http.get appState.session.token
        { url = appState.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


jwtGetWithTracker : String -> String -> Decoder a -> AppState -> ToMsg a msg -> Cmd msg
jwtGetWithTracker tracker url decoder appState toMsg =
    let
        options =
            { method = "GET"
            , headers = [ Http.header "Authorization" ("Bearer " ++ appState.session.token) ]
            , url = appState.apiUrl ++ url
            , body = Http.emptyBody
            , expect = expectJson toMsg decoder
            , timeout = Nothing
            , tracker = Just tracker
            }
    in
    Http.request options


jwtPost : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
jwtPost url body appState toMsg =
    Jwt.Http.post appState.session.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


jwtPostString : String -> String -> String -> AppState -> ToMsg () msg -> Cmd msg
jwtPostString url mime contents appState toMsg =
    Jwt.Http.post appState.session.token
        { url = appState.apiUrl ++ url
        , body = Http.stringBody mime contents
        , expect = expectWhatever toMsg
        }


jwtPostEmpty : String -> AppState -> ToMsg () msg -> Cmd msg
jwtPostEmpty url appState toMsg =
    Jwt.Http.post appState.session.token
        { url = appState.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectWhatever toMsg
        }


jwtFetch : String -> Decoder a -> Value -> AppState -> ToMsg a msg -> Cmd msg
jwtFetch url decoder body appState toMsg =
    Jwt.Http.post appState.session.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


jwtFetchEmpty : String -> Decoder a -> AppState -> ToMsg a msg -> Cmd msg
jwtFetchEmpty url decoder appState toMsg =
    Jwt.Http.post appState.session.token
        { url = appState.apiUrl ++ url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


jwtPut : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
jwtPut url body appState toMsg =
    Jwt.Http.put appState.session.token
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


jwtDelete : String -> AppState -> ToMsg () msg -> Cmd msg
jwtDelete url appState toMsg =
    Jwt.Http.delete appState.session.token
        { url = appState.apiUrl ++ url
        , expect = expectWhatever toMsg
        }


httpGet : String -> Decoder a -> AppState -> ToMsg a msg -> Cmd msg
httpGet url decoder appState toMsg =
    Http.get
        { url = appState.apiUrl ++ url
        , expect = expectJson toMsg decoder
        }


httpPost : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
httpPost url body appState toMsg =
    Http.post
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


httpFetch : String -> Decoder a -> Value -> AppState -> ToMsg a msg -> Cmd msg
httpFetch url decoder body appState toMsg =
    Http.post
        { url = appState.apiUrl ++ url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


httpPut : String -> Value -> AppState -> ToMsg () msg -> Cmd msg
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


getResultCmd : Result ApiError a -> Cmd Wizard.Msgs.Msg
getResultCmd result =
    case result of
        Ok _ ->
            Cmd.none

        Err error ->
            case error of
                BadStatus 401 _ ->
                    dispatch <| Wizard.Msgs.AuthMsg Wizard.Auth.Msgs.Logout

                _ ->
                    Cmd.none


applyResult :
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResult { setResult, defaultError, model, result } =
    applyResultTransform
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = identity
        }


applyResultTransform :
    { setResult : ActionResult data2 -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data1
    , transform : data1 -> data2
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultTransform { setResult, defaultError, model, result, transform } =
    applyResultTransformCmd
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = transform
        , cmd = Cmd.none
        }


applyResultCmd :
    { setResult : ActionResult data -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data
    , cmd : Cmd Wizard.Msgs.Msg
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultCmd { setResult, defaultError, model, result, cmd } =
    applyResultTransformCmd
        { setResult = setResult
        , defaultError = defaultError
        , model = model
        , result = result
        , transform = identity
        , cmd = cmd
        }


applyResultTransformCmd :
    { setResult : ActionResult data2 -> model -> model
    , defaultError : String
    , model : model
    , result : Result ApiError data1
    , transform : data1 -> data2
    , cmd : Cmd Wizard.Msgs.Msg
    }
    -> ( model, Cmd Wizard.Msgs.Msg )
applyResultTransformCmd { setResult, defaultError, model, result, transform, cmd } =
    case result of
        Ok data ->
            ( setResult (Success <| transform data) model
            , cmd
            )

        Err error ->
            ( setResult (ApiError.toActionResult defaultError error) model
            , getResultCmd result
            )


expectJson : ToMsg a msg -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        resolve <|
            \string ->
                Result.mapError Decode.errorToString (Decode.decodeString decoder string)


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
