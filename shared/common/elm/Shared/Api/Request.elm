module Shared.Api.Request exposing
    ( ServerInfo
    , ToMsg
    , authorizationHeaders
    , authorizedUrl
    , createRequest
    , delete
    , expectJson
    , expectMetadataAndJson
    , expectWhatever
    , get
    , getString
    , post
    , postAsString
    , postEmpty
    , postEmptyBody
    , postFile
    , postFileWithData
    , postFileWithDataWhatever
    , postMultiPart
    , postWhatever
    , put
    , putEmpty
    , putEmptyBody
    , putFile
    , putString
    , putWhatever
    )

import File exposing (File)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Data.ApiError as ApiError exposing (ApiError(..))


type alias ServerInfo =
    { apiUrl : String
    , token : Maybe String
    }


type alias ToMsg a msg =
    Result ApiError a -> msg


get : ServerInfo -> String -> Decoder a -> ToMsg a msg -> Cmd msg
get serverInfo url decoder toMsg =
    createRequest "GET"
        serverInfo
        { url = url
        , expect = expectJson toMsg decoder
        , body = Http.emptyBody
        }


getString : ServerInfo -> String -> ToMsg String msg -> Cmd msg
getString serverInfo url toMsg =
    createRequest "GET"
        serverInfo
        { url = url
        , expect = expectString toMsg
        , body = Http.emptyBody
        }


post : ServerInfo -> String -> Decoder a -> E.Value -> ToMsg a msg -> Cmd msg
post serverInfo url decoder body toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


postAsString : ServerInfo -> String -> E.Value -> ToMsg String msg -> Cmd msg
postAsString serverInfo url body toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectString toMsg
        }


postFile : ServerInfo -> String -> File -> ToMsg () msg -> Cmd msg
postFile serverInfo url file toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.multipartBody [ Http.filePart "file" file ]
        , expect = expectWhatever toMsg
        }


postFileWithData : ServerInfo -> String -> File -> List Http.Part -> Decoder a -> ToMsg a msg -> Cmd msg
postFileWithData serverInfo url file data decoder toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.multipartBody (Http.filePart "file" file :: data)
        , expect = expectJson toMsg decoder
        }


postFileWithDataWhatever : ServerInfo -> String -> File -> List Http.Part -> ToMsg () msg -> Cmd msg
postFileWithDataWhatever serverInfo url file data toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.multipartBody (Http.filePart "file" file :: data)
        , expect = expectWhatever toMsg
        }


postMultiPart : ServerInfo -> String -> List Http.Part -> ToMsg () msg -> Cmd msg
postMultiPart serverInfo url data toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.multipartBody data
        , expect = expectWhatever toMsg
        }


postEmptyBody : ServerInfo -> String -> Decoder a -> ToMsg a msg -> Cmd msg
postEmptyBody serverInfo url decoder toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


postEmpty : ServerInfo -> String -> ToMsg () msg -> Cmd msg
postEmpty serverInfo url toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = expectWhatever toMsg
        }


postWhatever : ServerInfo -> String -> E.Value -> ToMsg () msg -> Cmd msg
postWhatever serverInfo url body toMsg =
    createRequest "POST"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


put : ServerInfo -> String -> Decoder a -> E.Value -> ToMsg a msg -> Cmd msg
put serverInfo url decoder body toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectJson toMsg decoder
        }


putString : ServerInfo -> String -> String -> String -> ToMsg () msg -> Cmd msg
putString serverInfo url contentType body toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.stringBody contentType body
        , expect = expectWhatever toMsg
        }


putFile : ServerInfo -> String -> File -> ToMsg () msg -> Cmd msg
putFile serverInfo url file toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.multipartBody [ Http.filePart "file" file ]
        , expect = expectWhatever toMsg
        }


putWhatever : ServerInfo -> String -> E.Value -> ToMsg () msg -> Cmd msg
putWhatever serverInfo url body toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.jsonBody body
        , expect = expectWhatever toMsg
        }


putEmpty : ServerInfo -> String -> ToMsg () msg -> Cmd msg
putEmpty serverInfo url toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = expectWhatever toMsg
        }


putEmptyBody : ServerInfo -> String -> Decoder a -> ToMsg a msg -> Cmd msg
putEmptyBody serverInfo url decoder toMsg =
    createRequest "PUT"
        serverInfo
        { url = url
        , body = Http.emptyBody
        , expect = expectJson toMsg decoder
        }


delete : ServerInfo -> String -> ToMsg () msg -> Cmd msg
delete serverInfo url toMsg =
    createRequest "DELETE"
        serverInfo
        { url = url
        , expect = expectWhatever toMsg
        , body = Http.emptyBody
        }


createRequest : String -> ServerInfo -> { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
createRequest method serverInfo { url, body, expect } =
    let
        options =
            { method = method
            , headers = authorizationHeaders serverInfo
            , url = serverInfo.apiUrl ++ url
            , body = body
            , expect = expect
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request options


authorizationHeaders : ServerInfo -> List Http.Header
authorizationHeaders { token } =
    case token of
        Just tokenString ->
            [ Http.header "Authorization" ("Bearer " ++ tokenString) ]

        Nothing ->
            []


authorizedUrl : ServerInfo -> String -> String
authorizedUrl serverInfo url =
    let
        token =
            case serverInfo.token of
                Just tokenString ->
                    "?" ++ "Authorization=Bearer%20" ++ tokenString

                Nothing ->
                    ""
    in
    serverInfo.apiUrl ++ url ++ token


expectJson : ToMsg a msg -> Decoder a -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        resolve <|
            \string ->
                Result.mapError D.errorToString (D.decodeString decoder string)


expectString : ToMsg String msg -> Http.Expect msg
expectString toMsg =
    Http.expectStringResponse toMsg <|
        resolve Ok


expectWhatever : ToMsg () msg -> Http.Expect msg
expectWhatever toMsg =
    Http.expectStringResponse toMsg <|
        resolve <|
            \_ -> Ok ()


expectMetadataAndJson : ToMsg ( Http.Metadata, Maybe a ) msg -> Decoder a -> Http.Expect msg
expectMetadataAndJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err OtherError

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (BadStatus metadata.statusCode body)

                Http.GoodStatus_ metadata body ->
                    if metadata.statusCode == 200 then
                        D.decodeString decoder body
                            |> Result.mapError (always OtherError)
                            |> Result.map (\data -> ( metadata, Just data ))

                    else
                        Ok ( metadata, Nothing )


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
            --Result.mapError (always ApiError.OtherError) (Debug.log "result" <| toResult body)
            Result.mapError (always ApiError.OtherError) (toResult body)
