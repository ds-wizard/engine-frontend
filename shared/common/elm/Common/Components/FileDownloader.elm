module Common.Components.FileDownloader exposing
    ( Msg
    , fetchFile
    , update
    )

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request exposing (ServerInfo)
import Common.Ports.File as File


type Msg
    = GotFileUrl (Result ApiError UrlResponse)


fetchFile : ServerInfo -> String -> Cmd Msg
fetchFile serverInfo fileUrl =
    Request.get serverInfo fileUrl UrlResponse.decoder GotFileUrl


update : Msg -> Cmd Msg
update msg =
    case msg of
        GotFileUrl result ->
            case result of
                Ok urlResponse ->
                    File.downloadFile urlResponse.url

                Err _ ->
                    Cmd.none
