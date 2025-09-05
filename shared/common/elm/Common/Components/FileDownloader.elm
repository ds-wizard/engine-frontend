module Common.Components.FileDownloader exposing
    ( Msg
    , fetchFile
    , update
    )

import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request exposing (ServerInfo)
import Common.Data.ApiError exposing (ApiError)
import Common.Ports.File as File
import Http


type Msg
    = GotFileUrl (Result ApiError UrlResponse)


fetchFile : ServerInfo -> String -> Cmd Msg
fetchFile serverInfo fileUrl =
    Request.get serverInfo fileUrl UrlResponse.decoder GotFileUrl



--Http.request
--    { method = "GET"
--    , headers = Request.authorizationHeaders serverInfo
--    , url = fileUrl
--    , body = Http.emptyBody
--    , expect = Request.expectJson GotFileUrl UrlResponse.decoder
--    , timeout = Nothing
--    , tracker = Nothing
--    }


update : Msg -> Cmd Msg
update msg =
    case msg of
        GotFileUrl result ->
            case result of
                Ok urlResponse ->
                    File.downloadFile urlResponse.url

                Err _ ->
                    Cmd.none
