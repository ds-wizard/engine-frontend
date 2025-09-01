module Wizard.Common.FileDownloader exposing
    ( Msg
    , fetchFile
    , update
    )

import Http
import Shared.Api.Request as Request
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Ports as Ports


type Msg
    = GotFileUrl (Result ApiError UrlResponse)


fetchFile : AppState -> String -> Cmd Msg
fetchFile appState fileUrl =
    Http.request
        { method = "GET"
        , headers = Request.authorizationHeaders (AppState.toServerInfo appState)
        , url = fileUrl
        , body = Http.emptyBody
        , expect = Request.expectJson GotFileUrl UrlResponse.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Cmd Msg
update msg =
    case msg of
        GotFileUrl result ->
            case result of
                Ok urlResponse ->
                    Ports.downloadFile urlResponse.url

                Err _ ->
                    Cmd.none
