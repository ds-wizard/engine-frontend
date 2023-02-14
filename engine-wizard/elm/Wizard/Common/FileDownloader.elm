module Wizard.Common.FileDownloader exposing
    ( Msg
    , fetchFile
    , update
    )

import Http
import Shared.Api as Api
import Shared.Data.UrlResponse as UrlResponse exposing (UrlResponse)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Ports as Ports


type Msg
    = GotFileUrl (Result ApiError UrlResponse)


fetchFile : AppState -> String -> Cmd Msg
fetchFile appState fileUrl =
    Http.request
        { method = "GET"
        , headers = Api.authorizationHeaders appState
        , url = fileUrl
        , body = Http.emptyBody
        , expect = Api.expectJson GotFileUrl UrlResponse.decoder
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
