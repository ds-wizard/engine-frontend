module Wizard.Pages.Projects.FileDownload.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.ProjectFiles as ProjectFilesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Pages.Projects.FileDownload.Models exposing (Model)
import Wizard.Pages.Projects.FileDownload.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate, toUrl)


fetchData : AppState -> Uuid -> Uuid -> Cmd Msg
fetchData appState projectUuid fileUuid =
    ProjectFilesApi.getFileUrl appState projectUuid fileUuid GotFileUrlCompleted


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GotFileUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( { model | urlResponse = ActionResult.Success () }, Navigation.load urlResponse.url )

                Err err ->
                    let
                        redirectCmd =
                            case ( Session.exists appState.session, err ) of
                                ( False, ApiError.BadStatus 403 _ ) ->
                                    Routes.projectsFileDownload model.projectUuid model.fileUuid
                                        |> toUrl
                                        |> Just
                                        |> Routes.publicLogin
                                        |> cmdNavigate appState

                                _ ->
                                    Cmd.none

                        errorMessage =
                            case err of
                                ApiError.BadStatus 403 _ ->
                                    gettext "You do not have permission to access this file" appState.locale

                                ApiError.BadStatus 404 _ ->
                                    gettext "File not found" appState.locale

                                _ ->
                                    gettext "Unable to get file" appState.locale
                    in
                    ( { model | urlResponse = ActionResult.Error errorMessage }
                    , redirectCmd
                    )
