module Wizard.Projects.FileDownload.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Browser.Navigation as Navigation
import Gettext exposing (gettext)
import Shared.Api.QuestionnaireFiles as QuestionnaireFilesApi
import Shared.Auth.Session as Session
import Shared.Error.ApiError as ApiError
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.FileDownload.Models exposing (Model)
import Wizard.Projects.FileDownload.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate, toUrl)


fetchData : AppState -> Uuid -> Uuid -> Cmd Msg
fetchData appState projectUuid fileUuid =
    QuestionnaireFilesApi.getFileUrl projectUuid fileUuid appState GotFileUrlCompleted


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
                                    Routes.projectsFileDownload model.questionnaireUuid model.fileUuid
                                        |> toUrl appState
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
