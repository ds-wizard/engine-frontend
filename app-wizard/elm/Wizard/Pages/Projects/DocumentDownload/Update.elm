module Wizard.Pages.Projects.DocumentDownload.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Browser.Navigation as Navigation
import Common.Data.ApiError as ApiError
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.Documents as DocumentsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session
import Wizard.Pages.Projects.DocumentDownload.Models exposing (Model)
import Wizard.Pages.Projects.DocumentDownload.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate, toUrl)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState fileUuid =
    DocumentsApi.getDocumentUrl appState fileUuid GotDocumentUrlCompleted


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GotDocumentUrlCompleted result ->
            case result of
                Ok urlResponse ->
                    ( { model | urlResponse = ActionResult.Success () }, Navigation.load urlResponse.url )

                Err err ->
                    let
                        redirectCmd =
                            case ( Session.exists appState.session, err ) of
                                ( False, ApiError.BadStatus 403 _ ) ->
                                    Routes.projectDocumentDownload model.questionnaireUuid model.fileUuid
                                        |> toUrl
                                        |> Just
                                        |> Routes.publicLogin
                                        |> cmdNavigate appState

                                _ ->
                                    Cmd.none

                        errorMessage =
                            case err of
                                ApiError.BadStatus 403 _ ->
                                    gettext "You do not have permission to access this document" appState.locale

                                ApiError.BadStatus 404 _ ->
                                    gettext "Document not found" appState.locale

                                _ ->
                                    gettext "Unable to get document" appState.locale
                    in
                    ( { model | urlResponse = ActionResult.Error errorMessage }
                    , redirectCmd
                    )
