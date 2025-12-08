module Wizard.Pages.Projects.Common.DeleteProjectModal.Update exposing (UpdateConfig, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.DeleteProjectModal.Models exposing (Model)
import Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type alias UpdateConfig =
    { wrapMsg : Msg -> Wizard.Msgs.Msg
    , deleteCompleteCmd : Cmd Wizard.Msgs.Msg
    }


update : UpdateConfig -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update cfg msg appState model =
    case msg of
        ShowHideDeleteProject mbQuestionnaire ->
            handleShowHideDeleteProject model mbQuestionnaire

        DeleteProject ->
            handleDeleteProject cfg appState model

        DeleteProjectCompleted result ->
            handleDeleteProjectCompleted cfg appState model result



-- Handlers


handleShowHideDeleteProject : Model -> Maybe ProjectDescriptor -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteProject model mbQuestionnaire =
    ( { model | projectToBeDeleted = mbQuestionnaire, deletingProject = Unset }
    , Cmd.none
    )


handleDeleteProject : UpdateConfig -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteProject cfg appState model =
    case model.projectToBeDeleted of
        Just project ->
            let
                newModel =
                    { model | deletingProject = Loading }

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        ProjectsApi.delete appState project.uuid DeleteProjectCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteProjectCompleted : UpdateConfig -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteProjectCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingProject = Unset
                , projectToBeDeleted = Nothing
              }
            , cfg.deleteCompleteCmd
            )

        Err error ->
            ( { model | deletingProject = ApiError.toActionResult appState (gettext "Project could not be deleted." appState.locale) error }
            , Cmd.none
            )
