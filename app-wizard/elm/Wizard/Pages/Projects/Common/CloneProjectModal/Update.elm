module Wizard.Pages.Projects.Common.CloneProjectModal.Update exposing (UpdateConfig, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Common.CloneProjectModal.Models exposing (Model)
import Wizard.Pages.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type alias UpdateConfig =
    { wrapMsg : Msg -> Wizard.Msgs.Msg
    , cloneCompleteCmd : Project -> Cmd Wizard.Msgs.Msg
    }


update : UpdateConfig -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update cfg msg appState model =
    case msg of
        ShowHideCloneProject mbProject ->
            handleShowHideDeleteProject model mbProject

        CloneProject ->
            handleDeleteProject cfg appState model

        CloneProjectCompleted result ->
            handleDeleteProjectCompleted cfg appState model result



-- Handlers


handleShowHideDeleteProject : Model -> Maybe ProjectDescriptor -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteProject model mbProject =
    ( { model | projectToBeDeleted = mbProject, cloningProject = Unset }
    , Cmd.none
    )


handleDeleteProject : UpdateConfig -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteProject cfg appState model =
    case model.projectToBeDeleted of
        Just project ->
            let
                newModel =
                    { model | cloningProject = Loading }

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        ProjectsApi.clone appState project.uuid CloneProjectCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteProjectCompleted : UpdateConfig -> AppState -> Model -> Result ApiError Project -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteProjectCompleted cfg appState model result =
    case result of
        Ok project ->
            ( { model
                | cloningProject = Success <| gettext "%s has been created." appState.locale
                , projectToBeDeleted = Nothing
              }
            , cfg.cloneCompleteCmd project
            )

        Err error ->
            ( { model | cloningProject = ApiError.toActionResult appState (gettext "Unable to clone project." appState.locale) error }
            , Cmd.none
            )
