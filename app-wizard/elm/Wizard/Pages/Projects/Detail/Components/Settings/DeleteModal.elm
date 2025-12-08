module Wizard.Pages.Projects.Detail.Components.Settings.DeleteModal exposing (Model, Msg, UpdateConfig, initialModel, open, update, view)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Modal as Modal
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import String.Format as String
import Wizard.Api.Projects as ProjectsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Common.ProjectDescriptor exposing (ProjectDescriptor)


type alias Model =
    { projectToBeDeleted : Maybe ProjectDescriptor
    , deletingProject : ActionResult String
    }


initialModel : Model
initialModel =
    { projectToBeDeleted = Nothing
    , deletingProject = Unset
    }


type Msg
    = ShowHideDeleteProject (Maybe ProjectDescriptor)
    | DeleteProject
    | DeleteProjectCompleted (Result ApiError ())


open : ProjectDescriptor -> Msg
open =
    ShowHideDeleteProject << Just


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , deleteCompleteCmd : Cmd msg
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        ShowHideDeleteProject mbProject ->
            handleShowHideDeleteProject model mbProject

        DeleteProject ->
            handleDeleteProject cfg appState model

        DeleteProjectCompleted result ->
            handleDeleteProjectCompleted cfg appState model result


handleShowHideDeleteProject : Model -> Maybe ProjectDescriptor -> ( Model, Cmd msg )
handleShowHideDeleteProject model mbQuestionnaire =
    ( { model | projectToBeDeleted = mbQuestionnaire, deletingProject = Unset }
    , Cmd.none
    )


handleDeleteProject : UpdateConfig msg -> AppState -> Model -> ( Model, Cmd msg )
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


handleDeleteProjectCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
handleDeleteProjectCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingProject = Success <| gettext "Questionnaire was successfully deleted." appState.locale
                , projectToBeDeleted = Nothing
              }
            , cfg.deleteCompleteCmd
            )

        Err error ->
            ( { model | deletingProject = ApiError.toActionResult appState (gettext "Questionnaire could not be deleted." appState.locale) error }
            , Cmd.none
            )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.projectToBeDeleted of
                Just project ->
                    ( True, project.name )

                Nothing ->
                    ( False, "" )

        modalContent =
            [ p []
                (String.formatHtml
                    (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                    [ strong [] [ text name ] ]
                )
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Delete Project" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.deletingProject
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteProject
                |> Modal.confirmConfigCancelMsg (ShowHideDeleteProject Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "project-delete"
    in
    Modal.confirm appState modalConfig
