module Common.Pages.Projects.Components.DeleteProjectModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

{-| "Delete Project": a destructive confirmation. On success the dialog closes
and the caller is told (the listing reloads and moves focus to its heading,
because the row that had it is gone).
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.EntityCard as EntityCard
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Pages.Projects.Components.ProjectDescriptor exposing (ProjectDescriptor)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


type alias Model =
    { projectToBeDeleted : Maybe ProjectDescriptor
    , deletingProject : ActionResult ()
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
    , logoutMsg : msg
    , deleteCompleteCmd : Cmd msg
    , closeCmd : Uuid -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ShowHideDeleteProject mbProject ->
            -- `closeCmd` runs when the dialog is dismissed, for returning focus
            -- to where it was opened from. The dialog also reports closing
            -- once it has done its work, when there is no project any more.
            ( { model | projectToBeDeleted = mbProject, deletingProject = Unset }
            , case ( mbProject, model.projectToBeDeleted ) of
                ( Nothing, Just project ) ->
                    cfg.closeCmd project.uuid

                _ ->
                    Cmd.none
            )

        DeleteProject ->
            case model.projectToBeDeleted of
                Just project ->
                    ( { model | deletingProject = Loading }
                    , ProjectsApi.delete appState project.uuid (cfg.wrapMsg << DeleteProjectCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteProjectCompleted result ->
            case result of
                Ok _ ->
                    ( { model | deletingProject = Unset, projectToBeDeleted = Nothing }
                    , cfg.deleteCompleteCmd
                    )

                Err error ->
                    ( { model | deletingProject = ApiError.toActionResult appState (gettext "Project could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> Html Msg
view appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete Project" appState.locale
        , open = Maybe.isJust model.projectToBeDeleted
        , question = gettext "Are you sure you want to permanently delete this project?" appState.locale
        , entity =
            EntityCard.slot
                (Maybe.map
                    (\project ->
                        { icon = UserAvatar.item project.name Nothing
                        , name = project.name
                        , badges = []
                        , code = Nothing
                        , detail = project.description
                        }
                    )
                    model.projectToBeDeleted
                )
        , onConfirm = DeleteProject
        , onClose = ShowHideDeleteProject Nothing
        , result = model.deletingProject
        , testId = Just "project-delete"
        }
