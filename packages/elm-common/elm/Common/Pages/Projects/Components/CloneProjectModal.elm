module Common.Pages.Projects.Components.CloneProjectModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

{-| "Clone Project": a confirmation that creates a copy of a project and, when
the copy exists, hands it to the caller (the listing opens it).

A failure stays in the dialog (`Common.Components.ConfirmModal`), so the user
can try again or cancel.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Project exposing (Project)
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Data.AppState exposing (AppState)
import Common.Pages.Projects.Components.ProjectDescriptor exposing (ProjectDescriptor)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { projectToBeCloned : Maybe ProjectDescriptor
    , cloningProject : ActionResult ()
    }


initialModel : Model
initialModel =
    { projectToBeCloned = Nothing
    , cloningProject = Unset
    }


type Msg
    = ShowHideCloneProject (Maybe ProjectDescriptor)
    | CloneProject
    | CloneProjectCompleted (Result ApiError Project)


open : ProjectDescriptor -> Msg
open =
    ShowHideCloneProject << Just


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , cloneCompleteCmd : Project -> Cmd msg
    , closeCmd : Uuid -> Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ShowHideCloneProject mbProject ->
            -- `closeCmd` runs when the dialog is dismissed, for returning focus
            -- to where it was opened from. The dialog also reports closing
            -- once it has done its work, when there is no project any more.
            ( { model | projectToBeCloned = mbProject, cloningProject = Unset }
            , case ( mbProject, model.projectToBeCloned ) of
                ( Nothing, Just project ) ->
                    cfg.closeCmd project.uuid

                _ ->
                    Cmd.none
            )

        CloneProject ->
            case model.projectToBeCloned of
                Just project ->
                    ( { model | cloningProject = Loading }
                    , ProjectsApi.clone appState project.uuid (cfg.wrapMsg << CloneProjectCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        CloneProjectCompleted result ->
            case result of
                Ok project ->
                    ( { model | cloningProject = Success (), projectToBeCloned = Nothing }
                    , cfg.cloneCompleteCmd project
                    )

                Err error ->
                    ( { model | cloningProject = ApiError.toActionResult appState (gettext "Unable to clone project." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> Html Msg
view appState model =
    let
        name =
            Maybe.map .name model.projectToBeCloned
                |> Maybe.withDefault ""
    in
    ConfirmModal.confirmWithTestId appState
        { title = gettext "Clone Project" appState.locale
        , open = Maybe.isJust model.projectToBeCloned
        , body =
            [ Text.body (String.format (gettext "Do you want to create a copy of %s?" appState.locale) [ name ])
            , Text.muted (gettext "The original project will remain unchanged." appState.locale)
            ]
        , confirmLabel = gettext "Clone" appState.locale
        , onConfirm = CloneProject
        , onClose = ShowHideCloneProject Nothing
        , result = model.cloningProject
        , testId = Just "clone-project"
        }
