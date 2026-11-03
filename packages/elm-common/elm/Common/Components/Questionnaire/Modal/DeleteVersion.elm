module Common.Components.Questionnaire.Modal.DeleteVersion exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , open
    , update
    , view
    )

{-| Taking a version's name off a change.

The change itself stays and so do the answers — only the name goes, which is what
the dialog says, because "delete version" reads like losing the work.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectVersion exposing (ProjectVersion)
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Data.AppState exposing (AppState)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Maybe.Extra as Maybe
import Task
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , version : Maybe ProjectVersion
    , result : ActionResult ()
    }


init : Uuid -> Model
init projectUuid =
    { projectUuid = projectUuid, version = Nothing, result = Unset }


open : ProjectVersion -> Model -> Model
open version model =
    { model | version = Just version, result = Unset }


type Msg
    = Confirm
    | Deleted (Result ApiError ())
    | Close


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , deletedMsg : ProjectVersion -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Confirm ->
            case model.version of
                Just version ->
                    ( { model | result = Loading }
                    , ProjectsApi.deleteVersion appState model.projectUuid version.uuid (cfg.wrapMsg << Deleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        Deleted result ->
            case ( result, model.version ) of
                ( Ok _, Just version ) ->
                    ( { model | version = Nothing, result = Success () }
                    , Task.perform (always (cfg.deletedMsg version)) (Task.succeed ())
                    )

                ( Err error, _ ) ->
                    ( { model | result = ApiError.toActionResult appState (gettext "Unable to delete the version." appState.locale) error }
                    , RequestHelpers.getResultCmd (cfg.wrapMsg Close) result
                    )

                _ ->
                    ( model, Cmd.none )

        Close ->
            ( { model | version = Nothing }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete the version" appState.locale
        , open = Maybe.isJust model.version
        , question = gettext "Are you sure you want to delete this version? The answers stay as they are; only its name goes." appState.locale
        , entity = text (Maybe.unwrap "" .name model.version)
        , onConfirm = Confirm
        , onClose = Close
        , result = model.result
        , testId = Just "version-delete"
        }
