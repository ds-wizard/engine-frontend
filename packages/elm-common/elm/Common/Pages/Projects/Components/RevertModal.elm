module Common.Pages.Projects.Components.RevertModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , setEvent
    , update
    , view
    )

{-| Putting the project back to what it was at some change.

Everything after that change goes, which is why the dialog says so before it
asks. The answer comes back from the server as a new state for the whole
project, so the page is loaded again rather than patched here — reverting is
rare, and half-updating a questionnaire somebody else is also editing is not
worth the cleverness.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Data.AppState exposing (AppState)
import Common.Ports.Window as Window
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { event : Maybe ProjectEvent
    , result : ActionResult ()
    }


init : Model
init =
    { event = Nothing, result = Unset }


setEvent : ProjectEvent -> Model -> Model
setEvent event model =
    { model | event = Just event, result = Unset }


type Msg
    = Confirm
    | Reverted (Result ApiError ())
    | Close


type alias UpdateConfig =
    { projectUuid : Uuid }


update : AppState -> UpdateConfig -> Msg -> Model -> ( Model, Cmd Msg )
update appState cfg msg model =
    case msg of
        Confirm ->
            case model.event of
                Just event ->
                    ( { model | result = Loading }
                    , ProjectsApi.postRevert appState cfg.projectUuid (ProjectEvent.getUuid event) Reverted
                    )

                Nothing ->
                    ( model, Cmd.none )

        Reverted result ->
            case result of
                Ok _ ->
                    ( model, Window.refresh () )

                Err error ->
                    ( { model | result = ApiError.toActionResult appState (gettext "Unable to revert the project." appState.locale) error }
                    , Cmd.none
                    )

        Close ->
            ( { model | event = Nothing }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        moment =
            Maybe.unwrap "" (ProjectEvent.getCreatedAt >> TimeUtils.toReadableDateTime appState.timeZone) model.event
    in
    ConfirmModal.dangerConfirm appState
        { title = gettext "Revert the project" appState.locale
        , open = Maybe.isJust model.event
        , body =
            [ Text.body
                (String.format (gettext "Are you sure you want to put the project back to what it was on %s?" appState.locale) [ moment ])
            , Alert.toHtml (Alert.warning (gettext "Everything answered since then is lost, and this cannot be undone." appState.locale))
            ]
        , confirmLabel = gettext "Revert" appState.locale
        , onConfirm = Confirm
        , onClose = Close
        , result = model.result
        , testId = Just "project-revert"
        }
