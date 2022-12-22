module Wizard.Projects.Detail.Components.RevertModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , setEvent
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, br, p, strong, text)
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Modal as Modal
import Wizard.Ports as Ports



-- MODEL


type alias Model =
    { mbEvent : Maybe QuestionnaireEvent
    , revertResult : ActionResult ()
    }


init : Model
init =
    { mbEvent = Nothing
    , revertResult = Unset
    }


setEvent : QuestionnaireEvent -> Model -> Model
setEvent event model =
    { model
        | mbEvent = Just event
        , revertResult = Unset
    }



-- UPDATE


type Msg
    = Revert
    | PostRevertVersionComplete (Result ApiError ())
    | Close


type alias UpdateConfig =
    { questionnaireUuid : Uuid
    }


update : UpdateConfig -> AppState -> Msg -> Model -> ( Model, Cmd Msg )
update cfg appState msg model =
    case msg of
        Revert ->
            case model.mbEvent of
                Just event ->
                    let
                        cmd =
                            QuestionnairesApi.postRevert cfg.questionnaireUuid (QuestionnaireEvent.getUuid event) appState PostRevertVersionComplete
                    in
                    ( { model | revertResult = Loading }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        PostRevertVersionComplete result ->
            case result of
                Ok _ ->
                    ( model, Ports.refresh () )

                Err error ->
                    ( { model | revertResult = ApiError.toActionResult appState "Unable to revert the project" error }
                    , Cmd.none
                    )

        Close ->
            ( { model | mbEvent = Nothing }, Cmd.none )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        datetime =
            Maybe.unwrap "" (QuestionnaireEvent.getCreatedAt >> TimeUtils.toReadableDateTime appState.timeZone) model.mbEvent

        content =
            [ Flash.warning appState (gettext "Heads up! This action cannot be undone." appState.locale)
            , p []
                (String.formatHtml
                    (gettext "Are you sure you want to revert the projects to its state from %s?" appState.locale)
                    [ strong [] [ br [] [], text datetime ] ]
                )
            ]
    in
    Modal.confirm appState
        { modalTitle = gettext "Revert questionnaire" appState.locale
        , modalContent = content
        , visible = Maybe.isJust model.mbEvent
        , actionResult = ActionResult.map (always "") model.revertResult
        , actionName = gettext "Revert" appState.locale
        , actionMsg = Revert
        , cancelMsg = Just Close
        , dangerous = True
        , dataCy = "project-revert"
        }
