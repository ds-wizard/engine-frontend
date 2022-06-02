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
import Html exposing (Html, br, p, strong, text)
import Maybe.Extra as Maybe
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lh)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Modal as Modal
import Wizard.Ports as Ports


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.RevertModal"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Projects.Detail.Components.RevertModal"



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
                    ( { model | revertResult = ApiError.toActionResult appState "Unable to revert questionnaire" error }
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
            [ Flash.warning appState (l_ "warning" appState)
            , p []
                (lh_ "message" [ strong [] [ br [] [], text datetime ] ] appState)
            ]
    in
    Modal.confirm appState
        { modalTitle = l_ "title" appState
        , modalContent = content
        , visible = Maybe.isJust model.mbEvent
        , actionResult = ActionResult.map (always "") model.revertResult
        , actionName = l_ "action" appState
        , actionMsg = Revert
        , cancelMsg = Just Close
        , dangerous = True
        , dataCy = "project-revert"
        }
