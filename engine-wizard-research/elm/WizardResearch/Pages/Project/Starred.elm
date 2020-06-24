module WizardResearch.Pages.Project.Starred exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html.Styled exposing (Html, h1, text)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Foundations.Grid as Grid
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import WizardResearch.Common.AppState exposing (AppState)



-- MODEL


type alias Model =
    { questionnaire : ActionResult QuestionnaireDetail }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState questionnaireUuid =
    ( { questionnaire = Loading }
    , QuestionnairesApi.getQuestionnaire questionnaireUuid appState GetQuestionnaireComplete
    )



-- UPDATE


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)


update : AppState -> Msg -> Model -> ( Model, Cmd msg )
update appState msg model =
    case msg of
        GetQuestionnaireComplete result ->
            case result of
                Ok questionnaire ->
                    ( { model | questionnaire = Success questionnaire }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | questionnaire = ApiError.toActionResult "Unable to get project" error }
                      -- TODO maybe logout
                    , Cmd.none
                    )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    ActionResultWrapper.blockLG appState.theme (viewContent appState model) model.questionnaire


viewContent : AppState -> Model -> QuestionnaireDetail -> Html Msg
viewContent appState model questionnaire =
    let
        grid =
            Grid.comfortable
    in
    grid.container
        [ Grid.containerLimited ]
        [ grid.row []
            [ grid.col 12
                []
                [ h1 [] [ text "Starred" ]
                ]
            ]
        ]
