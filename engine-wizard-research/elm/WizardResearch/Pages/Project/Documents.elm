module WizardResearch.Pages.Project.Documents exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

-- MODEL

import ActionResult exposing (ActionResult(..))
import Html.Styled exposing (Html, h1, text)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Elemental.Atoms.Heading as Heading
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Foundations.Grid as Grid
import Shared.Error.ApiError as ApiError exposing (ApiError)
import WizardResearch.Common.AppState exposing (AppState)


type alias Model =
    { questionnaire : ActionResult Questionnaire }


init : AppState -> String -> ( Model, Cmd Msg )
init appState questionnaireUuid =
    ( { questionnaire = Loading }
    , QuestionnairesApi.getQuestionnaire questionnaireUuid appState GetQuestionnaireComplete
    )



-- UPDATE


type Msg
    = GetQuestionnaireComplete (Result ApiError Questionnaire)


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


viewContent : AppState -> Model -> Questionnaire -> Html Msg
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
                [ h1 [] [ text "Documents" ]
                ]
            ]
        ]
