module WizardResearch.Pages.Project exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

-- MODEL

import ActionResult exposing (ActionResult(..))
import Html.Styled exposing (Html)
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


view : AppState -> Model -> { title : String, content : Html Msg }
view appState model =
    let
        grid =
            Grid.comfortable

        viewContent questionnaire =
            grid.container
                [ Grid.containerLimitedSmall, Grid.containerIndented ]
                [ grid.row []
                    [ grid.col 12 [] [ Heading.h1 appState.theme questionnaire.name ] ]
                ]

        title =
            ActionResult.unwrap "Project" .name model.questionnaire
    in
    { title = title
    , content = ActionResultWrapper.page appState.theme viewContent model.questionnaire
    }
