module Wizard.Public.Questionnaire.Update exposing (fetchData, handleGetQuestionnaireCompleted, handleQuestionnaireMsg, update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
import Wizard.Common.Questionnaire.Models exposing (initialModel)
import Wizard.Common.Questionnaire.Msgs
import Wizard.Common.Questionnaire.Update
import Wizard.Msgs
import Wizard.Public.Questionnaire.Models exposing (Model)
import Wizard.Public.Questionnaire.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


fetchData : AppState -> Cmd Msg
fetchData appState =
    QuestionnairesApi.getQuestionnairePublic appState GetQuestionnaireCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg questionnaireMsg appState model


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model | questionnaireModel = Success <| initialModel appState questionnaireDetail [] [] }

                Err error ->
                    { model | questionnaireModel = ApiError.toActionResult (lg "apiError.questionnaires.pubic.getError" appState) error }
    in
    ( newModel, Cmd.none )


handleQuestionnaireMsg : (Msg -> Wizard.Msgs.Msg) -> Wizard.Common.Questionnaire.Msgs.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg appState model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Wizard.Common.Questionnaire.Update.update msg appState qm
                    in
                    ( Success questionnaireModel, questionnaireCmd )

                _ ->
                    ( model.questionnaireModel, Cmd.none )
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg) )
