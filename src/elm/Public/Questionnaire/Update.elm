module Public.Questionnaire.Update exposing (fetchData, handleGetQuestionnaireCompleted, handleQuestionnaireMsg, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (initialModel)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Msgs
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg(..))
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        QuestionnairesApi.getQuestionnairePublic appState GetQuestionnaireCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg questionnaireMsg appState model


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model | questionnaireModel = Success <| initialModel appState questionnaireDetail [] [] }

                Err error ->
                    { model | questionnaireModel = getServerError error "Unable to get questionnaire." }
    in
    ( newModel, Cmd.none )


handleQuestionnaireMsg : (Msg -> Msgs.Msg) -> Common.Questionnaire.Msgs.Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg appState model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Common.Questionnaire.Update.update msg appState qm
                    in
                    ( Success questionnaireModel, questionnaireCmd )

                _ ->
                    ( model.questionnaireModel, Cmd.none )
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg) )
