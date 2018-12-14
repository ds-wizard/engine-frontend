module Public.Questionnaire.Update exposing (fetchData, handleGetQuestionnaireCompleted, handleQuestionnaireMsg, update)

import ActionResult exposing (ActionResult(..))
import Common.Models exposing (getServerError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, initialModel)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Http
import Msgs
import Public.Questionnaire.Models exposing (Model)
import Public.Questionnaire.Msgs exposing (Msg(..))
import Public.Questionnaire.Requests exposing (getQuestionnaire)


fetchData : (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
fetchData wrapMsg =
    getQuestionnaire
        |> Http.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg questionnaireMsg model


handleGetQuestionnaireCompleted : Model -> Result Http.Error QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model | questionnaireModel = Success <| initialModel questionnaireDetail }

                Err error ->
                    { model | questionnaireModel = getServerError error "Unable to get questionnaire." }
    in
    ( newModel, Cmd.none )


handleQuestionnaireMsg : (Msg -> Msgs.Msg) -> Common.Questionnaire.Msgs.Msg -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Common.Questionnaire.Update.update msg Nothing qm
                    in
                    ( Success questionnaireModel, questionnaireCmd )

                _ ->
                    ( model.questionnaireModel, Cmd.none )
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg) )
