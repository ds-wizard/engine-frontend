module DSPlanner.Detail.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, initialModel, updateReplies)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Common.Types exposing (ActionResult(..))
import DSPlanner.Detail.Models exposing (Model)
import DSPlanner.Detail.Msgs exposing (Msg(..))
import DSPlanner.Requests exposing (getQuestionnaire, putReplies)
import DSPlanner.Routing exposing (Route(Index))
import FormEngine.Model exposing (..)
import Jwt
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchData wrapMsg session uuid =
    getQuestionnaire uuid session
        |> Jwt.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        QuestionnaireMsg msg ->
            handleQuestionnaireMsg wrapMsg msg model

        Save ->
            handleSave wrapMsg session model

        PutRepliesCompleted result ->
            handlePutRepliesCompleted model result


handleGetQuestionnaireCompleted : Model -> Result Jwt.JwtError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    let
        newModel =
            case result of
                Ok questionnaireDetail ->
                    { model | questionnaireModel = Success <| initialModel questionnaireDetail }

                Err error ->
                    { model | questionnaireModel = getServerErrorJwt error "Unable to get questionnaire." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleQuestionnaireMsg : (Msg -> Msgs.Msg) -> Common.Questionnaire.Msgs.Msg -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Common.Questionnaire.Update.update msg qm
                    in
                    ( Success questionnaireModel, questionnaireCmd )

                _ ->
                    ( model.questionnaireModel, Cmd.none )
    in
    ( { model | questionnaireModel = newQuestionnaireModel }, cmd |> Cmd.map (QuestionnaireMsg >> wrapMsg) )


handleSave : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleSave wrapMsg session model =
    case model.questionnaireModel of
        Success questionnaireModel ->
            let
                newQuestionnaireModel =
                    updateReplies questionnaireModel

                cmd =
                    putRepliesCmd wrapMsg session model.uuid newQuestionnaireModel.questionnaire
            in
            ( { model | questionnaireModel = Success newQuestionnaireModel }, cmd )

        _ ->
            ( model, Cmd.none )


handlePutRepliesCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
handlePutRepliesCompleted model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate <| Routing.DSPlanner Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerErrorJwt error "Questionnaire could not be saved." }
            , getResultCmd result
            )


putRepliesCmd : (Msg -> Msgs.Msg) -> Session -> String -> QuestionnaireDetail -> Cmd Msgs.Msg
putRepliesCmd wrapMsg session uuid questionnaire =
    questionnaire.replies
        |> encodeFormValues
        |> putReplies uuid session
        |> Jwt.send PutRepliesCompleted
        |> Cmd.map wrapMsg
