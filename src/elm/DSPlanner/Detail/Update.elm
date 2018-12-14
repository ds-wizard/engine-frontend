module DSPlanner.Detail.Update exposing (fetchData, fetchLevels, fetchQuestionnaire, handleGetLevelsCompleted, handleGetQuestionnaireCompleted, handlePutRepliesCompleted, handleQuestionnaireMsg, handleSave, putRepliesCmd, update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, encodeQuestionnaireDetail, initialModel, updateReplies)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import DSPlanner.Detail.Models exposing (Model)
import DSPlanner.Detail.Msgs exposing (Msg(..))
import DSPlanner.Requests exposing (getQuestionnaire, putQuestionnaire)
import DSPlanner.Routing exposing (Route(..))
import Jwt
import KMEditor.Common.Models.Entities exposing (Level)
import KMEditor.Requests exposing (getLevels)
import Models exposing (State)
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchData wrapMsg session uuid =
    Cmd.batch
        [ fetchQuestionnaire wrapMsg session uuid
        , fetchLevels wrapMsg session
        ]


fetchQuestionnaire : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchQuestionnaire wrapMsg session uuid =
    getQuestionnaire uuid session
        |> Jwt.send GetQuestionnaireCompleted
        |> Cmd.map wrapMsg


fetchLevels : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchLevels wrapMsg session =
    getLevels session
        |> Jwt.send GetLevelsCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        GetLevelsCompleted result ->
            handleGetLevelsCompleted model result

        QuestionnaireMsg qMsg ->
            handleQuestionnaireMsg wrapMsg qMsg state.session model

        Save ->
            handleSave wrapMsg state.session model

        PutRepliesCompleted result ->
            handlePutRepliesCompleted state model result


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


handleGetLevelsCompleted : Model -> Result Jwt.JwtError (List Level) -> ( Model, Cmd Msgs.Msg )
handleGetLevelsCompleted model result =
    let
        newModel =
            case result of
                Ok levels ->
                    { model | levels = Success levels }

                Err error ->
                    { model | levels = getServerErrorJwt error "Unable to get levels." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleQuestionnaireMsg : (Msg -> Msgs.Msg) -> Common.Questionnaire.Msgs.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg wrapMsg msg session model =
    let
        ( newQuestionnaireModel, cmd ) =
            case model.questionnaireModel of
                Success qm ->
                    let
                        ( questionnaireModel, questionnaireCmd ) =
                            Common.Questionnaire.Update.update msg (Just session) qm
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


handlePutRepliesCompleted : State -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
handlePutRepliesCompleted state model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate state.key <| Routing.DSPlanner Index )

        Err error ->
            ( { model | savingQuestionnaire = getServerErrorJwt error "Questionnaire could not be saved." }
            , getResultCmd result
            )


putRepliesCmd : (Msg -> Msgs.Msg) -> Session -> String -> QuestionnaireDetail -> Cmd Msgs.Msg
putRepliesCmd wrapMsg session uuid questionnaire =
    encodeQuestionnaireDetail questionnaire
        |> putQuestionnaire uuid session
        |> Jwt.send PutRepliesCompleted
        |> Cmd.map wrapMsg
