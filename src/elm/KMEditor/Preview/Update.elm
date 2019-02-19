module KMEditor.Preview.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import Jwt
import KMEditor.Preview.Models exposing (..)
import KMEditor.Preview.Msgs exposing (Msg(..))
import KMEditor.Requests exposing (getKnowledgeModelData, getLevels)
import Models exposing (State)
import Msgs
import Requests exposing (getResultCmd)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    Cmd.batch
        [ fetchKnowledgeModel wrapMsg uuid session
        , fetchLevels wrapMsg session
        ]


fetchKnowledgeModel : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchKnowledgeModel wrapMsg uuid session =
    getKnowledgeModelData uuid session
        |> Jwt.send GetKnowledgeModelCompleted
        |> Cmd.map wrapMsg


fetchLevels : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchLevels wrapMsg session =
    getLevels session
        |> Jwt.send GetLevelsCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        GetKnowledgeModelCompleted result ->
            case result of
                Ok km ->
                    ( setKnowledgeModel km model, Cmd.none )

                Err error ->
                    ( { model | questionnaireModel = getServerErrorJwt error "Unable to get Knowledge Model" }
                    , getResultCmd result
                    )

        GetLevelsCompleted result ->
            case result of
                Ok levels ->
                    ( { model | levels = Success levels }, Cmd.none )

                Err error ->
                    ( { model | levels = getServerErrorJwt error "Unable to get levels." }
                    , getResultCmd result
                    )

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg questionnaireMsg model

        AddTag uuid ->
            ( addTag uuid model, Cmd.none )

        RemoveTag uuid ->
            ( removeTag uuid model, Cmd.none )

        SelectAllTags ->
            ( selectAllTags model, Cmd.none )

        SelectNoneTags ->
            ( selectNoneTags model, Cmd.none )


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
