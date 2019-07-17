module Questionnaires.Migration.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult)
import Common.Api.Levels as LevelsApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (initialModel)
import Common.Questionnaire.Msgs as QuestionnaireMsgs
import Common.Questionnaire.Update
import Common.Setters exposing (setLevels)
import KMEditor.Common.Models.Entities exposing (Level)
import Maybe.Extra as Maybe
import Msgs
import Ports
import Questionnaires.Common.QuestionChange as QuestionChange exposing (QuestionChange)
import Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail
import Questionnaires.Common.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Questionnaires.Migration.Models exposing (Model, initializeChangeList)
import Questionnaires.Migration.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (Route(..), cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaireMigration uuid appState GetQuestionnaireMigrationCompleted
        , LevelsApi.getLevels appState GetLevelsCompleted
        ]


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnaireMigrationCompleted result ->
            handleGetQuestionnaireMigrationCompleted appState model result

        PutQuestionnaireMigrationCompleted result ->
            handlePutQuestionnaireMigrationCompleted model result

        GetLevelsCompleted result ->
            handleGetLevelsCompleted model result

        SelectChange change ->
            handleSelectChange appState model (Just change)

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg appState model questionnaireMsg

        ResolveCurrentChange ->
            handleResolveCurrentChange wrapMsg appState model

        UndoResolveCurrentChange ->
            handleUndoResolveCurrentChange wrapMsg appState model

        FinalizeMigration ->
            handleFinalizeMigration wrapMsg appState model

        FinalizeMigrationCompleted result ->
            handleFinalizeMigrationCompleted appState model result

        PutQuestionnaireCompleted _ ->
            ( model, Cmd.none )



-- Handlers


handleGetQuestionnaireMigrationCompleted : AppState -> Model -> Result ApiError QuestionnaireMigration -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireMigrationCompleted appState model result =
    let
        ( modelWithMigration, cmd ) =
            applyResult
                { setResult = setResult appState
                , defaultError = "Unable to get questionnaire migration."
                , result = result
                , model = model
                }

        ( newModel, scrollCmd ) =
            handleSelectChange appState modelWithMigration modelWithMigration.selectedChange
    in
    ( newModel, Cmd.batch [ cmd, scrollCmd ] )


handlePutQuestionnaireMigrationCompleted : Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handlePutQuestionnaireMigrationCompleted model result =
    applyResult
        { setResult = \_ _ -> model
        , defaultError = "Unable to save migration."
        , result = result
        , model = model
        }


handleGetLevelsCompleted : Model -> Result ApiError (List Level) -> ( Model, Cmd Msgs.Msg )
handleGetLevelsCompleted model result =
    applyResult
        { setResult = setLevels
        , defaultError = "Unable to get levels."
        , result = result
        , model = model
        }


handleSelectChange : AppState -> Model -> Maybe QuestionChange -> ( Model, Cmd Msgs.Msg )
handleSelectChange appState model mbChange =
    case mbChange of
        Just change ->
            let
                newModelWithChapter =
                    case model.questionnaireModel of
                        Just questionnaireModel ->
                            let
                                ( newQuestionnaireModel, _ ) =
                                    Common.Questionnaire.Update.update
                                        (QuestionnaireMsgs.SetActiveChapter <| QuestionChange.getChapter change)
                                        appState
                                        questionnaireModel
                            in
                            { model | questionnaireModel = Just newQuestionnaireModel }

                        Nothing ->
                            model

                newModel =
                    { newModelWithChapter | selectedChange = Just change }
            in
            ( newModel, scrollToQuestion newModel )

        Nothing ->
            ( model, Cmd.none )


handleQuestionnaireMsg : (Msg -> Msgs.Msg) -> AppState -> Model -> QuestionnaireMsgs.Msg -> ( Model, Cmd Msgs.Msg )
handleQuestionnaireMsg wrapMsg appState model questionnaireMsg =
    case model.questionnaireModel of
        Just questionnaireModel ->
            let
                ( updatedQuestionnaireModel, _ ) =
                    Common.Questionnaire.Update.update questionnaireMsg appState questionnaireModel

                body =
                    QuestionnaireDetail.encode updatedQuestionnaireModel.questionnaire

                ( newQuestionnaireModel, cmd ) =
                    if updatedQuestionnaireModel.dirty then
                        ( { updatedQuestionnaireModel | dirty = False }
                        , Cmd.map wrapMsg <|
                            QuestionnairesApi.putQuestionnaire model.questionnaireUuid body appState PutQuestionnaireCompleted
                        )

                    else
                        ( updatedQuestionnaireModel, Cmd.none )
            in
            ( { model | questionnaireModel = Just newQuestionnaireModel }, cmd )

        Nothing ->
            ( model, Cmd.none )


handleResolveCurrentChange : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleResolveCurrentChange wrapMsg appState model =
    let
        newQuestionnaireMigration =
            model.selectedChange
                |> Maybe.map (QuestionChange.getQuestionUuid >> setQuestionUuid)
                |> Maybe.withDefault model.questionnaireMigration

        setQuestionUuid questionUuid =
            ActionResult.map (QuestionnaireMigration.addResolvedQuestion questionUuid) model.questionnaireMigration

        isResolved questionUuid =
            ActionResult.map (QuestionnaireMigration.isQuestionResolved questionUuid) newQuestionnaireMigration
                |> ActionResult.withDefault False

        nextChange =
            model.changes.questions
                |> List.filter (not << isResolved << QuestionChange.getQuestionUuid)
                |> List.head
                |> Maybe.orElse model.selectedChange

        modelWithMigration =
            { model | questionnaireMigration = newQuestionnaireMigration }

        ( newModel, scrollCmd ) =
            handleSelectChange appState modelWithMigration nextChange

        putCmd =
            putCurrentResolvedIds wrapMsg appState newModel
    in
    ( newModel, Cmd.batch [ scrollCmd, putCmd ] )


handleUndoResolveCurrentChange : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleUndoResolveCurrentChange wrapMsg appState model =
    let
        newQuestionnaireMigration =
            model.selectedChange
                |> Maybe.map (QuestionChange.getQuestionUuid >> removeQuestionUuid)
                |> Maybe.withDefault model.questionnaireMigration

        removeQuestionUuid questionUuid =
            ActionResult.map (QuestionnaireMigration.removeResolvedQuestion questionUuid) model.questionnaireMigration

        newModel =
            { model | questionnaireMigration = newQuestionnaireMigration }
    in
    ( newModel, putCurrentResolvedIds wrapMsg appState newModel )


handleFinalizeMigration : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleFinalizeMigration wrapMsg appState model =
    ( model
    , Cmd.map wrapMsg <|
        QuestionnairesApi.deleteQuestionnaireMigration model.questionnaireUuid appState FinalizeMigrationCompleted
    )


handleFinalizeMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
handleFinalizeMigrationCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| Questionnaires <| Detail model.questionnaireUuid )

        _ ->
            ( model, Cmd.none )



-- Helpers


putCurrentResolvedIds : (Msg -> Msgs.Msg) -> AppState -> Model -> Cmd Msgs.Msg
putCurrentResolvedIds wrapMsg appState model =
    let
        createCmd migration =
            Cmd.map wrapMsg <|
                QuestionnairesApi.putQuestionnaireMigration
                    model.questionnaireUuid
                    (QuestionnaireMigration.encode migration)
                    appState
                    PutQuestionnaireMigrationCompleted
    in
    model.questionnaireMigration
        |> ActionResult.map createCmd
        |> ActionResult.withDefault Cmd.none


setResult : AppState -> ActionResult QuestionnaireMigration -> Model -> Model
setResult appState migration model =
    let
        questionnaireModel =
            case migration of
                Success m ->
                    Just <| initialModel appState m.newQuestionnaire [] []

                _ ->
                    Nothing
    in
    initializeChangeList appState
        { model
            | questionnaireMigration = migration
            , questionnaireModel = questionnaireModel
        }


scrollToQuestion : Model -> Cmd msg
scrollToQuestion model =
    model.selectedChange
        |> Maybe.map (QuestionChange.getQuestionUuid >> Ports.scrollIntoView)
        |> Maybe.withDefault Cmd.none
