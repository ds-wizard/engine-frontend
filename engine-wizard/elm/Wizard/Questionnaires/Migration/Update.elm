module Wizard.Questionnaires.Migration.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Maybe.Extra as Maybe
import Shared.Api.Levels as LevelsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setLevels)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Questionnaire.Models exposing (initialModel)
import Wizard.Common.Questionnaire.Msgs as QuestionnaireMsgs
import Wizard.Common.Questionnaire.Update
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Questionnaires.Common.QuestionChange as QuestionChange exposing (QuestionChange)
import Wizard.Questionnaires.Migration.Models exposing (Model, initializeChangeList)
import Wizard.Questionnaires.Migration.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaireMigration uuid appState GetQuestionnaireMigrationCompleted
        , LevelsApi.getLevels appState GetLevelsCompleted
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnaireMigrationCompleted result ->
            handleGetQuestionnaireMigrationCompleted appState model result

        PutQuestionnaireMigrationCompleted result ->
            handlePutQuestionnaireMigrationCompleted appState model result

        GetLevelsCompleted result ->
            handleGetLevelsCompleted appState model result

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


handleGetQuestionnaireMigrationCompleted : AppState -> Model -> Result ApiError QuestionnaireMigration -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireMigrationCompleted appState model result =
    let
        ( modelWithMigration, cmd ) =
            applyResult
                { setResult = setResult appState
                , defaultError = lg "apiError.questionnaires.migrations.getError" appState
                , result = result
                , model = model
                }

        ( newModel, scrollCmd ) =
            handleSelectChange appState modelWithMigration modelWithMigration.selectedChange
    in
    ( newModel, Cmd.batch [ cmd, scrollCmd ] )


handlePutQuestionnaireMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutQuestionnaireMigrationCompleted appState model result =
    applyResult
        { setResult = \_ _ -> model
        , defaultError = lg "apiError.questionnaires.migrations.putError" appState
        , result = result
        , model = model
        }


handleGetLevelsCompleted : AppState -> Model -> Result ApiError (List Level) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetLevelsCompleted appState model result =
    applyResult
        { setResult = setLevels
        , defaultError = lg "apiError.levels.getListError" appState
        , result = result
        , model = model
        }


handleSelectChange : AppState -> Model -> Maybe QuestionChange -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectChange appState model mbChange =
    case mbChange of
        Just change ->
            let
                newModelWithChapter =
                    case model.questionnaireModel of
                        Just questionnaireModel ->
                            let
                                ( newQuestionnaireModel, _ ) =
                                    Wizard.Common.Questionnaire.Update.update
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


handleQuestionnaireMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> QuestionnaireMsgs.Msg -> ( Model, Cmd Wizard.Msgs.Msg )
handleQuestionnaireMsg wrapMsg appState model questionnaireMsg =
    case model.questionnaireModel of
        Just questionnaireModel ->
            let
                ( updatedQuestionnaireModel, _ ) =
                    Wizard.Common.Questionnaire.Update.update questionnaireMsg appState questionnaireModel

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


handleResolveCurrentChange : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleUndoResolveCurrentChange : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleFinalizeMigration : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFinalizeMigration wrapMsg appState model =
    ( model
    , Cmd.map wrapMsg <|
        QuestionnairesApi.completeQuestionnaireMigration model.questionnaireUuid appState FinalizeMigrationCompleted
    )


handleFinalizeMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleFinalizeMigrationCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState <| Routes.QuestionnairesRoute <| DetailRoute model.questionnaireUuid )

        _ ->
            ( model, Cmd.none )



-- Helpers


putCurrentResolvedIds : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Cmd Wizard.Msgs.Msg
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
    let
        prefix uuid =
            "#question-" ++ uuid
    in
    model.selectedChange
        |> Maybe.map (QuestionChange.getQuestionUuid >> prefix >> Ports.scrollIntoView)
        |> Maybe.withDefault Cmd.none
