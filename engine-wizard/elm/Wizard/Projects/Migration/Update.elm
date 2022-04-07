module Wizard.Projects.Migration.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent
import Shared.Data.QuestionnaireMigration as QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (getUuid)
import Time
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionChange as QuestionChange exposing (QuestionChange)
import Wizard.Projects.Migration.Models exposing (Model, initializeChangeList)
import Wizard.Projects.Migration.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    QuestionnairesApi.getQuestionnaireMigration uuid appState GetQuestionnaireMigrationCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        GetQuestionnaireMigrationCompleted result ->
            handleGetQuestionnaireMigrationCompleted appState model result

        PutQuestionnaireMigrationCompleted result ->
            withSeed <| handlePutQuestionnaireMigrationCompleted appState model result

        SelectChange change ->
            handleSelectChange appState model (Just change)

        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg wrapMsg appState model questionnaireMsg

        ResolveCurrentChange ->
            handleResolveCurrentChange wrapMsg appState model

        UndoResolveCurrentChange ->
            withSeed <| handleUndoResolveCurrentChange wrapMsg appState model

        FinalizeMigration ->
            withSeed <| handleFinalizeMigration wrapMsg appState model

        FinalizeMigrationCompleted result ->
            withSeed <| handleFinalizeMigrationCompleted appState model result

        PutQuestionnaireContentCompleted _ ->
            ( appState.seed, model, Cmd.none )



-- Handlers


handleGetQuestionnaireMigrationCompleted : AppState -> Model -> Result ApiError QuestionnaireMigration -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireMigrationCompleted appState model result =
    let
        ( modelWithMigration, cmd ) =
            applyResult appState
                { setResult = setResult appState
                , defaultError = lg "apiError.questionnaires.migrations.getError" appState
                , result = result
                , model = model
                }

        ( newSeed, newModel, scrollCmd ) =
            handleSelectChange appState modelWithMigration modelWithMigration.selectedChange
    in
    ( newSeed, newModel, Cmd.batch [ cmd, scrollCmd ] )


handlePutQuestionnaireMigrationCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutQuestionnaireMigrationCompleted appState model result =
    applyResult appState
        { setResult = \_ _ -> model
        , defaultError = lg "apiError.questionnaires.migrations.putError" appState
        , result = result
        , model = model
        }


handleSelectChange : AppState -> Model -> Maybe QuestionChange -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleSelectChange appState model mbChange =
    case mbChange of
        Just change ->
            let
                questionnaireModel =
                    Maybe.map
                        (Questionnaire.setActiveChapterUuid (QuestionChange.getChapter change).uuid)
                        model.questionnaireModel

                newModel =
                    { model
                        | selectedChange = Just change
                        , questionnaireModel = questionnaireModel
                    }
            in
            ( appState.seed, newModel, scrollToQuestion newModel )

        Nothing ->
            ( appState.seed, model, Cmd.none )


handleQuestionnaireMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Questionnaire.Msg -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleQuestionnaireMsg wrapMsg appState model questionnaireMsg =
    case model.questionnaireModel of
        Just questionnaireModel ->
            let
                ( newSeed, newQuestionnaireModel, questionnaireCmd ) =
                    Questionnaire.update
                        questionnaireMsg
                        (wrapMsg << QuestionnaireMsg)
                        (Just Wizard.Msgs.SetFullscreen)
                        appState
                        { events = [] }
                        questionnaireModel

                ( newSeed2, saveCmd ) =
                    case questionnaireMsg of
                        Questionnaire.SetLabels path value ->
                            let
                                ( uuid, newSeed2_ ) =
                                    getUuid newSeed

                                event =
                                    QuestionnaireEvent.SetLabels
                                        { uuid = uuid
                                        , path = path
                                        , value = value
                                        , createdAt = Time.millisToPosix 0
                                        , createdBy = Nothing
                                        }
                            in
                            ( newSeed2_
                            , QuestionnairesApi.putQuestionnaireContent model.questionnaireUuid
                                [ event ]
                                appState
                                PutQuestionnaireContentCompleted
                            )

                        _ ->
                            ( newSeed, Cmd.none )
            in
            ( newSeed2
            , { model | questionnaireModel = Just newQuestionnaireModel }
            , Cmd.batch
                [ questionnaireCmd
                , Cmd.map wrapMsg <| saveCmd
                ]
            )

        _ ->
            ( appState.seed, model, Cmd.none )


handleResolveCurrentChange : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
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

        ( newSeed, newModel, scrollCmd ) =
            handleSelectChange appState modelWithMigration nextChange

        putCmd =
            putCurrentResolvedIds wrapMsg appState newModel
    in
    ( newSeed, newModel, Cmd.batch [ scrollCmd, putCmd ] )


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
            let
                route =
                    case model.questionnaireMigration of
                        Success questionnaireMigration ->
                            Routes.projectsDetailQuestionnaire questionnaireMigration.oldQuestionnaire.uuid

                        _ ->
                            Routes.projectsIndex
            in
            ( model, cmdNavigate appState route )

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
                    Just <| Questionnaire.init appState m.newQuestionnaire

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
