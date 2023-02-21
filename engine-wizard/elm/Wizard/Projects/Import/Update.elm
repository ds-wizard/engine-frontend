module Wizard.Projects.Import.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Json.Encode as E
import Random exposing (Seed)
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.QuestionnaireImporters as QuestionnaireImportersApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData as SetReplyData
import Shared.Error.ApiError as ApiError
import Shared.Setters exposing (setKnowledgeModelString, setQuestionnaireImporter)
import Shared.Utils exposing (flip)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.Importer as Importer
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Import.Models exposing (Model)
import Wizard.Projects.Import.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> String -> Cmd Msg
fetchData appState uuid importerId =
    Cmd.batch
        [ QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireComplete
        , QuestionnaireImportersApi.getQuestionnaireImporter importerId appState GetQuestionnaireImporterComplete
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )

        openImporter newModel =
            case ( newModel.knowledgeModelString, newModel.questionnaireImporter ) of
                ( Success knowledgeModelString, Success importer ) ->
                    Ports.openImporter <|
                        E.object
                            [ ( "url", E.string importer.url )
                            , ( "knowledgeModel", E.string knowledgeModelString )
                            ]

                _ ->
                    Cmd.none
    in
    case msg of
        GetQuestionnaireComplete result ->
            let
                setResult r m =
                    { m
                        | questionnaire = r
                        , questionnaireModel = ActionResult.map (Tuple.first << flip (Questionnaire.init appState) Nothing) r
                    }

                ( newModel, cmd ) =
                    applyResult appState
                        { setResult = setResult
                        , defaultError = gettext "Unable to get the project." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        }

                fetchCmd =
                    case newModel.questionnaire of
                        Success questionnaire ->
                            KnowledgeModelsApi.fetchAsString questionnaire.package.id questionnaire.selectedQuestionTagUuids appState (wrapMsg << FetchKnowledgeModelStringComplete)

                        _ ->
                            Cmd.none
            in
            withSeed <|
                ( newModel, Cmd.batch [ cmd, fetchCmd ] )

        GetQuestionnaireImporterComplete result ->
            case result of
                Ok importer ->
                    let
                        newModel =
                            setQuestionnaireImporter (Success importer) model
                    in
                    withSeed <|
                        ( newModel
                        , openImporter newModel
                        )

                Err error ->
                    withSeed <|
                        ( setQuestionnaireImporter (ApiError.toActionResult appState (gettext "Unable to get importer." appState.locale) error) model
                        , getResultCmd Wizard.Msgs.logoutMsg result
                        )

        FetchKnowledgeModelStringComplete result ->
            let
                ( newModel, cmd ) =
                    applyResult appState
                        { setResult = setKnowledgeModelString
                        , defaultError = gettext "Unable to get the project." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        }
            in
            withSeed ( newModel, Cmd.batch [ cmd, openImporter newModel ] )

        GotImporterData data ->
            case ( model.questionnaire, model.questionnaireModel ) of
                ( Success questionnaire, Success questionnaireModel ) ->
                    let
                        ( newSeed, importResult ) =
                            Importer.convertToQuestionnaireEvents appState questionnaire data

                        updateQuestionnaire event qm =
                            case event of
                                QuestionnaireEvent.SetReply setReplyData ->
                                    qm
                                        |> Questionnaire.addEvent event
                                        |> Questionnaire.setReply setReplyData.path (SetReplyData.toReply setReplyData)

                                _ ->
                                    qm

                        newQuestionnaireModel =
                            List.foldl updateQuestionnaire questionnaireModel importResult.questionnaireEvents

                        newModel =
                            { model
                                | importResult = Just importResult
                                , questionnaireModel = Success newQuestionnaireModel
                            }
                    in
                    ( newSeed, newModel, Cmd.none )

                _ ->
                    withSeed ( model, Cmd.none )

        QuestionnaireMsg questionnaireMsg ->
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        ( newSeed, newQuestionnaireModel, questionnaireCmd ) =
                            Questionnaire.update questionnaireMsg
                                (wrapMsg << QuestionnaireMsg)
                                Nothing
                                appState
                                { events = [] }
                                questionnaireModel
                    in
                    ( newSeed
                    , { model | questionnaireModel = Success newQuestionnaireModel }
                    , questionnaireCmd
                    )

                _ ->
                    withSeed ( model, Cmd.none )

        PutImportData ->
            case model.importResult of
                Just importResult ->
                    withSeed
                        ( { model | importing = Loading }
                        , Cmd.map wrapMsg <| QuestionnairesApi.putQuestionnaireContent model.uuid importResult.questionnaireEvents appState PutImporterDataComplete
                        )

                Nothing ->
                    withSeed ( model, Cmd.none )

        PutImporterDataComplete result ->
            case result of
                Ok _ ->
                    withSeed
                        ( model
                        , cmdNavigate appState (Routes.projectsDetailQuestionnaire model.uuid Nothing)
                        )

                Err error ->
                    withSeed
                        ( { model | importing = ApiError.toActionResult appState (gettext "Questionnaire changes could not be saved." appState.locale) error }
                        , Cmd.none
                        )

        ChangeSidePanel sidePanel ->
            withSeed ( { model | sidePanel = sidePanel }, Cmd.none )
