module Wizard.Pages.Projects.ImportLegacy.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelString, setQuestionnaireImporter)
import Gettext exposing (gettext)
import Random exposing (Seed)
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeel
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData as SetReplyData
import Wizard.Api.ProjectImporters as ProjectsImportersApi
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer as Importer
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Integrations as Integrations
import Wizard.Msgs
import Wizard.Pages.Projects.ImportLegacy.Models exposing (Model)
import Wizard.Pages.Projects.ImportLegacy.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> String -> Cmd Msg
fetchData appState uuid importerId =
    Cmd.batch
        [ ProjectsApi.getQuestionnaire appState uuid GetQuestionnaireComplete
        , ProjectsImportersApi.get appState importerId GetQuestionnaireImporterComplete
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )

        openImporter newModel =
            case ( newModel.knowledgeModelString, newModel.questionnaireImporter ) of
                ( Success knowledgeModelString, Success importer ) ->
                    Integrations.openImporter
                        { url = importer.url
                        , theme = Maybe.withDefault (LookAndFeel.getTheme appState.config.lookAndFeel) appState.theme
                        , data =
                            { knowledgeModel = knowledgeModelString
                            }
                        }

                _ ->
                    Cmd.none
    in
    case msg of
        GetQuestionnaireComplete result ->
            let
                setResult r m =
                    { m
                        | questionnaire = ActionResult.map .data r
                        , questionnaireModel = ActionResult.map (Tuple.first << Questionnaire.initSimple appState << .data) r
                    }

                ( newModel, cmd ) =
                    RequestHelpers.applyResult
                        { setResult = setResult
                        , defaultError = gettext "Unable to get the project." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        , locale = appState.locale
                        }

                fetchCmd =
                    case newModel.questionnaire of
                        Success questionnaire ->
                            KnowledgeModelsApi.fetchAsString appState questionnaire.knowledgeModelPackageId questionnaire.selectedQuestionTagUuids (wrapMsg << FetchKnowledgeModelStringComplete)

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
                        , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                        )

        FetchKnowledgeModelStringComplete result ->
            let
                ( newModel, cmd ) =
                    RequestHelpers.applyResult
                        { setResult = setKnowledgeModelString
                        , defaultError = gettext "Unable to get the project." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        , locale = appState.locale
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
                                ProjectEvent.SetReply setReplyData ->
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
                                { events = []
                                , kmEditorUuid = Nothing
                                }
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
                        , Cmd.map wrapMsg <| ProjectsApi.putContent appState model.uuid importResult.questionnaireEvents PutImporterDataComplete
                        )

                Nothing ->
                    withSeed ( model, Cmd.none )

        PutImporterDataComplete result ->
            case result of
                Ok _ ->
                    withSeed
                        ( model
                        , cmdNavigate appState (Routes.projectsDetail model.uuid)
                        )

                Err error ->
                    withSeed
                        ( { model | importing = ApiError.toActionResult appState (gettext "Questionnaire changes could not be saved." appState.locale) error }
                        , Cmd.none
                        )

        ChangeSidePanel sidePanel ->
            withSeed ( { model | sidePanel = sidePanel }, Cmd.none )
