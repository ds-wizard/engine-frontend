module Wizard.Pages.Projects.Import.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelString)
import Gettext exposing (gettext)
import Random exposing (Seed)
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData as SetReplyData
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer as Importer
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Import.Models exposing (Model)
import Wizard.Pages.Projects.Import.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    ProjectsApi.getQuestionnaire appState uuid GetQuestionnaireComplete


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        GetQuestionnaireComplete result ->
            let
                setResult r m =
                    { m
                        | project = ActionResult.map .data r
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
                    case newModel.project of
                        Success questionnaire ->
                            KnowledgeModelsApi.fetchAsString appState questionnaire.knowledgeModelPackageId questionnaire.selectedQuestionTagUuids (wrapMsg << FetchKnowledgeModelStringComplete)

                        _ ->
                            Cmd.none
            in
            withSeed <|
                ( newModel, Cmd.batch [ cmd, fetchCmd ] )

        FetchKnowledgeModelStringComplete result ->
            withSeed <|
                RequestHelpers.applyResult
                    { setResult = setKnowledgeModelString
                    , defaultError = gettext "Unable to get the project." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    , locale = appState.locale
                    }

        GotImporterData data ->
            case ( model.project, model.questionnaireModel ) of
                ( Success questionnaire, Success questionnaireModel ) ->
                    let
                        ( newSeed, importResult ) =
                            Importer.convertToQuestionnaireEvents appState questionnaire (Ok data)

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
