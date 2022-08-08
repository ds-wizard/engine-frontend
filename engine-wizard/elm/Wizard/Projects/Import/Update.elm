module Wizard.Projects.Import.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Random exposing (Seed)
import Shared.Api.QuestionnaireImporters as QuestionnaireImportersApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setQuestionnaire, setQuestionnaireImporter)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
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
    in
    case msg of
        GetQuestionnaireComplete result ->
            withSeed <|
                applyResult appState
                    { setResult = setQuestionnaire
                    , defaultError = lg "apiError.questionnaires.getError" appState
                    , model = model
                    , result = result
                    }

        GetQuestionnaireImporterComplete result ->
            case result of
                Ok importer ->
                    withSeed <|
                        ( setQuestionnaireImporter (Success importer) model
                        , Ports.openImporter importer.url
                        )

                Err error ->
                    withSeed <|
                        ( setQuestionnaireImporter (ApiError.toActionResult appState (lg "apiError.questionnaireImporters.getError" appState) error) model
                        , getResultCmd result
                        )

        GotImporterData data ->
            case model.questionnaire of
                Success questionnaire ->
                    let
                        ( newSeed, importResult ) =
                            Importer.convertToQuestionnaireEvents appState questionnaire data

                        newModel =
                            { model | importResult = Just importResult }
                    in
                    ( newSeed, newModel, Cmd.none )

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
                        , cmdNavigate appState (Routes.projectsDetailQuestionnaire model.uuid)
                        )

                Err error ->
                    withSeed
                        ( { model | importing = ApiError.toActionResult appState (lg "apiError.questionnaires.putContentError" appState) error }
                        , Cmd.none
                        )
