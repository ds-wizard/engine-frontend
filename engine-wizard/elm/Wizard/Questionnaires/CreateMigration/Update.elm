module Wizard.Questionnaires.CreateMigration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Common.Api.Packages as PackagesApi
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
import Wizard.Common.Setters exposing (setPackages, setQuestionnaire)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.Msgs
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Questionnaires.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm
import Wizard.Questionnaires.CreateMigration.Models exposing (Model)
import Wizard.Questionnaires.CreateMigration.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)
import Wizard.Utils exposing (withNoCmd)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    let
        getPackagesCmd =
            PackagesApi.getPackages appState GetPackagesCompleted

        getQuestionnaireCmd =
            QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
    in
    Cmd.batch [ getPackagesCmd, getQuestionnaireCmd ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AddTag tagUuid ->
            handleAddTag model tagUuid

        RemoveTag tagUuid ->
            handleRemoveTag model tagUuid

        GetPackagesCompleted result ->
            handleGetPackagesCompleted appState model result

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        SelectPackage packageId ->
            handleSelectPackage model packageId

        PostMigrationCompleted result ->
            handlePostMigrationCompleted appState model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted appState model result



-- Handlers


handleAddTag : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleAddTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = tagUuid :: model.selectedTags }


handleRemoveTag : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleRemoveTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }


handleGetPackagesCompleted : AppState -> Model -> Result ApiError (List Package) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetPackagesCompleted appState model result =
    preselectKnowledgeModel <|
        applyResult
            { setResult = setPackages
            , defaultError = lg "apiError.packages.getListError" appState
            , model = model
            , result = result
            }


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    preselectKnowledgeModel <|
        applyResult
            { setResult = setQuestionnaire
            , defaultError = lg "apiError.questionnaires.getError" appState
            , model = model
            , result = result
            }


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireMigrationCreateForm.encode model.selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.fetchQuestionnaireMigration model.questionnaireUuid body appState PostMigrationCompleted
            in
            ( { model | savingMigration = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update QuestionnaireMigrationCreateForm.validation formMsg model.form }
            in
            case getSelectedPackageId newModel of
                Just packageId ->
                    if needFetchKnowledgeModelPreview model packageId then
                        ( { newModel
                            | lastFetchedPreview = Just packageId
                            , knowledgeModelPreview = Loading
                            , selectedTags = []
                          }
                        , Cmd.map wrapMsg <|
                            KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState GetKnowledgeModelPreviewCompleted
                        )

                    else
                        ( newModel, Cmd.none )

                Nothing ->
                    ( newModel, Cmd.none )


handleSelectPackage : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectPackage model packageId =
    let
        selectPackage =
            List.head << List.filter (.id >> (==) packageId)

        selectedPackage =
            model.packages
                |> ActionResult.map selectPackage
                |> ActionResult.withDefault Nothing
    in
    ( { model | selectedPackage = selectedPackage }, Cmd.none )


handlePostMigrationCompleted : AppState -> Model -> Result ApiError QuestionnaireMigration -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostMigrationCompleted appState model result =
    case result of
        Ok migration ->
            ( model, cmdNavigate appState <| Routes.QuestionnairesRoute << MigrationRoute <| migration.newQuestionnaire.uuid )

        Err error ->
            ( { model | savingMigration = ApiError.toActionResult (lg "apiError.questionnaires.migrations.postError" appState) error }
            , getResultCmd result
            )


handleGetKnowledgeModelPreviewCompleted : AppState -> Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetKnowledgeModelPreviewCompleted appState model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelPreview = Success knowledgeModel }

                Err error ->
                    { model | knowledgeModelPreview = ApiError.toActionResult (lg "apiError.knowledgeModels.tags.getError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )



-- Helpers


preselectKnowledgeModel : ( Model, Cmd Wizard.Msgs.Msg ) -> ( Model, Cmd Wizard.Msgs.Msg )
preselectKnowledgeModel ( model, cmd ) =
    let
        isSamePackage package1 package2 =
            package1.organizationId == package2.organizationId && package1.kmId == package2.kmId

        newModel =
            case ActionResult.combine model.questionnaire model.packages of
                Success ( questionnaire, packages ) ->
                    { model | selectedPackage = List.head <| List.filter (isSamePackage questionnaire.package) packages }

                _ ->
                    model
    in
    ( newModel, cmd )


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    (Form.getFieldAsString "packageId" model.form).value


needFetchKnowledgeModelPreview : Model -> String -> Bool
needFetchKnowledgeModelPreview model packageId =
    model.lastFetchedPreview /= Just packageId
