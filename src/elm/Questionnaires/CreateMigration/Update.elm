module Questionnaires.CreateMigration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Packages as PackagesApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Common.Setters exposing (setPackages, setQuestionnaire)
import Form
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)
import Msgs
import Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)
import Questionnaires.CreateMigration.Models exposing (Model, encodeQuestionnaireMigrationCreateForm, questionnaireMigrationCreateFormValidation)
import Questionnaires.CreateMigration.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    let
        getPackagesCmd =
            PackagesApi.getPackages appState GetPackagesCompleted

        getQuestionnaireCmd =
            QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
    in
    Cmd.batch [ getPackagesCmd, getQuestionnaireCmd ]


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        AddTag tagUuid ->
            ( { model | selectedTags = tagUuid :: model.selectedTags }, Cmd.none )

        RemoveTag tagUuid ->
            ( { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }, Cmd.none )

        GetPackagesCompleted result ->
            handleGetPackagesCompleted model result

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        SelectPackage packageId ->
            handleSelectPackage model packageId

        PostMigrationCompleted result ->
            handlePostMigrationCompleted appState model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted model result



-- Handlers


handleGetPackagesCompleted : Model -> Result ApiError (List Package) -> ( Model, Cmd Msgs.Msg )
handleGetPackagesCompleted model result =
    applyResult
        { setResult = setPackages
        , defaultError = "Unable to get packages"
        , model = model
        , result = result
        }
        |> preselectKnowledgeModel


handleGetQuestionnaireCompleted : Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    applyResult
        { setResult = setQuestionnaire
        , defaultError = "Unable to get packages"
        , model = model
        , result = result
        }
        |> preselectKnowledgeModel


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    encodeQuestionnaireMigrationCreateForm model.selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.fetchQuestionnaireMigration model.questionnaireUuid body appState PostMigrationCompleted
            in
            ( { model | savingMigration = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update questionnaireMigrationCreateFormValidation formMsg model.form }
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


handleSelectPackage : Model -> String -> ( Model, Cmd Msgs.Msg )
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


handlePostMigrationCompleted : AppState -> Model -> Result ApiError QuestionnaireMigration -> ( Model, Cmd Msgs.Msg )
handlePostMigrationCompleted appState model result =
    case result of
        Ok migration ->
            ( model, cmdNavigate appState.key <| Routing.Questionnaires << Migration <| migration.newQuestionnaire.uuid )

        Err error ->
            ( { model | savingMigration = getServerError error "Questionnaire migration could not be created." }
            , getResultCmd result
            )


handleGetKnowledgeModelPreviewCompleted : Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
handleGetKnowledgeModelPreviewCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelPreview = Success knowledgeModel }

                Err error ->
                    { model | knowledgeModelPreview = getServerError error "Unable to get knowledge model tags." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )



-- Helpers


preselectKnowledgeModel : ( Model, Cmd Msgs.Msg ) -> ( Model, Cmd Msgs.Msg )
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
