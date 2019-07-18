module Questionnaires.CreateMigration.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Packages as PackagesApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Setters exposing (setPackages, setQuestionnaire)
import Form
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)
import Msgs
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)
import Questionnaires.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm
import Questionnaires.CreateMigration.Models exposing (Model)
import Questionnaires.CreateMigration.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (cmdNavigate)
import Utils exposing (withNoCmd)


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    let
        getPackagesCmd =
            PackagesApi.getPackages appState GetPackagesCompleted

        getQuestionnaireCmd =
            QuestionnairesApi.getQuestionnaire uuid appState GetQuestionnaireCompleted
    in
    Cmd.batch [ getPackagesCmd, getQuestionnaireCmd ]


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AddTag tagUuid ->
            handleAddTag model tagUuid

        RemoveTag tagUuid ->
            handleRemoveTag model tagUuid

        GetPackagesCompleted result ->
            handleGetPackagesCompleted model result

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        SelectPackage packageId ->
            handleSelectPackage model packageId

        PostMigrationCompleted result ->
            handlePostMigrationCompleted appState model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted model result



-- Handlers


handleAddTag : Model -> String -> ( Model, Cmd Msgs.Msg )
handleAddTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = tagUuid :: model.selectedTags }


handleRemoveTag : Model -> String -> ( Model, Cmd Msgs.Msg )
handleRemoveTag model tagUuid =
    withNoCmd <|
        { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }


handleGetPackagesCompleted : Model -> Result ApiError (List Package) -> ( Model, Cmd Msgs.Msg )
handleGetPackagesCompleted model result =
    preselectKnowledgeModel <|
        applyResult
            { setResult = setPackages
            , defaultError = "Unable to get packages"
            , model = model
            , result = result
            }


handleGetQuestionnaireCompleted : Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Msgs.Msg )
handleGetQuestionnaireCompleted model result =
    preselectKnowledgeModel <|
        applyResult
            { setResult = setQuestionnaire
            , defaultError = "Unable to get packages"
            , model = model
            , result = result
            }


handleForm : (Msg -> Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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
