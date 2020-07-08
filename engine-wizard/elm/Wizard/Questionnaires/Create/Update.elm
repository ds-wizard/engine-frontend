module Wizard.Questionnaires.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Form
import Result exposing (Result)
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package exposing (Package)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (withNoCmd)
import Uuid
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm
import Wizard.Questionnaires.Create.Models exposing (Model)
import Wizard.Questionnaires.Create.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        getPackagesCmd =
            PackagesApi.getPackages appState GetPackagesCompleted

        fetchTagsCmd =
            case model.selectedPackage of
                Just packageId ->
                    KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState GetKnowledgeModelPreviewCompleted

                Nothing ->
                    Cmd.none
    in
    Cmd.batch [ getPackagesCmd, fetchTagsCmd ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AddTag tagUuid ->
            handleAddTag model tagUuid

        RemoveTag tagUuid ->
            handleRemoveTag model tagUuid

        GetPackagesCompleted result ->
            handleGetPackagesCompleted appState model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted appState model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostQuestionnaireCompleted result ->
            handlePostQuestionnaireCompleted appState model result



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
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage appState { model | packages = Success packages } packages

                Err error ->
                    { model | packages = ApiError.toActionResult (lg "apiError.packages.getListError" appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


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


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    QuestionnaireCreateForm.encode model.selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.postQuestionnaire body appState PostQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update QuestionnaireCreateForm.validation formMsg model.form }
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


handlePostQuestionnaireCompleted : AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( model
            , cmdNavigate appState <| Routes.QuestionnairesRoute <| DetailRoute questionnaire.uuid
            )

        Err error ->
            ( { model | savingQuestionnaire = ApiError.toActionResult (lg "apiError.questionnaires.postError" appState) error }
            , getResultCmd result
            )



-- Helpers


setSelectedPackage : AppState -> Model -> List Package -> Model
setSelectedPackage appState model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = QuestionnaireCreateForm.init appState model.selectedPackage }

            else
                model

        _ ->
            model


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    (Form.getFieldAsString "packageId" model.form).value


needFetchKnowledgeModelPreview : Model -> String -> Bool
needFetchKnowledgeModelPreview model packageId =
    model.lastFetchedPreview /= Just packageId
