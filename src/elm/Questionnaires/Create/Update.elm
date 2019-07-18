module Questionnaires.Create.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Packages as PackagesApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)
import Msgs
import Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm
import Questionnaires.Create.Models exposing (Model)
import Questionnaires.Create.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Result exposing (Result)
import Routing exposing (cmdNavigate)
import Utils exposing (withNoCmd)


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


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AddTag tagUuid ->
            handleAddTag model tagUuid

        RemoveTag tagUuid ->
            handleRemoveTag model tagUuid

        GetPackagesCompleted result ->
            handleGetPackagesCompleted model result

        GetKnowledgeModelPreviewCompleted result ->
            handleGetKnowledgeModelPreviewCompleted model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostQuestionnaireCompleted result ->
            handlePostQuestionnaireCompleted appState model result



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
    let
        newModel =
            case result of
                Ok packages ->
                    setSelectedPackage { model | packages = Success packages } packages

                Err error ->
                    { model | packages = getServerError error "Unable to get knowledge model list." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


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


handleForm : (Msg -> Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handlePostQuestionnaireCompleted : AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Msgs.Msg )
handlePostQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( model
            , cmdNavigate appState.key <| Routing.Questionnaires <| Questionnaires.Routing.Detail questionnaire.uuid
            )

        Err error ->
            ( { model | savingQuestionnaire = getServerError error "Questionnaire could not be created." }
            , getResultCmd result
            )



-- Helpers


setSelectedPackage : Model -> List Package -> Model
setSelectedPackage model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = QuestionnaireCreateForm.init model.selectedPackage }

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
