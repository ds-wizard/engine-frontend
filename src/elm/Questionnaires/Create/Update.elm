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
import Questionnaires.Create.Models exposing (Model, QuestionnaireCreateForm, encodeQuestionnaireCreateForm, initQuestionnaireCreateForm, questionnaireCreateFormValidation)
import Questionnaires.Create.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))
import Result exposing (Result)
import Routing exposing (cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> AppState -> Model -> Cmd Msgs.Msg
fetchData wrapMsg appState model =
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
    Cmd.map wrapMsg <|
        Cmd.batch [ getPackagesCmd, fetchTagsCmd ]


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        AddTag tagUuid ->
            ( { model | selectedTags = tagUuid :: model.selectedTags }, Cmd.none )

        RemoveTag tagUuid ->
            ( { model | selectedTags = List.filter (\t -> t /= tagUuid) model.selectedTags }, Cmd.none )

        GetPackagesCompleted result ->
            getPackagesCompleted model result

        GetKnowledgeModelPreviewCompleted result ->
            getKnowledgeModelPreviewCompleted model result

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostQuestionnaireCompleted result ->
            postQuestionnaireCompleted appState model result


getPackagesCompleted : Model -> Result ApiError (List Package) -> ( Model, Cmd Msgs.Msg )
getPackagesCompleted model result =
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


getKnowledgeModelPreviewCompleted : Model -> Result ApiError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelPreviewCompleted model result =
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


setSelectedPackage : Model -> List Package -> Model
setSelectedPackage model packages =
    case model.selectedPackage of
        Just id ->
            if List.any (.id >> (==) id) packages then
                { model | form = initQuestionnaireCreateForm model.selectedPackage }

            else
                model

        _ ->
            model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    encodeQuestionnaireCreateForm model.selectedTags form

                cmd =
                    Cmd.map wrapMsg <|
                        QuestionnairesApi.postQuestionnaire body appState PostQuestionnaireCompleted
            in
            ( { model | savingQuestionnaire = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update questionnaireCreateFormValidation formMsg model.form }
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


getSelectedPackageId : Model -> Maybe String
getSelectedPackageId model =
    (Form.getFieldAsString "packageId" model.form).value


needFetchKnowledgeModelPreview : Model -> String -> Bool
needFetchKnowledgeModelPreview model packageId =
    model.lastFetchedPreview /= Just packageId


postQuestionnaireCompleted : AppState -> Model -> Result ApiError Questionnaire -> ( Model, Cmd Msgs.Msg )
postQuestionnaireCompleted appState model result =
    case result of
        Ok questionnaire ->
            ( model
            , cmdNavigate appState.key <| Routing.Questionnaires <| Questionnaires.Routing.Detail questionnaire.uuid
            )

        Err error ->
            ( { model | savingQuestionnaire = getServerError error "Questionnaire could not be created." }
            , getResultCmd result
            )
