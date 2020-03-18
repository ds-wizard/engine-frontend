module Wizard.Documents.Create.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (applyResult, applyResultCmd, getResultCmd)
import Wizard.Common.Api.Documents as DocumentsApi
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.Api.Templates as TemplatesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Setters exposing (setQuestionnaires, setTemplates)
import Wizard.Documents.Common.Document exposing (Document)
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Documents.Create.Models exposing (Model)
import Wizard.Documents.Create.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.Msgs
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Cmd Msg
fetchData appState =
    QuestionnairesApi.getQuestionnaires appState GetQuestionnairesCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnairesCompleted result ->
            handleGetQuestionnairesCompleted wrapMsg appState model result

        GetTemplatesCompleted result ->
            handleGetTemplatesCompleted appState model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted appState model result



-- Handlers


handleGetQuestionnairesCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError (List Questionnaire) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnairesCompleted wrapMsg appState model result =
    let
        ( newModel, cmd ) =
            applyResult
                { setResult = setQuestionnaires
                , defaultError = lg "apiError.questionnaires.getListError" appState
                , model = model
                , result = result
                }
    in
    case getSelectedQuestionnaire newModel of
        Just questionnaire ->
            ( { newModel | lastFetchedTemplatesFor = Just questionnaire.uuid }
            , Cmd.map wrapMsg <|
                TemplatesApi.getTemplatesFor questionnaire.package.id appState GetTemplatesCompleted
            )

        Nothing ->
            ( newModel, cmd )


handleGetTemplatesCompleted : AppState -> Model -> Result ApiError (List Template) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetTemplatesCompleted appState model result =
    applyResult
        { setResult = setTemplates
        , defaultError = lg "apiError.templates.getListError" appState
        , model = model
        , result = result
        }


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    DocumentCreateForm.encode form

                cmd =
                    Cmd.map wrapMsg <|
                        DocumentsApi.postDocument body appState PostDocumentCompleted
            in
            ( { model | savingDocument = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update DocumentCreateForm.validation formMsg model.form }
            in
            case getSelectedQuestionnaire newModel of
                Just questionnaire ->
                    if needFetchTemplates model questionnaire.uuid then
                        ( { newModel
                            | lastFetchedTemplatesFor = Just questionnaire.uuid
                            , templates = Loading
                          }
                        , Cmd.map wrapMsg <|
                            TemplatesApi.getTemplatesFor questionnaire.package.id appState GetTemplatesCompleted
                        )

                    else
                        ( newModel, Cmd.none )

                _ ->
                    ( newModel, Cmd.none )


handlePostDocumentCompleted : AppState -> Model -> Result ApiError Document -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostDocumentCompleted appState model result =
    case result of
        Ok document ->
            ( model
            , cmdNavigate appState <| Routes.DocumentsRoute <| IndexRoute <| Maybe.map .uuid document.questionnaire
            )

        Err error ->
            ( { model | savingDocument = ApiError.toActionResult (lg "apiError.documents.postError" appState) error }
            , getResultCmd result
            )



-- Helpers


getSelectedQuestionnaire : Model -> Maybe Questionnaire
getSelectedQuestionnaire model =
    let
        findQuestionnaire qUuid =
            ActionResult.withDefault [] model.questionnaires
                |> List.filter (\q -> q.uuid == qUuid)
                |> List.head
    in
    (Form.getFieldAsString "questionnaireUuid" model.form).value
        |> Maybe.andThen findQuestionnaire


needFetchTemplates : Model -> String -> Bool
needFetchTemplates model questionnaireUuid =
    model.lastFetchedTemplatesFor /= Just questionnaireUuid
