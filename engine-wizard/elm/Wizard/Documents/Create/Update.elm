module Wizard.Documents.Create.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Form.Field as Field
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Document exposing (Document)
import Shared.Data.Package exposing (Package)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Template exposing (Template)
import Shared.Data.Template.TemplateState as TemplateState
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setQuestionnaire, setTemplates)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm
import Wizard.Documents.Create.Models exposing (Model)
import Wizard.Documents.Create.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState questionnaireUuid =
    QuestionnairesApi.getQuestionnaire questionnaireUuid appState GetQuestionnaireCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted wrapMsg appState model result

        GetTemplatesCompleted result ->
            handleGetTemplatesCompleted appState model result

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted appState model result



-- Handlers


handleGetQuestionnaireCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted wrapMsg appState model result =
    let
        ( newModel, cmd ) =
            applyResult
                { setResult = setQuestionnaire
                , defaultError = lg "apiError.questionnaires.getError" appState
                , model = model
                , result = result
                }
    in
    case newModel.questionnaire of
        Success questionnaire ->
            let
                formMsg =
                    Form.Input "name" Form.Text (Field.String questionnaire.name)

                form =
                    Form.update DocumentCreateForm.validation formMsg newModel.form
            in
            ( { newModel | form = form }
            , Cmd.map wrapMsg <| TemplatesApi.getTemplatesFor questionnaire.package.id appState GetTemplatesCompleted
            )

        _ ->
            ( newModel, cmd )


handleGetTemplatesCompleted : AppState -> Model -> Result ApiError (List Template) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetTemplatesCompleted appState model result =
    let
        ( newModel, cmd ) =
            applyResult
                { setResult = setTemplates
                , defaultError = lg "apiError.templates.getListError" appState
                , model = model
                , result = result
                }

        form =
            case newModel.templates of
                Success (template :: []) ->
                    if template.state /= TemplateState.UnsupportedMetamodelVersion then
                        let
                            setFormat =
                                case ( List.length template.formats, List.head template.formats ) of
                                    ( 1, Just format ) ->
                                        Form.update DocumentCreateForm.validation (Form.Input "formatUuid" Form.Text (Field.String (Uuid.toString format.uuid)))

                                    _ ->
                                        identity
                        in
                        newModel.form
                            |> Form.update DocumentCreateForm.validation (Form.Input "templateId" Form.Text (Field.String template.id))
                            |> setFormat

                    else
                        newModel.form

                _ ->
                    newModel.form
    in
    ( { newModel | form = form }, cmd )


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.questionnaire ) of
        ( Form.Submit, Just form, Success questionnaire ) ->
            let
                body =
                    DocumentCreateForm.encode questionnaire.uuid form

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
            ( newModel, Cmd.none )


handlePostDocumentCompleted : AppState -> Model -> Result ApiError Document -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostDocumentCompleted appState model result =
    case result of
        Ok document ->
            ( model
            , cmdNavigate appState <| Routes.DocumentsRoute <| IndexRoute (Maybe.map .uuid document.questionnaire) PaginationQueryString.empty
            )

        Err error ->
            ( { model | savingDocument = ApiError.toActionResult (lg "apiError.documents.postError" appState) error }
            , getResultCmd result
            )
