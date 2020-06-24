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
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setQuestionnaires, setTemplates)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, applyResultTransform, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm
import Wizard.Documents.Create.Models exposing (Model)
import Wizard.Documents.Create.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        paginationQueryString =
            PaginationQueryString.withSize Nothing PaginationQueryString.empty
    in
    QuestionnairesApi.getQuestionnaires paginationQueryString appState GetQuestionnairesCompleted


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


handleGetQuestionnairesCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError (Pagination Questionnaire) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnairesCompleted wrapMsg appState model result =
    let
        ( newModel, cmd ) =
            applyResultTransform
                { setResult = setQuestionnaires
                , defaultError = lg "apiError.questionnaires.getListError" appState
                , model = model
                , result = result
                , transform = .items
                }

        form =
            case ( newModel.selectedQuestionnaire, newModel.questionnaires ) of
                ( Just questionnaireUuid, Success questionnaires ) ->
                    let
                        questionnaireName =
                            List.filter (\q -> q.uuid == questionnaireUuid) questionnaires
                                |> List.head
                                |> Maybe.map .name
                                |> Maybe.withDefault ""

                        formMsg =
                            Form.Input "name" Form.Text (Field.String questionnaireName)
                    in
                    Form.update DocumentCreateForm.validation formMsg newModel.form

                _ ->
                    newModel.form
    in
    case getSelectedQuestionnaire newModel of
        Just questionnaire ->
            ( { newModel | lastFetchedTemplatesFor = Just questionnaire.uuid, form = form }
            , Cmd.map wrapMsg <|
                TemplatesApi.getTemplatesFor questionnaire.package.id appState GetTemplatesCompleted
            )

        Nothing ->
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
                    let
                        setFormat =
                            case ( List.length template.formats, List.head template.formats ) of
                                ( 1, Just format ) ->
                                    Form.update DocumentCreateForm.validation (Form.Input "formatUuid" Form.Text (Field.String (Uuid.toString format.uuid)))

                                _ ->
                                    identity
                    in
                    newModel.form
                        |> Form.update DocumentCreateForm.validation (Form.Input "templateUuid" Form.Text (Field.String (Uuid.toString template.uuid)))
                        |> setFormat

                _ ->
                    newModel.form
    in
    ( { newModel | form = form }, cmd )


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
            , cmdNavigate appState <| Routes.DocumentsRoute <| IndexRoute (Maybe.map .uuid document.questionnaire) PaginationQueryString.empty
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
                |> List.filter (\q -> Uuid.toString q.uuid == qUuid)
                |> List.head
    in
    (Form.getFieldAsString "questionnaireUuid" model.form).value
        |> Maybe.andThen findQuestionnaire


needFetchTemplates : Model -> Uuid -> Bool
needFetchTemplates model questionnaireUuid =
    model.lastFetchedTemplatesFor /= Just questionnaireUuid
