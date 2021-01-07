module Wizard.Projects.Detail.Components.NewDocument exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Html exposing (..)
import Html.Attributes exposing (class)
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Document exposing (Document)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.SummaryReport exposing (SummaryReport)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg)
import Shared.Setters exposing (setSelected)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.SummaryReport exposing (viewIndications)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.NewDocument"



-- MODEL


type alias Model =
    { summaryReport : ActionResult SummaryReport
    , form : Form FormError DocumentCreateForm
    , templateTypeHintInputModel : TypeHintInput.Model TemplateSuggestion
    , savingDocument : ActionResult String
    }


initialModel : { q | name : String, template : Maybe TemplateSuggestion, formatUuid : Maybe Uuid } -> Model
initialModel questionnaire =
    { summaryReport = Loading
    , form = DocumentCreateForm.init questionnaire
    , templateTypeHintInputModel = setSelected questionnaire.template <| TypeHintInput.init "templateId"
    , savingDocument = Unset
    }



-- UPDATE


type Msg
    = GetSummaryReportComplete (Result ApiError SummaryReport)
    | FormMsg Form.Msg
    | TemplateTypeHintInputMsg (TypeHintInput.Msg TemplateSuggestion)
    | PostDocumentCompleted (Result ApiError Document)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState questionnaireUuid =
    QuestionnairesApi.getSummaryReport questionnaireUuid appState GetSummaryReportComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , packageId : String
    , documentsNavigateCmd : Cmd msg
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        GetSummaryReportComplete result ->
            handleGetSummaryReportCompleted appState model result

        FormMsg formMsg ->
            handleForm cfg formMsg appState model

        TemplateTypeHintInputMsg typeHintInputMsg ->
            handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted cfg appState model result



-- Handlers


handleGetSummaryReportCompleted : AppState -> Model -> Result ApiError SummaryReport -> ( Model, Cmd msg )
handleGetSummaryReportCompleted appState model result =
    let
        newSummaryReport =
            case result of
                Ok summaryReport ->
                    Success summaryReport

                Err error ->
                    ApiError.toActionResult appState (lg "apiError.questionnaires.summaryReport.fetchError" appState) error
    in
    ( { model | summaryReport = newSummaryReport }, Cmd.none )


handleForm : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleForm cfg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    DocumentCreateForm.encode cfg.questionnaireUuid form

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        DocumentsApi.postDocument body appState PostDocumentCompleted
            in
            ( { model | savingDocument = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update DocumentCreateForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handleTemplateTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg TemplateSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        formMsg =
            cfg.wrapMsg << FormMsg << Form.Input "templateId" Form.Select << Field.String

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << TemplateTypeHintInputMsg
            , getTypeHints = TemplatesApi.getTemplatesFor cfg.packageId
            , getError = lg "apiError.packages.getListError" appState
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( templateTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg appState model.templateTypeHintInputModel
    in
    ( { model | templateTypeHintInputModel = templateTypeHintInputModel }, cmd )


handlePostDocumentCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError Document -> ( Model, Cmd msg )
handlePostDocumentCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( model, cfg.documentsNavigateCmd )

        Err error ->
            ( { model | savingDocument = ApiError.toActionResult appState (lg "apiError.documents.postError" appState) error }, Cmd.none )



-- VIEW


view : AppState -> QuestionnaireDetail -> Model -> Html Msg
view appState questionnaire model =
    Page.actionResultView appState (viewFormState appState questionnaire.uuid model) model.summaryReport


viewFormState : AppState -> Uuid -> Model -> SummaryReport -> Html Msg
viewFormState appState questionnaireUuid model summaryReport =
    div [ class "Plans__Detail__Content Plans__Detail__Content--NewDocument" ]
        [ div [ detailClass "container" ]
            [ Page.header (l_ "header.title" appState) []
            , div []
                [ FormResult.view appState model.savingDocument
                , formView appState model summaryReport
                , FormActions.view appState
                    (Routes.ProjectsRoute <| DetailRoute questionnaireUuid <| PlanDetailRoute.Documents PaginationQueryString.empty)
                    (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingDocument (FormMsg Form.Submit) False)
                ]
            ]
        ]


formView : AppState -> Model -> SummaryReport -> Html Msg
formView appState model summaryReport =
    let
        cfg =
            { viewItem = TypeHintItem.templateSuggestion appState
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            }

        nameInput =
            FormGroup.input appState model.form "name" <| lg "document.name" appState

        templateInput =
            TypeHintInput.view appState cfg model.templateTypeHintInputModel

        formatInput =
            case model.templateTypeHintInputModel.selected of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState selectedTemplate.formats model.form "formatUuid" <| lg "template.format" appState

                _ ->
                    emptyNode
    in
    div []
        [ Html.map FormMsg <| nameInput
        , div [ class "form-group" ] [ viewIndications appState summaryReport.totalReport.indications ]
        , FormGroup.formGroupCustom templateInput appState model.form "templateId" <| lg "questionnaire.template" appState
        , Html.map FormMsg <| formatInput
        ]
