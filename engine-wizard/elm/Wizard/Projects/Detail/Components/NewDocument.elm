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
import Html exposing (..)
import Html.Attributes exposing (class)
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Document exposing (Document)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.SummaryReport exposing (SummaryReport)
import Shared.Data.TemplateDetail exposing (TemplateDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lg)
import Shared.Setters exposing (setTemplate)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.SummaryReport exposing (viewIndications)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)
import Wizard.Projects.Detail.Components.Shared.TemplateNotSet as TemplateNotSet
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.NewDocument"



-- MODEL


type alias Model =
    { template : ActionResult TemplateDetail
    , summaryReport : ActionResult SummaryReport
    , state : State
    , savingDocument : ActionResult String
    }


type State
    = FormState (Form FormError DocumentCreateForm)
    | TemplateNotSetState


initialModel : { q | name : String, templateId : Maybe String, formatUuid : Maybe Uuid } -> Model
initialModel questionnaire =
    let
        state =
            case questionnaire.templateId of
                Just _ ->
                    FormState <|
                        DocumentCreateForm.init questionnaire

                Nothing ->
                    TemplateNotSetState
    in
    { template = Loading
    , summaryReport = Loading
    , state = state
    , savingDocument = Unset
    }



-- UPDATE


type Msg
    = GetTemplateComplete (Result ApiError TemplateDetail)
    | GetSummaryReportComplete (Result ApiError SummaryReport)
    | FormMsg Form.Msg
    | PostDocumentCompleted (Result ApiError Document)


fetchData : AppState -> Uuid -> Maybe String -> Cmd Msg
fetchData appState questionnaireUuid mbTemplateId =
    case mbTemplateId of
        Just templateId ->
            Cmd.batch
                [ QuestionnairesApi.getSummaryReport questionnaireUuid appState GetSummaryReportComplete
                , TemplatesApi.getTemplate templateId appState GetTemplateComplete
                ]

        _ ->
            Cmd.none


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaireUuid : Uuid
    , documentsNavigateCmd : Cmd msg
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        GetTemplateComplete result ->
            handleGetTemplatesCompleted appState model result

        GetSummaryReportComplete result ->
            handleGetSummaryReportCompleted appState model result

        FormMsg formMsg ->
            handleForm cfg formMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted cfg appState model result



-- Handlers


handleGetTemplatesCompleted : AppState -> Model -> Result ApiError TemplateDetail -> ( Model, Cmd msg )
handleGetTemplatesCompleted appState model result =
    let
        newTemplate =
            case result of
                Ok template ->
                    Success template

                Err error ->
                    ApiError.toActionResult (lg "apiError.templates.getError" appState) error
    in
    ( setTemplate newTemplate model, Cmd.none )


handleGetSummaryReportCompleted : AppState -> Model -> Result ApiError SummaryReport -> ( Model, Cmd msg )
handleGetSummaryReportCompleted appState model result =
    let
        newSummaryReport =
            case result of
                Ok summaryReport ->
                    Success summaryReport

                Err error ->
                    ApiError.toActionResult (lg "apiError.questionnaires.summaryReport.fetchError" appState) error
    in
    ( { model | summaryReport = newSummaryReport }, Cmd.none )


handleForm : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleForm cfg formMsg appState model =
    case model.state of
        FormState modelForm ->
            case ( formMsg, Form.getOutput modelForm ) of
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
                            { model | state = FormState <| Form.update DocumentCreateForm.validation formMsg modelForm }
                    in
                    ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


handlePostDocumentCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError Document -> ( Model, Cmd msg )
handlePostDocumentCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( model, cfg.documentsNavigateCmd )

        Err error ->
            ( { model | savingDocument = ApiError.toActionResult (lg "apiError.documents.postError" appState) error }, Cmd.none )



-- VIEW


view : AppState -> QuestionnaireDetail -> Model -> Html Msg
view appState questionnaire model =
    case model.state of
        FormState form ->
            let
                actionResult =
                    ActionResult.combine model.summaryReport model.template
            in
            Page.actionResultView appState (viewFormState appState questionnaire.uuid model form) actionResult

        TemplateNotSetState ->
            TemplateNotSet.view appState questionnaire


viewFormState : AppState -> Uuid -> Model -> Form FormError DocumentCreateForm -> ( SummaryReport, TemplateDetail ) -> Html Msg
viewFormState appState questionnaireUuid model form ( summaryReport, template ) =
    div [ class "Plans__Detail__Content Plans__Detail__Content--NewDocument" ]
        [ div [ detailClass "container" ]
            [ Page.header (l_ "header.title" appState) []
            , div []
                [ FormResult.view appState model.savingDocument
                , Html.map FormMsg <| formView appState form summaryReport template
                , FormActions.view appState
                    (Routes.PlansRoute <| DetailRoute questionnaireUuid <| PlanDetailRoute.Documents PaginationQueryString.empty)
                    (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingDocument (FormMsg Form.Submit) False)
                ]
            ]
        ]


formView : AppState -> Form FormError DocumentCreateForm -> SummaryReport -> TemplateDetail -> Html Form.Msg
formView appState form summaryReport template =
    let
        nameInput =
            FormGroup.input appState form "name" <| lg "document.name" appState

        formatInput =
            FormGroup.formatRadioGroup appState template.formats form "formatUuid" <| lg "template.format" appState
    in
    div []
        [ nameInput
        , div [ class "form-group" ] [ viewIndications appState summaryReport.totalReport.indications ]
        , formatInput
        ]
