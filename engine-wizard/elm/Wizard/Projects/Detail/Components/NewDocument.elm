module Wizard.Projects.Detail.Components.NewDocument exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initEmpty
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, br, div, p, strong, text)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Document exposing (Document)
import Shared.Data.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.SummaryReport exposing (SummaryReport)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Setters exposing (setSelected)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.SummaryReport exposing (viewIndications)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes



-- MODEL


type alias Model =
    { summaryReport : ActionResult SummaryReport
    , event : ActionResult QuestionnaireEvent
    , form : Form FormError DocumentCreateForm
    , templateTypeHintInputModel : TypeHintInput.Model DocumentTemplateSuggestion
    , savingDocument : ActionResult String
    }


initialModel :
    { q | name : String, documentTemplate : Maybe DocumentTemplateSuggestion, formatUuid : Maybe Uuid }
    -> Maybe Uuid
    -> Model
initialModel questionnaire mbEventUuid =
    { summaryReport = Loading
    , event = Maybe.unwrap Unset (always Loading) mbEventUuid
    , form = DocumentCreateForm.init questionnaire mbEventUuid
    , templateTypeHintInputModel = setSelected questionnaire.documentTemplate <| TypeHintInput.init "documentTemplateId"
    , savingDocument = Unset
    }


initEmpty : Model
initEmpty =
    initialModel { name = "", documentTemplate = Nothing, formatUuid = Nothing, events = [] } Nothing



-- UPDATE


type Msg
    = GetSummaryReportComplete (Result ApiError SummaryReport)
    | GetQuestionnaireEventComplete (Result ApiError QuestionnaireEvent)
    | FormMsg Form.Msg
    | TemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | PostDocumentCompleted (Result ApiError Document)


fetchData : AppState -> Uuid -> Maybe Uuid -> Cmd Msg
fetchData appState questionnaireUuid mbEventUuid =
    let
        eventCmd =
            case mbEventUuid of
                Just eventUuid ->
                    QuestionnairesApi.getQuestionnaireEvent questionnaireUuid eventUuid appState GetQuestionnaireEventComplete

                Nothing ->
                    Cmd.none

        summaryReportCmd =
            QuestionnairesApi.getSummaryReport questionnaireUuid appState GetSummaryReportComplete
    in
    Cmd.batch [ eventCmd, summaryReportCmd ]


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

        GetQuestionnaireEventComplete result ->
            handleGetQuestionnaireEventCompleted appState model result

        FormMsg formMsg ->
            handleForm cfg formMsg appState model

        TemplateTypeHintInputMsg typeHintInputMsg ->
            handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted cfg appState model result


handleGetSummaryReportCompleted : AppState -> Model -> Result ApiError SummaryReport -> ( Model, Cmd msg )
handleGetSummaryReportCompleted appState model result =
    let
        newSummaryReport =
            case result of
                Ok summaryReport ->
                    Success summaryReport

                Err error ->
                    ApiError.toActionResult appState (gettext "Unable to get the summary report." appState.locale) error
    in
    ( { model | summaryReport = newSummaryReport }, Cmd.none )


handleGetQuestionnaireEventCompleted : AppState -> Model -> Result ApiError QuestionnaireEvent -> ( Model, Cmd msg )
handleGetQuestionnaireEventCompleted appState model result =
    let
        newEvent =
            case result of
                Ok event ->
                    Success event

                Err error ->
                    ApiError.toActionResult appState (gettext "Unable to get questionnaire event." appState.locale) error
    in
    ( { model | event = newEvent }, Cmd.none )


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


handleTemplateTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg DocumentTemplateSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        formMsg =
            cfg.wrapMsg << FormMsg << Form.Input "documentTemplateId" Form.Select << Field.String

        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << TemplateTypeHintInputMsg
            , getTypeHints = DocumentTemplatesApi.getTemplatesFor cfg.packageId
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        form =
            case typeHintInputMsg of
                TypeHintInput.SetReply _ ->
                    Form.update DocumentCreateForm.validation (Form.Input "formatUuid" Form.Text (Field.String "")) model.form

                _ ->
                    model.form

        ( templateTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg appState model.templateTypeHintInputModel
    in
    ( { model
        | templateTypeHintInputModel = templateTypeHintInputModel
        , form = form
      }
    , cmd
    )


handlePostDocumentCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError Document -> ( Model, Cmd msg )
handlePostDocumentCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( model, cfg.documentsNavigateCmd )

        Err error ->
            ( { model | savingDocument = ApiError.toActionResult appState (gettext "Document could not be created." appState.locale) error }, Cmd.none )



-- VIEW


view : AppState -> QuestionnaireDetail -> Model -> Html Msg
view appState questionnaire model =
    let
        eventActionResult =
            if ActionResult.isUnset model.event then
                Success Nothing

            else
                ActionResult.map Just model.event

        actionResult =
            ActionResult.combine model.summaryReport eventActionResult
    in
    Page.actionResultView appState (viewFormState appState questionnaire model) actionResult


viewFormState : AppState -> QuestionnaireDetail -> Model -> ( SummaryReport, Maybe QuestionnaireEvent ) -> Html Msg
viewFormState appState questionnaire model ( summaryReport, mbEvent ) =
    div [ class "Projects__Detail__Content Projects__Detail__Content--NewDocument" ]
        [ div [ detailClass "container" ]
            [ Page.header (gettext "New Document" appState.locale) []
            , div []
                [ FormResult.view appState model.savingDocument
                , formView appState questionnaire mbEvent model summaryReport
                , FormActions.view appState
                    (Routes.ProjectsRoute <| DetailRoute questionnaire.uuid <| ProjectDetailRoute.Documents PaginationQueryString.empty)
                    (ActionResult.ButtonConfig (gettext "Create" appState.locale) model.savingDocument (FormMsg Form.Submit) False)
                ]
            ]
        ]


formView : AppState -> QuestionnaireDetail -> Maybe QuestionnaireEvent -> Model -> SummaryReport -> Html Msg
formView appState questionnaire mbEvent model summaryReport =
    let
        cfg =
            { viewItem = TypeHintItem.templateSuggestion
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            }

        nameInput =
            FormGroup.input appState model.form "name" <| gettext "Name" appState.locale

        templateInput =
            TypeHintInput.view appState cfg model.templateTypeHintInputModel

        formatInput =
            case model.templateTypeHintInputModel.selected of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState selectedTemplate.formats model.form "formatUuid" <| gettext "Format" appState.locale

                _ ->
                    emptyNode

        extraInfo =
            case mbEvent of
                Just event ->
                    let
                        datetime =
                            QuestionnaireEvent.getCreatedAt event
                                |> TimeUtils.toReadableDateTime appState.timeZone

                        currentLink =
                            linkTo appState
                                (Routes.projectsDetailDocumentsNew questionnaire.uuid Nothing)
                                []
                                [ text (gettext "Create for current version" appState.locale) ]
                    in
                    div [ class "alert alert-info" ]
                        [ p []
                            (String.formatHtml
                                (gettext "You are creating a document for a project version from %s" appState.locale)
                                [ strong [] [ br [] [], text datetime ] ]
                            )
                        , currentLink
                        ]

                Nothing ->
                    viewIndications appState summaryReport.totalReport.indications
    in
    div []
        [ Html.map FormMsg <| nameInput
        , div [ class "form-group" ] [ extraInfo ]
        , FormGroup.formGroupCustom templateInput appState model.form "documentTemplateId" <| gettext "Document Template" appState.locale
        , Html.map FormMsg <| formatInput
        ]
