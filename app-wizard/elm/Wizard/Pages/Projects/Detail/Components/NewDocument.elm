module Wizard.Pages.Projects.Detail.Components.NewDocument exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initEmpty
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, br, div, p, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Shared.Components.ActionButton as ActionResult
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Shared.Components.Page as Page
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Setters exposing (setSelected)
import Shared.Utils.TimeUtils as TimeUtils
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Documents as DocumentsApi
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.SummaryReport exposing (SummaryReport)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.FormActions as FormActions
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.SummaryReport exposing (viewIndications)
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



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
    = GetSummaryReportComplete (Result ApiError (QuestionnaireDetailWrapper SummaryReport))
    | GetQuestionnaireEventComplete (Result ApiError QuestionnaireEvent)
    | Cancel
    | FormMsg Form.Msg
    | SetTemplateTypeHintInputReply String
    | TemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | PostDocumentCompleted (Result ApiError Document)


fetchData : AppState -> Uuid -> Maybe Uuid -> Cmd Msg
fetchData appState questionnaireUuid mbEventUuid =
    let
        eventCmd =
            case mbEventUuid of
                Just eventUuid ->
                    QuestionnairesApi.getQuestionnaireEvent appState questionnaireUuid eventUuid GetQuestionnaireEventComplete

                Nothing ->
                    Cmd.none

        summaryReportCmd =
            QuestionnairesApi.getSummaryReport appState questionnaireUuid GetSummaryReportComplete
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

        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl (Routes.projectsDetailDocuments cfg.questionnaireUuid)) )

        FormMsg formMsg ->
            handleForm cfg formMsg appState model

        SetTemplateTypeHintInputReply value ->
            handleSetTemplateTypeHintInputReplyMsg model value

        TemplateTypeHintInputMsg typeHintInputMsg ->
            handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted cfg appState model result


handleGetSummaryReportCompleted : AppState -> Model -> Result ApiError (QuestionnaireDetailWrapper SummaryReport) -> ( Model, Cmd msg )
handleGetSummaryReportCompleted appState model result =
    let
        newSummaryReport =
            case result of
                Ok summaryReport ->
                    Success summaryReport.data

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
                        DocumentsApi.postDocument appState body PostDocumentCompleted
            in
            ( { model | savingDocument = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update DocumentCreateForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handleSetTemplateTypeHintInputReplyMsg : Model -> String -> ( Model, Cmd msg )
handleSetTemplateTypeHintInputReplyMsg model value =
    let
        formMsg field =
            Form.Input field Form.Select << Field.String

        updateFormatUuid =
            case (Form.getFieldAsString "formatUuid" model.form).value of
                Just "" ->
                    identity

                _ ->
                    Form.update DocumentCreateForm.validation (formMsg "formatUuid" "")

        form =
            model.form
                |> Form.update DocumentCreateForm.validation (formMsg "documentTemplateId" value)
                |> updateFormatUuid
    in
    ( { model | form = form }, Cmd.none )


handleTemplateTypeHintInputMsg : UpdateConfig msg -> TypeHintInput.Msg DocumentTemplateSuggestion -> AppState -> Model -> ( Model, Cmd msg )
handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model =
    let
        typeHintInputCfg =
            { wrapMsg = cfg.wrapMsg << TemplateTypeHintInputMsg
            , getTypeHints = DocumentTemplatesApi.getTemplatesFor appState cfg.packageId
            , getError = gettext "Unable to get document templates." appState.locale
            , setReply = cfg.wrapMsg << SetTemplateTypeHintInputReply << .id
            , clearReply = Just <| cfg.wrapMsg <| SetTemplateTypeHintInputReply ""
            , filterResults = Nothing
            }

        ( templateTypeHintInputModel, cmd ) =
            TypeHintInput.update typeHintInputCfg typeHintInputMsg model.templateTypeHintInputModel
    in
    ( { model | templateTypeHintInputModel = templateTypeHintInputModel }
    , cmd
    )


handlePostDocumentCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError Document -> ( Model, Cmd msg )
handlePostDocumentCompleted cfg appState model result =
    case result of
        Ok _ ->
            ( model, cfg.documentsNavigateCmd )

        Err error ->
            ( { model | savingDocument = ApiError.toActionResult appState (gettext "Document could not be created." appState.locale) error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TemplateTypeHintInputMsg <|
        TypeHintInput.subscriptions model.templateTypeHintInputModel



-- VIEW


view : AppState -> QuestionnaireCommon -> Model -> Html Msg
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


viewFormState : AppState -> QuestionnaireCommon -> Model -> ( SummaryReport, Maybe QuestionnaireEvent ) -> Html Msg
viewFormState appState questionnaire model ( summaryReport, mbEvent ) =
    div [ class "Projects__Detail__Content Projects__Detail__Content--NewDocument" ]
        [ div [ detailClass "container" ]
            [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsNewDocument) (gettext "New Document" appState.locale)
            , div []
                [ FormResult.view model.savingDocument
                , formView appState questionnaire mbEvent model summaryReport
                , FormActions.view appState
                    Cancel
                    (ActionResult.ButtonConfig (gettext "Create" appState.locale) model.savingDocument (FormMsg Form.Submit) False)
                ]
            ]
        ]


formView : AppState -> QuestionnaireCommon -> Maybe QuestionnaireEvent -> Model -> SummaryReport -> Html Msg
formView appState questionnaire mbEvent model summaryReport =
    let
        cfg =
            { viewItem = TypeHintItem.templateSuggestion
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            }

        nameInput =
            FormGroup.input appState.locale model.form "name" <| gettext "Name" appState.locale

        templateInput =
            TypeHintInput.view appState cfg model.templateTypeHintInputModel

        formatInput =
            case model.templateTypeHintInputModel.selected of
                Just selectedTemplate ->
                    FormGroup.formatRadioGroup appState.locale selectedTemplate.formats model.form "formatUuid" <| gettext "Format" appState.locale

                _ ->
                    Html.nothing

        extraInfo =
            case mbEvent of
                Just event ->
                    let
                        datetime =
                            QuestionnaireEvent.getCreatedAt event
                                |> TimeUtils.toReadableDateTime appState.timeZone

                        currentLink =
                            linkTo (Routes.projectsDetailDocumentsNew questionnaire.uuid Nothing)
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
        , FormGroup.formGroupCustom templateInput appState.locale model.form "documentTemplateId" <| gettext "Document Template" appState.locale
        , Html.map FormMsg <| formatInput
        ]
