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
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Container as Container
import Common.Components.Form as Form
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Components.TypeHintInput as TypeHintInput
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Setters exposing (setSelected)
import Common.Utils.TimeUtils as TimeUtils
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, br, div, p, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Documents as DocumentsApi
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.SummaryReport exposing (SummaryReport)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.SummaryReport exposing (viewIndications)
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)
import Wizard.Routes as Routes
import Wizard.Routing as Routing
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



-- MODEL


type alias Model =
    { summaryReport : ActionResult SummaryReport
    , event : ActionResult ProjectEvent
    , form : Form FormError DocumentCreateForm
    , templateTypeHintInputModel : TypeHintInput.Model DocumentTemplateSuggestion
    , savingDocument : ActionResult String
    }


initialModel :
    { q | name : String, documentTemplate : Maybe DocumentTemplateSuggestion, formatUuid : Maybe Uuid }
    -> Maybe Uuid
    -> Model
initialModel project mbEventUuid =
    { summaryReport = Loading
    , event = Maybe.unwrap Unset (always Loading) mbEventUuid
    , form = DocumentCreateForm.init project mbEventUuid
    , templateTypeHintInputModel = setSelected project.documentTemplate <| TypeHintInput.init "documentTemplateId"
    , savingDocument = Unset
    }


initEmpty : Model
initEmpty =
    initialModel { name = "", documentTemplate = Nothing, formatUuid = Nothing, events = [] } Nothing



-- UPDATE


type Msg
    = GetSummaryReportComplete (Result ApiError (ProjectDetailWrapper SummaryReport))
    | GetProjectEventComplete (Result ApiError ProjectEvent)
    | Cancel
    | FormMsg Form.Msg
    | SetTemplateTypeHintInputReply String
    | TemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | PostDocumentCompleted (Result ApiError Document)


fetchData : AppState -> Uuid -> Maybe Uuid -> Cmd Msg
fetchData appState projectUuid mbEventUuid =
    let
        eventCmd =
            case mbEventUuid of
                Just eventUuid ->
                    ProjectsApi.getEvent appState projectUuid eventUuid GetProjectEventComplete

                Nothing ->
                    Cmd.none

        summaryReportCmd =
            ProjectsApi.getSummaryReport appState projectUuid GetSummaryReportComplete
    in
    Cmd.batch
        [ eventCmd
        , summaryReportCmd
        , Dom.focus "#name"
        ]


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , projectUuid : Uuid
    , knowledgeModelPackageId : String
    , documentsNavigateCmd : Cmd msg
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        GetSummaryReportComplete result ->
            handleGetSummaryReportCompleted appState model result

        GetProjectEventComplete result ->
            handleGetProjectEventCompleted appState model result

        Cancel ->
            ( model, Window.historyBack (Routing.toUrl (Routes.projectsDetailDocuments cfg.projectUuid)) )

        FormMsg formMsg ->
            handleForm cfg formMsg appState model

        SetTemplateTypeHintInputReply value ->
            handleSetTemplateTypeHintInputReplyMsg model value

        TemplateTypeHintInputMsg typeHintInputMsg ->
            handleTemplateTypeHintInputMsg cfg typeHintInputMsg appState model

        PostDocumentCompleted result ->
            handlePostDocumentCompleted cfg appState model result


handleGetSummaryReportCompleted : AppState -> Model -> Result ApiError (ProjectDetailWrapper SummaryReport) -> ( Model, Cmd msg )
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


handleGetProjectEventCompleted : AppState -> Model -> Result ApiError ProjectEvent -> ( Model, Cmd msg )
handleGetProjectEventCompleted appState model result =
    let
        newEvent =
            case result of
                Ok event ->
                    Success event

                Err error ->
                    ApiError.toActionResult appState (gettext "Unable to get project event." appState.locale) error
    in
    ( { model | event = newEvent }, Cmd.none )


handleForm : UpdateConfig msg -> Form.Msg -> AppState -> Model -> ( Model, Cmd msg )
handleForm cfg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    DocumentCreateForm.encode cfg.projectUuid form

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
            , getTypeHints = DocumentTemplatesApi.getTemplatesFor appState cfg.knowledgeModelPackageId
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


view : AppState -> ProjectCommon -> Model -> Html Msg
view appState project model =
    let
        eventActionResult =
            if ActionResult.isUnset model.event then
                Success Nothing

            else
                ActionResult.map Just model.event

        actionResult =
            ActionResult.combine model.summaryReport eventActionResult
    in
    Page.actionResultView appState (viewFormState appState project model) actionResult


viewFormState : AppState -> ProjectCommon -> Model -> ( SummaryReport, Maybe ProjectEvent ) -> Html Msg
viewFormState appState project model ( summaryReport, mbEvent ) =
    Container.simpleForm
        [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsNewDocument) (gettext "New Document" appState.locale)
        , Form.viewSimple
            { formMsg = FormMsg
            , formResult = model.savingDocument
            , formView = formView appState project mbEvent model summaryReport
            , submitLabel = gettext "Create" appState.locale
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
        ]


formView : AppState -> ProjectCommon -> Maybe ProjectEvent -> Model -> SummaryReport -> Html Msg
formView appState project mbEvent model summaryReport =
    let
        cfg =
            { viewItem = TypeHintInputItem.templateSuggestion
            , wrapMsg = TemplateTypeHintInputMsg
            , nothingSelectedItem = text "--"
            , clearEnabled = False
            , locale = appState.locale
            }

        nameInput =
            FormGroup.input appState.locale model.form "name" <| gettext "Name" appState.locale

        templateInput =
            TypeHintInput.view cfg model.templateTypeHintInputModel

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
                            ProjectEvent.getCreatedAt event
                                |> TimeUtils.toReadableDateTime appState.timeZone

                        currentLink =
                            linkTo (Routes.projectsDetailDocumentsNew project.uuid Nothing)
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
