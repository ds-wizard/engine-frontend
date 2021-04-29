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
import Html exposing (..)
import Html.Attributes exposing (class)
import List.Extra as List
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Document exposing (Document)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.SummaryReport exposing (SummaryReport)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lh, lx)
import Shared.Setters exposing (setSelected)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Projects.Detail.Components.NewDocument"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Projects.Detail.Components.NewDocument"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Projects.Detail.Components.NewDocument"



-- MODEL


type alias Model =
    { summaryReport : ActionResult SummaryReport
    , form : Form FormError DocumentCreateForm
    , templateTypeHintInputModel : TypeHintInput.Model TemplateSuggestion
    , savingDocument : ActionResult String
    }


initialModel :
    { q | name : String, template : Maybe TemplateSuggestion, formatUuid : Maybe Uuid, events : List QuestionnaireEvent }
    -> Maybe Uuid
    -> Model
initialModel questionnaire mbEventUuid =
    { summaryReport = Loading
    , form = DocumentCreateForm.init questionnaire mbEventUuid
    , templateTypeHintInputModel = setSelected questionnaire.template <| TypeHintInput.init "templateId"
    , savingDocument = Unset
    }


initEmpty : Model
initEmpty =
    initialModel { name = "", template = Nothing, formatUuid = Nothing, events = [] } Nothing



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
            ( { model | savingDocument = ApiError.toActionResult appState (lg "apiError.documents.postError" appState) error }, Cmd.none )



-- VIEW


view : AppState -> QuestionnaireDetail -> Maybe String -> Model -> Html Msg
view appState questionnaire mbEventUuid model =
    Page.actionResultView appState (viewFormState appState questionnaire mbEventUuid model) model.summaryReport


viewFormState : AppState -> QuestionnaireDetail -> Maybe String -> Model -> SummaryReport -> Html Msg
viewFormState appState questionnaire mbEventUuid model summaryReport =
    div [ class "Plans__Detail__Content Plans__Detail__Content--NewDocument" ]
        [ div [ detailClass "container" ]
            [ Page.header (l_ "header.title" appState) []
            , div []
                [ FormResult.view appState model.savingDocument
                , formView appState questionnaire mbEventUuid model summaryReport
                , FormActions.view appState
                    (Routes.ProjectsRoute <| DetailRoute questionnaire.uuid <| ProjectDetailRoute.Documents PaginationQueryString.empty)
                    (ActionResult.ButtonConfig (l_ "form.create" appState) model.savingDocument (FormMsg Form.Submit) False)
                ]
            ]
        ]


formView : AppState -> QuestionnaireDetail -> Maybe String -> Model -> SummaryReport -> Html Msg
formView appState questionnaire mbEventUuid model summaryReport =
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

        mbEvent =
            List.find (QuestionnaireEvent.getUuid >> Uuid.toString >> Just >> (==) mbEventUuid) questionnaire.events

        extraInfo =
            case mbEvent of
                Just event ->
                    let
                        datetime =
                            QuestionnaireEvent.getCreatedAt event
                                |> TimeUtils.toReadableDateTime appState.timeZone

                        currentLink =
                            linkTo appState
                                (Routes.ProjectsRoute <| DetailRoute questionnaire.uuid <| ProjectDetailRoute.NewDocument Nothing)
                                []
                                [ lx_ "oldVersionInfo.link" appState ]
                    in
                    div [ class "alert alert-info" ]
                        [ p []
                            (lh_ "oldVersionInfo.text" [ strong [] [ br [] [], text datetime ] ] appState)
                        , currentLink
                        ]

                Nothing ->
                    viewIndications appState summaryReport.totalReport.indications
    in
    div []
        [ Html.map FormMsg <| nameInput
        , div [ class "form-group" ] [ extraInfo ]
        , FormGroup.formGroupCustom templateInput appState model.form "templateId" <| lg "questionnaire.template" appState
        , Html.map FormMsg <| formatInput
        ]
