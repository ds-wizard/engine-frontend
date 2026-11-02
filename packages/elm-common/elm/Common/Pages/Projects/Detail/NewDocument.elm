module Common.Pages.Projects.Detail.NewDocument exposing
    ( Model
    , Msg
    , init
    , initialModel
    , setProject
    , subscriptions
    , update
    , view
    )

{-| Generate a document from a project.

It is part of the documents tab rather than a page of its own: the project's
name is above it and the Documents tab stays current while it is open, so this
is a form in the tab's body (`Form.createWithinPage`) and not a second page.

The form starts from the project's own settings — its name, the document
template it was last given, that template's format and language — so the usual
case is to press Create. Those settings arrive with the request the project's
frame already makes for this route; `setProject` is where they fill the form.

With `?eventUuid=` the document is generated from the project as it was at that
event rather than as it is now. That event is fetched only to say, above the
form, which version is being used and to offer the current one instead.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Documents as DocumentsApi
import Common.Api.Models.Document exposing (Document)
import Common.Api.Models.DocumentTemplateSuggestion as DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectSettings exposing (ProjectSettings)
import Common.Api.Projects as ProjectsApi
import Common.Components.Form as Form
import Common.Components.Form.Field as FormField
import Common.Components.GuideLink as GuideLink
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.DocumentCreateForm as DocumentCreateForm
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.TimeUtils as TimeUtils
import Form as ElmForm exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { eventUuid : Maybe Uuid
    , event : ActionResult ProjectEvent

    -- The project's settings are what the form is made of, and they come from
    -- the frame's own request for this route, so all this has to remember is
    -- whether they have arrived.
    , project : ActionResult ()
    , form : Form FormError DocumentCreateForm.DocumentCreateForm
    , documentTemplateTypeHint : TypeHint.Model DocumentTemplateSuggestion
    , savingDocument : ActionResult ()
    }


initialModel : Maybe Uuid -> Model
initialModel mbEventUuid =
    { eventUuid = mbEventUuid
    , event =
        case mbEventUuid of
            Just _ ->
                Loading

            Nothing ->
                Unset
    , project = Loading
    , form = DocumentCreateForm.init emptyProject Nothing
    , documentTemplateTypeHint = TypeHint.init
    , savingDocument = Unset
    }


{-| A form with nothing in it, for the moment before the project's settings
arrive.
-}
emptyProject : { name : String, documentTemplate : Maybe DocumentTemplateSuggestion, formatUuid : Maybe Uuid, documentTemplateLanguage : Maybe String }
emptyProject =
    { name = ""
    , documentTemplate = Nothing
    , formatUuid = Nothing
    , documentTemplateLanguage = Nothing
    }


init : AppState -> Uuid -> Maybe Uuid -> ( Model, Cmd Msg )
init appState projectUuid mbEventUuid =
    ( initialModel mbEventUuid
    , case mbEventUuid of
        Just eventUuid ->
            ProjectsApi.getEvent appState projectUuid eventUuid GetProjectEventCompleted

        Nothing ->
            Cmd.none
    )


{-| Fill the form from the project's settings, once they have arrived.
-}
setProject : ProjectSettings -> Model -> Model
setProject project model =
    { model
        | project = Success ()
        , form = DocumentCreateForm.init project model.eventUuid
        , documentTemplateTypeHint = TypeHint.setSelected project.documentTemplate model.documentTemplateTypeHint
    }



-- UPDATE


type Msg
    = GetProjectEventCompleted (Result ApiError ProjectEvent)
    | Cancel
    | FormMsg ElmForm.Msg
    | SetDocumentTemplate (Maybe DocumentTemplateSuggestion)
    | DocumentTemplateTypeHintMsg (TypeHint.Msg DocumentTemplateSuggestion)
    | PostDocumentCompleted (Result ApiError Document)


update : UpdateConfig Msg msg -> AppState -> { projectUuid : Uuid, knowledgeModelPackageUuid : Uuid } -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState context msg model =
    case msg of
        GetProjectEventCompleted result ->
            case result of
                Ok event ->
                    ( { model | event = Success event }, Cmd.none )

                Err error ->
                    ( { model | event = ApiError.toActionResult appState (gettext "Unable to get project event." appState.locale) error }
                    , Cmd.none
                    )

        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState (Routes.projectsDetailDocuments context.projectUuid)) )

        FormMsg formMsg ->
            case ( formMsg, ElmForm.getOutput model.form ) of
                ( ElmForm.Submit, Just form ) ->
                    ( { model | savingDocument = Loading }
                    , DocumentsApi.postDocument appState
                        (DocumentCreateForm.encode context.projectUuid form)
                        (cfg.wrapMsg << PostDocumentCompleted)
                    )

                _ ->
                    ( { model | form = ElmForm.update DocumentCreateForm.validation formMsg model.form }
                    , Cmd.none
                    )

        SetDocumentTemplate mbDocumentTemplate ->
            ( { model | form = setDocumentTemplate mbDocumentTemplate model.form }, Cmd.none )

        DocumentTemplateTypeHintMsg typeHintMsg ->
            let
                ( documentTemplateTypeHint, cmd ) =
                    TypeHint.update appState
                        { wrapMsg = cfg.wrapMsg << DocumentTemplateTypeHintMsg
                        , getTypeHints = DocumentTemplatesApi.getTemplatesFor appState context.knowledgeModelPackageUuid
                        , getError = gettext "Unable to get document templates." appState.locale
                        , setReply = cfg.wrapMsg << SetDocumentTemplate << Just
                        , clearReply = Just (cfg.wrapMsg (SetDocumentTemplate Nothing))
                        , filterResults = Nothing
                        , logoutMsg = cfg.logoutMsg
                        , id = documentTemplateFieldId
                        , toKey = Uuid.toString << .uuid
                        }
                        typeHintMsg
                        model.documentTemplateTypeHint
            in
            ( { model | documentTemplateTypeHint = documentTemplateTypeHint }, cmd )

        PostDocumentCompleted result ->
            case result of
                Ok _ ->
                    ( model, Routes.navigate appState (Routes.projectsDetailDocuments context.projectUuid) )

                Err error ->
                    ( { model | savingDocument = ApiError.toActionResult appState (gettext "Document could not be created." appState.locale) error }
                    , Form.focusErrorSummary
                    )


{-| Choosing a template also sets the language it is written in and takes away
the format chosen for the previous one, which the new template may not have.
-}
setDocumentTemplate : Maybe DocumentTemplateSuggestion -> Form FormError DocumentCreateForm.DocumentCreateForm -> Form FormError DocumentCreateForm.DocumentCreateForm
setDocumentTemplate mbDocumentTemplate form =
    let
        setField field value =
            ElmForm.update DocumentCreateForm.validation (ElmForm.Input field ElmForm.Select (Form.Field.String value))

        clearFormat currentForm =
            case (ElmForm.getFieldAsString "formatUuid" currentForm).value of
                Just "" ->
                    currentForm

                _ ->
                    setField "formatUuid" "" currentForm
    in
    form
        |> setField "documentTemplateUuid" (Maybe.unwrap "" (Uuid.toString << .uuid) mbDocumentTemplate)
        |> clearFormat
        |> setField "language" (Maybe.unwrap "" .language mbDocumentTemplate)


documentTemplateFieldId : String
documentTemplateFieldId =
    FormField.fieldId "documentTemplateUuid"



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Form.submitShortcut appState { result = model.savingDocument, onSubmit = FormMsg ElmForm.Submit }
        , Sub.map DocumentTemplateTypeHintMsg (TypeHint.subscriptions model.documentTemplateTypeHint)
        ]



-- VIEW


view : AppState -> Uuid -> Model -> Html Msg
view appState projectUuid model =
    let
        loaded =
            ActionResult.combine model.project (ActionResult.map (always ()) (withoutUnset model.event))
    in
    Page.actionResult appState (always (viewForm appState projectUuid model)) loaded


{-| Nothing was asked for, so there is nothing to wait for.
-}
withoutUnset : ActionResult a -> ActionResult (Maybe a)
withoutUnset result =
    if ActionResult.isUnset result then
        Success Nothing

    else
        ActionResult.map Just result


viewForm : AppState -> Uuid -> Model -> Html Msg
viewForm appState projectUuid model =
    Form.createWithinPage appState
        { title = gettext "New Document" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsNewDocument ]
        , content =
            Html.map FormMsg
                (FormField.input appState model.form "name" (gettext "Name" appState.locale)
                    |> FormField.withRequired True
                    |> FormField.toHtml
                )
                :: versionNotice appState projectUuid model
                ++ (documentTemplateField appState model
                        :: formatField appState model
                        ++ languageField appState model
                   )
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingDocument
        , onSubmit = FormMsg ElmForm.Submit
        , onCancel = Just Cancel
        }


{-| Which version of the project this document will be made from, when it is not
the current one — and the way back to the current one.
-}
versionNotice : AppState -> Uuid -> Model -> List (Html Msg)
versionNotice appState projectUuid model =
    case model.event of
        Success event ->
            [ Alert.info ""
                |> Alert.toHtmlWithContent
                    [ Layout.stack
                        [ Text.body
                            (String.format (gettext "You are creating a document for a project version from %s" appState.locale)
                                [ TimeUtils.toReadableDateTime appState.timeZone (ProjectEvent.getCreatedAt event) ]
                            )
                        , Link.link
                            { label = gettext "Create for current version" appState.locale
                            , url = Routes.toUrl appState (Routes.projectsDetailDocumentsNew projectUuid Nothing)
                            }
                            |> Link.toHtml
                        ]
                    ]
            ]

        _ ->
            []


documentTemplateField : AppState -> Model -> Html Msg
documentTemplateField appState model =
    let
        label =
            gettext "Document Template" appState.locale
    in
    TypeHint.view appState
        { id = documentTemplateFieldId
        , label = label
        , placeholder = gettext "Select document template" appState.locale
        , required = True
        , error = FormField.errorText appState label (ElmForm.getFieldAsString "documentTemplateUuid" model.form)
        , hint = Nothing
        , clearable = False
        , viewItem =
            \documentTemplate ->
                { label = documentTemplate.name
                , code = Just (DocumentTemplateUtils.getId documentTemplate)
                , badges = RowBadge.owner appState documentTemplate.workspaceUuid
                , description = Just documentTemplate.description
                , icon = Nothing
                }
        , toKey = Uuid.toString << .uuid
        , wrapMsg = DocumentTemplateTypeHintMsg
        }
        model.documentTemplateTypeHint


formatField : AppState -> Model -> List (Html Msg)
formatField appState model =
    case TypeHint.selected model.documentTemplateTypeHint of
        Just documentTemplate ->
            let
                formatChoice format =
                    Field.choice { value = Uuid.toString format.uuid, label = format.name }
                        |> Field.withChoiceIcon (Icon.fromClass format.icon)
            in
            [ Html.map FormMsg
                (FormField.radioGroup appState
                    model.form
                    "formatUuid"
                    (gettext "Format" appState.locale)
                    (List.map formatChoice documentTemplate.formats)
                    |> FormField.withRequired True
                    |> FormField.toHtml
                )
            ]

        Nothing ->
            []


languageField : AppState -> Model -> List (Html Msg)
languageField appState model =
    case TypeHint.selected model.documentTemplateTypeHint of
        Just documentTemplate ->
            if List.isEmpty documentTemplate.locales then
                []

            else
                [ Html.map FormMsg
                    (FormField.select appState
                        model.form
                        "language"
                        (gettext "Language" appState.locale)
                        (DocumentTemplateSuggestion.languageOptions documentTemplate)
                        |> FormField.toHtml
                    )
                ]

        Nothing ->
            []
