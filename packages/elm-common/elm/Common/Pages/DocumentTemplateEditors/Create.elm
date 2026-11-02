module Common.Pages.DocumentTemplateEditors.Create exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Creating a document template editor, from scratch or based on an existing
template.

`?selected=<uuid>&edit=true` (the "Create editor" action of a template) loads
that template, shows it as the fixed base, and prefills the name, the id and
the next minor version from it.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Common.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Common.Api.Models.UuidResponse exposing (UuidResponse)
import Common.Components.Form as FormLayout
import Common.Components.Form.Field as Field
import Common.Components.GuideLink as GuideLink
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.DocumentTemplateEditorCreateForm as DocumentTemplateEditorCreateForm exposing (DocumentTemplateEditorCreateForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.DocumentTemplateUtils as DocumentTemplateUtils
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Normalize as Normalize
import Ui.Field as UiField
import Uuid exposing (Uuid)
import Version



-- MODEL


type alias Model =
    { savingDocumentTemplate : ActionResult ()
    , form : Form FormError DocumentTemplateEditorCreateForm
    , documentTemplateTypeHint : TypeHint.Model DocumentTemplateSuggestion
    , documentTemplate : ActionResult DocumentTemplateDetail
    , selectedDocumentTemplateUuid : Maybe Uuid
    , edit : Bool
    }


initialModel : AppState -> Maybe Uuid -> Maybe Bool -> Model
initialModel appState selectedDocumentTemplateUuid edit =
    { savingDocumentTemplate = Unset
    , form = DocumentTemplateEditorCreateForm.init appState selectedDocumentTemplateUuid
    , documentTemplateTypeHint = TypeHint.init
    , documentTemplate = Loading
    , selectedDocumentTemplateUuid = selectedDocumentTemplateUuid
    , edit = Maybe.withDefault False edit
    }


init : AppState -> Maybe Uuid -> Maybe Bool -> ( Model, Cmd Msg )
init appState selectedDocumentTemplateUuid edit =
    let
        model =
            initialModel appState selectedDocumentTemplateUuid edit

        cmd =
            case ( model.selectedDocumentTemplateUuid, model.edit ) of
                ( Just documentTemplateUuid, True ) ->
                    DocumentTemplatesApi.getTemplate appState documentTemplateUuid GetDocumentTemplateCompleted

                _ ->
                    Cmd.none
    in
    ( model, cmd )



-- UPDATE


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Field.VersionSuggestion
    | PostDocumentTemplateDraftCompleted (Result ApiError UuidResponse)
    | DocumentTemplateTypeHintMsg (TypeHint.Msg DocumentTemplateSuggestion)
    | GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState Routes.documentTemplateEditorsIndex) )

        FormMsg formMsg ->
            handleFormMsg cfg appState formMsg model

        FormSetVersion suggestion ->
            ( { model | form = Field.setVersion (DocumentTemplateEditorCreateForm.validation appState) versionFields suggestion model.form }
            , Cmd.none
            )

        PostDocumentTemplateDraftCompleted result ->
            case result of
                Ok documentTemplate ->
                    ( model, Routes.navigate appState (Routes.documentTemplateEditorDetail documentTemplate.uuid) )

                Err error ->
                    ( { model
                        | form = FormUtils.setFormErrors appState error model.form
                        , savingDocumentTemplate = ApiError.toActionResult appState (gettext "Document template could not be created." appState.locale) error
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , FormLayout.focusErrorSummary
                        ]
                    )

        DocumentTemplateTypeHintMsg typeHintMsg ->
            let
                setBasedOn =
                    cfg.wrapMsg << FormMsg << Form.Input "basedOn" Form.Select << Form.Field.String

                typeHintCfg =
                    { wrapMsg = cfg.wrapMsg << DocumentTemplateTypeHintMsg
                    , getTypeHints = DocumentTemplatesApi.getTemplatesSuggestions appState (Just False) True
                    , getError = gettext "Unable to get Knowledge Models." appState.locale
                    , setReply = setBasedOn << Uuid.toString << .uuid
                    , clearReply = Just (setBasedOn "")
                    , filterResults = Nothing
                    , logoutMsg = cfg.logoutMsg
                    , id = basedOnId
                    , toKey = Uuid.toString << .uuid
                    }

                ( documentTemplateTypeHint, cmd ) =
                    TypeHint.update appState typeHintCfg typeHintMsg model.documentTemplateTypeHint
            in
            ( { model | documentTemplateTypeHint = documentTemplateTypeHint }, cmd )

        GetDocumentTemplateCompleted result ->
            case result of
                Ok documentTemplate ->
                    let
                        nextVersion =
                            Version.nextMinor documentTemplate.version

                        form =
                            model.form
                                |> setFormValue appState "name" documentTemplate.name
                                |> setFormValue appState "id" documentTemplate.id
                                |> Field.setVersion (DocumentTemplateEditorCreateForm.validation appState)
                                    versionFields
                                    { major = Version.getMajor nextVersion
                                    , minor = Version.getMinor nextVersion
                                    , patch = Version.getPatch nextVersion
                                    }
                    in
                    ( { model | documentTemplate = Success documentTemplate, form = form }, Cmd.none )

                Err error ->
                    ( { model | documentTemplate = ApiError.toActionResult appState (gettext "Unable to get the document template." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


handleFormMsg : UpdateConfig Msg msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleFormMsg cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just documentTemplateCreateForm ) ->
            if ActionResult.isLoading model.savingDocumentTemplate then
                ( model, Cmd.none )

            else
                ( { model | savingDocumentTemplate = Loading }
                , DocumentTemplateDraftsApi.postDraft appState
                    (DocumentTemplateEditorCreateForm.encode documentTemplateCreateForm)
                    (cfg.wrapMsg << PostDocumentTemplateDraftCompleted)
                )

        _ ->
            let
                newForm =
                    Form.update (DocumentTemplateEditorCreateForm.validation appState) formMsg model.form

                templateIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "id" model.form).value

                formWithTemplateId =
                    case ( formMsg, templateIdEmpty ) of
                        ( Form.Blur "name", True ) ->
                            let
                                suggestedTemplateId =
                                    (Form.getFieldAsString "name" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            setFormValue appState "id" suggestedTemplateId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithTemplateId }
            , Cmd.none
            )


setFormValue : AppState -> String -> String -> Form FormError DocumentTemplateEditorCreateForm -> Form FormError DocumentTemplateEditorCreateForm
setFormValue appState field value =
    Form.update (DocumentTemplateEditorCreateForm.validation appState) (Form.Input field Form.Text (Form.Field.String value))


versionFields : { majorField : String, minorField : String, patchField : String }
versionFields =
    { majorField = "versionMajor"
    , minorField = "versionMinor"
    , patchField = "versionPatch"
    }


basedOnId : String
basedOnId =
    Field.fieldId "basedOn"



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ FormLayout.submitShortcut appState { result = model.savingDocumentTemplate, onSubmit = FormMsg Form.Submit }
        , Sub.map DocumentTemplateTypeHintMsg (TypeHint.subscriptions model.documentTemplateTypeHint)
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    case ( model.selectedDocumentTemplateUuid, model.edit ) of
        ( Just _, True ) ->
            Page.actionResult appState (always (viewCreate appState model)) model.documentTemplate

        _ ->
            viewCreate appState model


viewCreate : AppState -> Model -> Html Msg
viewCreate appState model =
    FormLayout.createPage appState
        { title = gettext "Create Document Template" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.documentTemplatesCreate ]
        , content = formContent appState model
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingDocumentTemplate
        , onSubmit = FormMsg Form.Submit
        , onCancel = Just Cancel
        }


formContent : AppState -> Model -> List (Html Msg)
formContent appState model =
    let
        basedOnLabel =
            gettext "Based on" appState.locale

        basedOnHint =
            gettext "You can create a new document template based on the existing one or start from scratch." appState.locale

        basedOnInput =
            case model.selectedDocumentTemplateUuid of
                Just _ ->
                    UiField.readOnly
                        { id = basedOnId
                        , label = basedOnLabel
                        , value =
                            case model.documentTemplate of
                                Success documentTemplate ->
                                    DocumentTemplateUtils.getId documentTemplate

                                Error message ->
                                    message

                                _ ->
                                    gettext "Loading..." appState.locale
                        }
                        |> UiField.withHint basedOnHint
                        |> UiField.toHtml

                Nothing ->
                    TypeHint.view appState
                        { id = basedOnId
                        , label = basedOnLabel
                        , placeholder = "--"
                        , required = False
                        , error = Field.errorText appState basedOnLabel (Form.getFieldAsString "basedOn" model.form)
                        , hint = Just basedOnHint
                        , clearable = True
                        , viewItem =
                            \template ->
                                { label = template.name
                                , code = Just (DocumentTemplateUtils.getId template)
                                , badges = RowBadge.owner appState template.workspaceUuid
                                , description = Just template.description
                                , icon = Nothing
                                }
                        , toKey = Uuid.toString << .uuid
                        , wrapMsg = DocumentTemplateTypeHintMsg
                        }
                        model.documentTemplateTypeHint
    in
    [ Field.input appState model.form "name" (gettext "Name" appState.locale)
        |> Field.withRequired True
        |> Field.toHtml
        |> Html.map FormMsg
    , Field.input appState model.form "id" (gettext "Document Template ID" appState.locale)
        |> Field.withRequired True
        |> Field.withHint (gettext "Document template ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
        |> Field.toHtml
        |> Html.map FormMsg
    , versionInput appState model
    , basedOnInput
    ]


{-| Major, minor and patch, with the next versions of the template this is
based on as one-press suggestions.
-}
versionInput : AppState -> Model -> Html Msg
versionInput appState model =
    Field.versionWithSuggestions appState
        model.form
        { majorField = versionFields.majorField
        , minorField = versionFields.minorField
        , patchField = versionFields.patchField
        , label = gettext "New version" appState.locale
        , hint = Just (Field.versionHint appState)
        , suggestions = Field.versionSuggestions (Maybe.map .version (ActionResult.toMaybe model.documentTemplate))
        , onSuggestion = FormSetVersion
        , toMsg = FormMsg
        }
