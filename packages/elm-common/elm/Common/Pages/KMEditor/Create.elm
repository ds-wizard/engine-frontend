module Common.Pages.KMEditor.Create exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Creating a knowledge model editor.

The query string:

  - `selected` — the knowledge model the editor is based on. It is loaded, shown
    as read-only, and its language is filled in; without it the parent is chosen
    in a type hint.
  - `edit` — the editor edits the selected knowledge model rather than forking
    it, so its name and id are filled in too and the version suggestions follow
    its version.

Leaving the name fills in the knowledge model id from it, as long as the id is
still empty. On success the new editor opens.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.UuidResponse exposing (UuidResponse)
import Common.Components.Form as CommonForm
import Common.Components.Form.Field as Field
import Common.Components.GuideLink as GuideLink
import Common.Components.Page as Page
import Common.Components.RowBadge as RowBadge
import Common.Components.TypeHint as TypeHint
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Forms.KnowledgeModelEditorCreateForm as KnowledgeModelEditorCreateForm exposing (KnowledgeModelEditorCreateForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html)
import LanguageCodes
import Maybe.Extra as Maybe
import String.Normalize as Normalize
import Ui.Field
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { savingKmEditor : ActionResult ()
    , form : Form FormError KnowledgeModelEditorCreateForm
    , kmPackageTypeHint : TypeHint.Model KnowledgeModelPackageSuggestion
    , kmPackage : ActionResult KnowledgeModelPackageDetail
    , selectedKmPackageUuid : Maybe Uuid
    , edit : Bool
    }


initialModel : AppState -> Maybe Uuid -> Maybe Bool -> Model
initialModel appState selectedPackage edit =
    { savingKmEditor = Unset
    , form = KnowledgeModelEditorCreateForm.init appState (Maybe.map Uuid.toString selectedPackage)
    , kmPackageTypeHint = TypeHint.init
    , kmPackage = Loading
    , selectedKmPackageUuid = selectedPackage
    , edit = Maybe.withDefault False edit
    }


init : AppState -> Maybe Uuid -> Maybe Bool -> ( Model, Cmd Msg )
init appState selectedPackage edit =
    ( initialModel appState selectedPackage edit
    , case selectedPackage of
        Just kmPackageUuid ->
            KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmPackageUuid GetPackageCompleted

        Nothing ->
            Cmd.none
    )



-- UPDATE


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Field.VersionSuggestion
    | PostKmEditorCompleted (Result ApiError UuidResponse)
    | KnowledgeModelPackageTypeHintMsg (TypeHint.Msg KnowledgeModelPackageSuggestion)
    | SelectPreviousPackage (Maybe Uuid)
    | GetPackageCompleted (Result ApiError KnowledgeModelPackageDetail)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Cancel ->
            ( model, Window.historyBack (Routes.toUrl appState Routes.kmEditorIndex) )

        FormMsg formMsg ->
            handleFormMsg cfg appState formMsg model

        FormSetVersion suggestion ->
            ( { model | form = Field.setVersion (KnowledgeModelEditorCreateForm.validation appState) versionFields suggestion model.form }
            , Cmd.none
            )

        PostKmEditorCompleted result ->
            case result of
                Ok kmEditor ->
                    ( model
                    , Navigation.pushUrl appState.key (Routes.toUrl appState (Routes.kmEditorEditor kmEditor.uuid Nothing))
                    )

                Err error ->
                    ( { model
                        | form = FormUtils.setFormErrors appState error model.form
                        , savingKmEditor = ApiError.toActionResult appState (gettext "Knowledge model could not be created." appState.locale) error
                      }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , CommonForm.focusErrorSummary
                        ]
                    )

        KnowledgeModelPackageTypeHintMsg typeHintMsg ->
            let
                typeHintCfg =
                    { wrapMsg = cfg.wrapMsg << KnowledgeModelPackageTypeHintMsg
                    , getTypeHints = KnowledgeModelPackagesApi.getKnowledgeModelPackagesSuggestions appState (Just False)
                    , getError = gettext "Unable to get Knowledge Models." appState.locale
                    , setReply = cfg.wrapMsg << SelectPreviousPackage << Just << .uuid
                    , clearReply = Just (cfg.wrapMsg (SelectPreviousPackage Nothing))
                    , filterResults = Nothing
                    , logoutMsg = cfg.logoutMsg
                    , id = previousPackageId
                    , toKey = Uuid.toString << .uuid
                    }

                ( kmPackageTypeHint, cmd ) =
                    TypeHint.update appState typeHintCfg typeHintMsg model.kmPackageTypeHint
            in
            ( { model | kmPackageTypeHint = kmPackageTypeHint }, cmd )

        SelectPreviousPackage mbUuid ->
            let
                form =
                    setFormValue appState "previousPackageUuid" (Maybe.unwrap "" Uuid.toString mbUuid) model.form

                ( kmPackage, fetchCmd ) =
                    case mbUuid of
                        Just uuid ->
                            ( Loading
                            , Cmd.map cfg.wrapMsg <| KnowledgeModelPackagesApi.getKnowledgeModelPackage appState uuid GetPackageCompleted
                            )

                        Nothing ->
                            ( model.kmPackage, Cmd.none )
            in
            ( { model | form = form, kmPackage = kmPackage }, fetchCmd )

        GetPackageCompleted result ->
            case result of
                Ok kmPackage ->
                    let
                        formWithNameAndKmId =
                            if model.edit then
                                model.form
                                    |> setFormValue appState "name" kmPackage.name
                                    |> setFormValue appState "id" kmPackage.id

                            else
                                model.form

                        form =
                            setFormValue appState "language" kmPackage.language formWithNameAndKmId
                    in
                    ( { model | kmPackage = Success kmPackage, form = form }, Cmd.none )

                Err error ->
                    ( { model | kmPackage = ApiError.toActionResult appState (gettext "Unable to get the Knowledge Model." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


handleFormMsg : UpdateConfig Msg msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handleFormMsg cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            ( { model | savingKmEditor = Loading }
            , Cmd.map cfg.wrapMsg <|
                KnowledgeModelEditorsApi.postKnowledgeModelEditor appState
                    (KnowledgeModelEditorCreateForm.encode kmCreateForm)
                    PostKmEditorCompleted
            )

        _ ->
            let
                newForm =
                    Form.update (KnowledgeModelEditorCreateForm.validation appState) formMsg model.form

                kmIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "id" model.form).value

                formWithKmId =
                    case ( formMsg, kmIdEmpty ) of
                        ( Form.Blur "name", True ) ->
                            let
                                suggestedKmId =
                                    (Form.getFieldAsString "name" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            setFormValue appState "id" suggestedKmId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithKmId }
            , Cmd.none
            )


setFormValue : AppState -> String -> String -> Form FormError KnowledgeModelEditorCreateForm -> Form FormError KnowledgeModelEditorCreateForm
setFormValue appState field value =
    Form.update (KnowledgeModelEditorCreateForm.validation appState) (Form.Input field Form.Text (Form.Field.String value))


versionFields : { majorField : String, minorField : String, patchField : String }
versionFields =
    { majorField = "versionMajor"
    , minorField = "versionMinor"
    , patchField = "versionPatch"
    }


previousPackageId : String
previousPackageId =
    Field.fieldId "previousPackageUuid"



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.batch
        [ Sub.map KnowledgeModelPackageTypeHintMsg (TypeHint.subscriptions model.kmPackageTypeHint)
        , CommonForm.submitShortcut appState
            { result = model.savingKmEditor
            , onSubmit = FormMsg Form.Submit
            }
        ]



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    case model.selectedKmPackageUuid of
        Just _ ->
            Page.actionResult appState (viewCreate appState model << Just) model.kmPackage

        Nothing ->
            viewCreate appState model Nothing


viewCreate : AppState -> Model -> Maybe KnowledgeModelPackageDetail -> Html Msg
viewCreate appState model mbSelectedPackage =
    CommonForm.createPage appState
        { title = gettext "Create knowledge model" appState.locale
        , description = Nothing
        , actions = [ GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.kmEditorCreate ]
        , content = formView appState model mbSelectedPackage
        , submitLabel = gettext "Create" appState.locale
        , result = model.savingKmEditor
        , onSubmit = FormMsg Form.Submit
        , onCancel = Just Cancel
        }


formView : AppState -> Model -> Maybe KnowledgeModelPackageDetail -> List (Html Msg)
formView appState model mbSelectedPackage =
    let
        basedOnLabel =
            gettext "Based on" appState.locale

        basedOnHint =
            gettext "You can create a new knowledge model based on the existing one or start from scratch." appState.locale

        parentInput =
            case mbSelectedPackage of
                Just kmPackage ->
                    Ui.Field.readOnlyHtml
                        { id = previousPackageId
                        , label = basedOnLabel
                        , value = [ Text.code (KnowledgeModelUtils.getPackageId kmPackage) ]
                        }
                        |> Ui.Field.withHint basedOnHint
                        |> Ui.Field.toHtml

                Nothing ->
                    TypeHint.view appState
                        { id = previousPackageId
                        , label = basedOnLabel
                        , placeholder = "--"
                        , required = False
                        , error = Field.errorText appState basedOnLabel (Form.getFieldAsString "previousPackageUuid" model.form)
                        , hint = Just basedOnHint
                        , clearable = True
                        , viewItem =
                            \package ->
                                { label = package.name
                                , code = Just (KnowledgeModelUtils.getPackageId package)
                                , badges = RowBadge.owner appState package.workspaceUuid
                                , description = Just package.description
                                , icon = Nothing
                                }
                        , toKey = Uuid.toString << .uuid
                        , wrapMsg = KnowledgeModelPackageTypeHintMsg
                        }
                        model.kmPackageTypeHint

        previousVersion =
            if model.edit then
                ActionResult.toMaybe model.kmPackage
                    |> Maybe.map .version

            else
                Nothing
    in
    [ Field.input appState model.form "name" (gettext "Name" appState.locale)
        |> Field.toHtml
        |> Html.map FormMsg
    , Field.input appState model.form "id" (gettext "Knowledge Model ID" appState.locale)
        |> Field.withHint (gettext "Knowledge model ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale)
        |> Field.toHtml
        |> Html.map FormMsg
    , Field.versionWithSuggestions appState
        model.form
        { majorField = versionFields.majorField
        , minorField = versionFields.minorField
        , patchField = versionFields.patchField
        , label = gettext "New version" appState.locale
        , hint = Just (Field.versionHint appState)
        , suggestions = Field.versionSuggestions previousVersion
        , onSuggestion = FormSetVersion
        , toMsg = FormMsg
        }
    , Field.select appState model.form "language" (gettext "Language" appState.locale) LanguageCodes.options
        |> Field.toHtml
        |> Html.map FormMsg
    , parentInput
    ]
