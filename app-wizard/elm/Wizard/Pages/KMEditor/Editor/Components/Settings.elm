module Wizard.Pages.KMEditor.Editor.Components.Settings exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , setKnowledgeModelEditorDetail
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Form as Form
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Html exposing (Html, br, button, div, h2, hr, p, strong, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState exposing (KnowledgeModelEditorState)
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorEditForm as KnowledgeModelEditorEditForm exposing (KnowledgeModelEditorEditForm)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUtils as KnowledgeModelEditorUtils
import Wizard.Pages.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.Routes as Routes
import Wizard.Utils.HtmlAttributesUtils exposing (detailClass)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


type alias Model =
    { form : Form FormError KnowledgeModelEditorEditForm
    , savingKMEditor : ActionResult String
    , deleteModal : DeleteModal.Model
    , upgradeModal : UpgradeModal.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { form = KnowledgeModelEditorEditForm.initEmpty appState
    , savingKMEditor = ActionResult.Unset
    , deleteModal = DeleteModal.initialModel
    , upgradeModal = UpgradeModal.initialModel
    }


setKnowledgeModelEditorDetail : AppState -> KnowledgeModelEditorDetail -> Model -> Model
setKnowledgeModelEditorDetail appState kmEditor model =
    { model | form = KnowledgeModelEditorEditForm.init appState kmEditor }


type Msg
    = FormMsg Form.Msg
    | FormSetVersion Version
    | PutKnowledgeModelEditorComplete (Result ApiError ())
    | DeleteModalMsg DeleteModal.Msg
    | UpgradeModalMsg UpgradeModal.Msg


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , cmdNavigate : AppState -> Routes.Route -> Cmd msg
    , kmEditorUuid : Uuid
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        body =
                            KnowledgeModelEditorEditForm.encode form

                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                KnowledgeModelEditorsApi.putKnowledgeModelEditor appState cfg.kmEditorUuid body PutKnowledgeModelEditorComplete
                    in
                    ( { model | savingKMEditor = ActionResult.Loading }, cmd )

                _ ->
                    ( { model | form = Form.update (KnowledgeModelEditorEditForm.validation appState) formMsg model.form }, Cmd.none )

        FormSetVersion version ->
            let
                setFormValue field value =
                    Form.update (KnowledgeModelEditorEditForm.validation appState) (Form.Input field Form.Text (Field.String value))

                form =
                    model.form
                        |> setFormValue "versionMajor" (String.fromInt (Version.getMajor version))
                        |> setFormValue "versionMinor" (String.fromInt (Version.getMinor version))
                        |> setFormValue "versionPatch" (String.fromInt (Version.getPatch version))
            in
            ( { model | form = form }, Cmd.none )

        PutKnowledgeModelEditorComplete result ->
            case result of
                Ok _ ->
                    ( { model | savingKMEditor = ActionResult.Success "" }
                    , Window.refresh ()
                    )

                Err error ->
                    ( { model | savingKMEditor = ApiError.toActionResult appState (gettext "Knowledge model could not be saved." appState.locale) error }
                    , Cmd.none
                    )

        DeleteModalMsg deleteModalMsg ->
            let
                updateConfig =
                    { cmdDeleted = cfg.cmdNavigate appState Routes.kmEditorIndex
                    , wrapMsg = cfg.wrapMsg << DeleteModalMsg
                    }

                ( deleteModal, cmd ) =
                    DeleteModal.update updateConfig appState deleteModalMsg model.deleteModal
            in
            ( { model | deleteModal = deleteModal }, cmd )

        UpgradeModalMsg upgradeModalMsg ->
            let
                updateConfig =
                    { cmdUpgraded = cfg.cmdNavigate appState << Routes.kmEditorMigration
                    , wrapMsg = cfg.wrapMsg << UpgradeModalMsg
                    }

                ( upgradeModal, cmd ) =
                    UpgradeModal.update updateConfig appState upgradeModalMsg model.upgradeModal
            in
            ( { model | upgradeModal = upgradeModal }, cmd )


view : AppState -> KnowledgeModelEditorDetail -> Model -> Html Msg
view appState kmEditorDetail model =
    let
        parentKnowledgeModelView =
            case kmEditorDetail.forkOfKnowledgeModelPackage of
                Just forkOfPackage ->
                    [ hr [ class "separator" ] []
                    , parentKnowledgeModel appState kmEditorDetail.state forkOfPackage kmEditorDetail
                    ]

                Nothing ->
                    []

        versionInputConfig =
            { label = gettext "Version" appState.locale
            , majorField = "versionMajor"
            , minorField = "versionMinor"
            , patchField = "versionPatch"
            , currentVersion = KnowledgeModelEditorUtils.lastVersion appState kmEditorDetail
            , wrapFormMsg = FormMsg
            , setVersionMsg = Just FormSetVersion
            }

        formContent =
            div []
                ([ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
                 , Html.map FormMsg <| FormGroup.input appState.locale model.form "description" (gettext "Description" appState.locale)
                 , Html.map FormMsg <| FormGroup.input appState.locale model.form "kmId" (gettext "Knowledge Model ID" appState.locale)
                 , FormExtra.textAfter <| gettext "Knowledge model ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale
                 , FormGroup.version appState.locale versionInputConfig model.form
                 , Html.map FormMsg <| FormGroup.input appState.locale model.form "license" <| gettext "License" appState.locale
                 , Html.map FormMsg <| FormGroup.markdownEditor appState.locale (WizardGuideLinks.markdownCheatsheet appState.guideLinks) model.form "readme" <| gettext "Readme" appState.locale
                 ]
                    ++ parentKnowledgeModelView
                    ++ [ hr [ class "separator" ] []
                       , dangerZone appState kmEditorDetail
                       ]
                )

        form =
            Form.initDynamic appState (FormMsg Form.Submit) model.savingKMEditor
                |> Form.setFormView formContent
                |> Form.setFormChanged (Form.containsChanges model.form)
                |> Form.viewDynamic
    in
    div [ class "KMEditor__Editor__SettingsEditor", dataCy "km-editor_settings" ]
        [ div [ detailClass "" ]
            [ Page.headerWithGuideLink (AppState.toGuideLinkConfig appState WizardGuideLinks.kmEditorSettings) (gettext "Settings" appState.locale)
            , form
            ]
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModal
        , Html.map UpgradeModalMsg <| UpgradeModal.view appState model.upgradeModal
        ]


parentKnowledgeModel : AppState -> KnowledgeModelEditorState -> KnowledgeModelPackage -> KnowledgeModelEditorDetail -> Html Msg
parentKnowledgeModel appState kmEditorState forkOfPackage kmEditorDetail =
    let
        outdatedWarning =
            case kmEditorState of
                KnowledgeModelEditorState.Outdated ->
                    div [ class "alert alert-warning mt-2 d-flex justify-content-between align-items-center" ]
                        [ div [] [ text (gettext "This is not the latest version of the parent knowledge model." appState.locale) ]
                        , button
                            [ class "btn btn-warning"
                            , onClick (UpgradeModalMsg (UpgradeModal.open kmEditorDetail.uuid kmEditorDetail.name forkOfPackage.id))
                            ]
                            [ text (gettext "Update" appState.locale) ]
                        ]

                _ ->
                    Html.nothing
    in
    div []
        [ h2 [] [ text (gettext "Parent Knowledge Model" appState.locale) ]
        , linkTo (Routes.knowledgeModelsDetail forkOfPackage.id)
            [ class "package-link" ]
            [ TypeHintInputItem.packageSuggestionWithVersion (KnowledgeModelPackageSuggestion.fromKnowledgeModelPackage forkOfPackage) ]
        , outdatedWarning
        ]


dangerZone : AppState -> KnowledgeModelEditorDetail -> Html Msg
dangerZone appState kmEditorDetail =
    div [ class "pb-6" ]
        [ h2 [] [ text (gettext "Danger Zone" appState.locale) ]
        , div [ class "card border-danger mt-3" ]
            [ div [ class "card-body d-flex justify-content-between align-items-center" ]
                [ p [ class "card-text" ]
                    [ strong [] [ text (gettext "Delete this knowledge model editor" appState.locale) ]
                    , br [] []
                    , text (gettext "Deleted knowledge model editors cannot be recovered." appState.locale)
                    ]
                , button
                    [ class "btn btn-outline-danger"
                    , onClick (DeleteModalMsg (DeleteModal.open kmEditorDetail.uuid kmEditorDetail.name))
                    ]
                    [ text (gettext "Delete" appState.locale) ]
                ]
            ]
        ]
