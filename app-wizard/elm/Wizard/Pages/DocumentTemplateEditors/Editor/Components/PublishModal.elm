module Wizard.Pages.DocumentTemplateEditors.Editor.Components.PublishModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , initialModel
    , openMsg
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Components.FontAwesome exposing (faSettings)
import Shared.Components.FormGroup as FormGroup
import Shared.Components.Modal as Modal
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Markdown as Markdown
import String.Format as String
import Version
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.TemplateEditor.DocumentTemplateForm as DocumentTemplateForm exposing (DocumentTemplateForm)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



-- MODEL


type alias Model =
    { open : Bool
    , publishing : ActionResult String
    }


initialModel : Model
initialModel =
    { open = False
    , publishing = ActionResult.Unset
    }



-- MSG


type Msg
    = SetOpen Bool
    | Publish
    | PublishCompleted (Result ApiError DocumentTemplateDraftDetail)


openMsg : Msg
openMsg =
    SetOpen True



-- UPDATE


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , documentTemplateId : String
    , documentTemplateForm : Maybe DocumentTemplateForm
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        SetOpen open ->
            ( { model | open = open }, Cmd.none )

        Publish ->
            case cfg.documentTemplateForm of
                Just documentTemplateForm ->
                    ( { model | publishing = ActionResult.Loading }
                    , DocumentTemplateDraftsApi.putDraft appState
                        cfg.documentTemplateId
                        (DocumentTemplateForm.encode DocumentTemplatePhase.Released documentTemplateForm)
                        (cfg.wrapMsg << PublishCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        PublishCompleted result ->
            case result of
                Ok documentTemplate ->
                    ( model, cmdNavigate appState (Routes.documentTemplatesDetail documentTemplate.id) )

                Err error ->
                    ( { model | publishing = ApiError.toActionResult appState (gettext "Unable to publish document template" appState.locale) error }
                    , Cmd.none
                    )



-- VIEW


type alias ViewConfig =
    { documentTemplate : DocumentTemplateDraftDetail }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    let
        info =
            div [ class "alert alert-info" ]
                (String.formatHtml (gettext "Check the document template's metadata before publishing. You change them in %s." appState.locale)
                    [ linkTo (Routes.documentTemplateEditorDetailSettings cfg.documentTemplate.id)
                        [ onClick (SetOpen False), class "btn-link with-icon" ]
                        [ faSettings
                        , text (gettext "Settings" appState.locale)
                        ]
                    ]
                )

        modalContent =
            [ info
            , FormGroup.readOnlyInput cfg.documentTemplate.name (gettext "Name" appState.locale)
            , FormGroup.readOnlyInput cfg.documentTemplate.description (gettext "Description" appState.locale)
            , FormGroup.readOnlyInput cfg.documentTemplate.templateId (gettext "Template ID" appState.locale)
            , FormGroup.readOnlyInput (Version.toString cfg.documentTemplate.version) (gettext "Version" appState.locale)
            , FormGroup.readOnlyInput cfg.documentTemplate.license (gettext "License" appState.locale)
            , FormGroup.plainGroup (Markdown.toHtml [ class "form-control disabled" ] cfg.documentTemplate.readme) (gettext "Readme" appState.locale)
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Publish" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible model.open
                |> Modal.confirmConfigActionResult model.publishing
                |> Modal.confirmConfigAction (gettext "Publish" appState.locale) Publish
                |> Modal.confirmConfigCancelMsg (SetOpen False)
                |> Modal.confirmConfigExtraClass "modal-wide"
                |> Modal.confirmConfigGuideLinkConfig (AppState.toGuideLinkConfig appState WizardGuideLinks.documentTemplatesPublish)
                |> Modal.confirmConfigDataCy "document-template-editor_publish"
    in
    Modal.confirm appState modalConfig
