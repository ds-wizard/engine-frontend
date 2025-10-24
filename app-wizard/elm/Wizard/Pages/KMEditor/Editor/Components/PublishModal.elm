module Wizard.Pages.KMEditor.Editor.Components.PublishModal exposing
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
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FontAwesome exposing (faSettings)
import Common.Components.FormGroup as FormGroup
import Common.Components.Modal as Modal
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String.Format as String
import Uuid exposing (Uuid)
import Version
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState as AppState exposing (AppState)
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
    | PublishCompleted (Result ApiError KnowledgeModelPackage)


openMsg : Msg
openMsg =
    SetOpen True



-- UPDATE


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , kmEditorUuid : Uuid
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        SetOpen open ->
            ( { model | open = open }, Cmd.none )

        Publish ->
            ( { model | publishing = ActionResult.Loading }
            , KnowledgeModelPackagesApi.postFromKnowledgeModelEditor appState cfg.kmEditorUuid (cfg.wrapMsg << PublishCompleted)
            )

        PublishCompleted result ->
            case result of
                Ok kmPackage ->
                    ( model, cmdNavigate appState (Routes.knowledgeModelsDetail kmPackage.id) )

                Err error ->
                    ( { model | publishing = ApiError.toActionResult appState (gettext "Unable to publish knowledge model" appState.locale) error }
                    , Cmd.none
                    )



-- VIEW


type alias ViewConfig =
    { kmEditor : KnowledgeModelEditorDetail }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    let
        info =
            div [ class "alert alert-info" ]
                (String.formatHtml (gettext "Check the knowledge model's metadata before publishing. You change them in %s." appState.locale)
                    [ linkTo (Routes.kmEditorEditorSettings cfg.kmEditor.uuid)
                        [ onClick (SetOpen False), class "btn-link with-icon" ]
                        [ faSettings
                        , text (gettext "Settings" appState.locale)
                        ]
                    ]
                )

        modalContent =
            [ info
            , FormGroup.readOnlyInput cfg.kmEditor.name (gettext "Name" appState.locale)
            , FormGroup.readOnlyInput cfg.kmEditor.description (gettext "Description" appState.locale)
            , FormGroup.readOnlyInput cfg.kmEditor.kmId (gettext "Knowledge Model ID" appState.locale)
            , FormGroup.readOnlyInput (Version.toString cfg.kmEditor.version) (gettext "Version" appState.locale)
            , FormGroup.readOnlyInput cfg.kmEditor.license (gettext "License" appState.locale)
            , FormGroup.plainGroup (Markdown.toHtml [ class "form-control disabled" ] cfg.kmEditor.readme) (gettext "Readme" appState.locale)
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Publish" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible model.open
                |> Modal.confirmConfigActionResult model.publishing
                |> Modal.confirmConfigAction (gettext "Publish" appState.locale) Publish
                |> Modal.confirmConfigCancelMsg (SetOpen False)
                |> Modal.confirmConfigExtraClass "modal-wide"
                |> Modal.confirmConfigGuideLinkConfig (AppState.toGuideLinkConfig appState WizardGuideLinks.kmEditorPublish)
                |> Modal.confirmConfigDataCy "km-editor_publish"
    in
    Modal.confirm appState modalConfig
