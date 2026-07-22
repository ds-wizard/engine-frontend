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
import Common.Components.Modal as Modal
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, code, dd, div, dl, dt, text)
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
import Wizard.Pages.KMEditor.Common.PublishLocaleSelection as PublishLocaleSelection
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



-- MODEL


type alias Model =
    { open : Bool
    , publishing : ActionResult String
    , localeSelection : PublishLocaleSelection.Model
    }


initialModel : Model
initialModel =
    { open = False
    , publishing = ActionResult.Unset
    , localeSelection = PublishLocaleSelection.initialModel
    }



-- MSG


type Msg
    = SetOpen Bool
    | Publish
    | PublishCompleted (Result ApiError KnowledgeModelPackage)
    | PublishLocaleSelectionMsg PublishLocaleSelection.Msg


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
            if open then
                ( { model | open = True, localeSelection = PublishLocaleSelection.initialModel }
                , Cmd.map (cfg.wrapMsg << PublishLocaleSelectionMsg) (PublishLocaleSelection.fetchLocales appState cfg.kmEditorUuid)
                )

            else
                ( { model | open = False }, Cmd.none )

        Publish ->
            ( { model | publishing = ActionResult.Loading }
            , KnowledgeModelPackagesApi.postFromKnowledgeModelEditor appState cfg.kmEditorUuid (PublishLocaleSelection.selectedLocaleUuids model.localeSelection) (cfg.wrapMsg << PublishCompleted)
            )

        PublishLocaleSelectionMsg subMsg ->
            ( { model | localeSelection = PublishLocaleSelection.update appState subMsg model.localeSelection }, Cmd.none )

        PublishCompleted result ->
            case result of
                Ok kmPackage ->
                    ( model, cmdNavigate appState (Routes.knowledgeModelsDetail kmPackage.uuid) )

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
            , Html.map PublishLocaleSelectionMsg (PublishLocaleSelection.view appState model.localeSelection)
            , viewMetadata appState cfg.kmEditor
            , Markdown.toHtml [ class "form-control disabled cursor-default" ] cfg.kmEditor.readme
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Publish" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible model.open
                |> Modal.confirmConfigActionResult model.publishing
                |> Modal.confirmConfigAction (gettext "Publish" appState.locale) Publish
                |> Modal.confirmConfigActionEnabled (PublishLocaleSelection.ready model.localeSelection)
                |> Modal.confirmConfigCancelMsg (SetOpen False)
                |> Modal.confirmConfigExtraClass "modal-wide"
                |> Modal.confirmConfigGuideLinkConfig (AppState.toGuideLinkConfig appState WizardGuideLinks.kmEditorPublish)
                |> Modal.confirmConfigDataCy "km-editor_publish"
    in
    Modal.confirm appState modalConfig


viewMetadata : AppState -> KnowledgeModelEditorDetail -> Html msg
viewMetadata appState kmEditor =
    let
        row label value =
            [ dt [ class "col-sm-3" ] [ text label ]
            , dd [ class "col-sm-9" ] [ value ]
            ]
    in
    div [ class "card bg-light mb-2" ]
        [ div [ class "card-body" ]
            [ dl [ class "row mb-0" ]
                (row (gettext "Name" appState.locale) (text kmEditor.name)
                    ++ row (gettext "Description" appState.locale) (text kmEditor.description)
                    ++ row (gettext "Knowledge Model ID" appState.locale) (code [] [ text kmEditor.kmId ])
                    ++ row (gettext "Version" appState.locale) (text (Version.toString kmEditor.version))
                    ++ row (gettext "License" appState.locale) (text kmEditor.license)
                )
            ]
        ]
