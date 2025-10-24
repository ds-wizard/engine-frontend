module Wizard.Pages.KMEditor.Common.UpgradeModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FormGroup as FormGroup
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.PackageDetail as PackageDetail exposing (PackageDetail)
import Wizard.Api.Packages as PackagesApi
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUpgradeForm as KnowledgeModelEditorUpgradeForm exposing (KnowledgeModelEditorUpgradeForm)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


type alias Model =
    { kmEditor : Maybe ( Uuid, String )
    , kmEditorUpgradeForm : Form FormError KnowledgeModelEditorUpgradeForm
    , creatingMigration : ActionResult String
    , package : ActionResult PackageDetail
    }


initialModel : Model
initialModel =
    { kmEditor = Nothing
    , kmEditorUpgradeForm = KnowledgeModelEditorUpgradeForm.init
    , creatingMigration = ActionResult.Unset
    , package = ActionResult.Unset
    }


type Msg
    = Open Uuid String String
    | FormMsg Form.Msg
    | UpgradeComplete (Result ApiError ())
    | GetPackageComplete (Result ApiError PackageDetail)
    | Close


open : Uuid -> String -> String -> Msg
open uuid name forkOfPackageId =
    Open uuid name forkOfPackageId


type alias UpdateConfig msg =
    { cmdUpgraded : Uuid -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open uuid name forkOfPackageId ->
            ( { model
                | kmEditor = Just ( uuid, name )
                , creatingMigration = ActionResult.Unset
                , package = ActionResult.Loading
              }
            , Cmd.map cfg.wrapMsg <| PackagesApi.getPackage appState forkOfPackageId GetPackageComplete
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.kmEditorUpgradeForm, model.kmEditor ) of
                ( Form.Submit, Just kmEditorUpgradeForm, Just ( uuid, _ ) ) ->
                    let
                        body =
                            KnowledgeModelEditorUpgradeForm.encode kmEditorUpgradeForm
                    in
                    ( { model | creatingMigration = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <| KnowledgeModelEditorsApi.postMigration appState uuid body UpgradeComplete
                    )

                _ ->
                    ( { model | kmEditorUpgradeForm = Form.update KnowledgeModelEditorUpgradeForm.validation formMsg model.kmEditorUpgradeForm }
                    , Cmd.none
                    )

        UpgradeComplete result ->
            case result of
                Ok _ ->
                    let
                        kmUuid =
                            Maybe.unwrap Uuid.nil Tuple.first model.kmEditor
                    in
                    ( { model | kmEditor = Nothing }, cfg.cmdUpgraded kmUuid )

                Err error ->
                    ( { model | creatingMigration = ApiError.toActionResult appState (gettext "Migration could not be created." appState.locale) error }
                    , Cmd.none
                    )

        GetPackageComplete result ->
            case result of
                Ok package ->
                    ( { model | package = ActionResult.Success package }, Cmd.none )

                Err error ->
                    ( { model | package = ApiError.toActionResult appState (gettext "Unable to get the Knowledge Model." appState.locale) error }
                    , Cmd.none
                    )

        Close ->
            ( { model | kmEditor = Nothing }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.kmEditor of
                Just ( _, kmEditorName ) ->
                    ( True, kmEditorName )

                Nothing ->
                    ( False, "" )

        modalContent =
            case model.package of
                Unset ->
                    [ Html.nothing ]

                Loading ->
                    [ Page.loader appState ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success _ ->
                    let
                        options =
                            case model.package of
                                Success package ->
                                    ( "", gettext "- select parent knowledge model -" appState.locale ) :: PackageDetail.createFormOptions package

                                _ ->
                                    []
                    in
                    [ p [ class "alert alert-info" ]
                        (String.formatHtml (gettext "Select the new parent knowledge model for %s." appState.locale) [ strong [] [ text name ] ])
                    , FormGroup.select appState.locale options model.kmEditorUpgradeForm "targetPackageId" (gettext "New parent knowledge model" appState.locale)
                        |> Html.map FormMsg
                    ]

        modalConfig =
            Modal.confirmConfig (gettext "Create migration" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.creatingMigration
                |> Modal.confirmConfigAction (gettext "Create" appState.locale) (FormMsg Form.Submit)
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigGuideLinkConfig (AppState.toGuideLinkConfig appState WizardGuideLinks.kmEditorMigration)
                |> Modal.confirmConfigDataCy "km-editor-update"
    in
    Modal.confirm appState modalConfig
