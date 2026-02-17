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
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.FormGroup as FormGroup
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Maybe.Extra as Maybe
import String.Format as String
import Uuid exposing (Uuid)
import Version exposing (Version)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUpgradeForm as KnowledgeModelEditorUpgradeForm exposing (KnowledgeModelEditorUpgradeForm)
import Wizard.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


type alias Model =
    { kmEditor : Maybe ( Uuid, String )
    , forkOfPackageVersion : Maybe Version
    , kmEditorUpgradeForm : Form FormError KnowledgeModelEditorUpgradeForm
    , creatingMigration : ActionResult String
    , kmPackages : ActionResult (Pagination KnowledgeModelPackage)
    }


initialModel : Model
initialModel =
    { kmEditor = Nothing
    , forkOfPackageVersion = Nothing
    , kmEditorUpgradeForm = KnowledgeModelEditorUpgradeForm.init
    , creatingMigration = ActionResult.Unset
    , kmPackages = ActionResult.Unset
    }


type Msg
    = Open Uuid String (Maybe String)
    | FormMsg Form.Msg
    | UpgradeComplete (Result ApiError ())
    | GetKnowledgeModelPackageComplete (Result ApiError (Pagination KnowledgeModelPackage))
    | Close


open : Uuid -> String -> Maybe String -> Msg
open uuid name mbForkOfPackageUuid =
    Open uuid name mbForkOfPackageUuid


type alias UpdateConfig msg =
    { cmdUpgraded : Uuid -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open uuid name mbForkOfPackageUuid ->
            case Maybe.map (String.split ":") mbForkOfPackageUuid of
                Just (organizationId :: kmId :: versionString :: []) ->
                    let
                        filters =
                            PaginationQueryFilters.fromValues
                                [ ( "organizationId", Just organizationId )
                                , ( "kmId", Just kmId )
                                ]

                        pqs =
                            PaginationQueryString.empty |> PaginationQueryString.withSize (Just 999)
                    in
                    ( { model
                        | kmEditor = Just ( uuid, name )
                        , forkOfPackageVersion = Version.fromString versionString
                        , creatingMigration = ActionResult.Unset
                        , kmPackages = ActionResult.Loading
                      }
                    , Cmd.map cfg.wrapMsg <| KnowledgeModelPackagesApi.getKnowledgeModelPackages appState filters pqs GetKnowledgeModelPackageComplete
                    )

                _ ->
                    ( model, Cmd.none )

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

        GetKnowledgeModelPackageComplete result ->
            case result of
                Ok kmPackage ->
                    ( { model | kmPackages = ActionResult.Success kmPackage }, Cmd.none )

                Err error ->
                    ( { model | kmPackages = ApiError.toActionResult appState (gettext "Unable to get the Knowledge Model." appState.locale) error }
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
            case model.kmPackages of
                Unset ->
                    [ Html.nothing ]

                Loading ->
                    [ Page.loader appState ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success results ->
                    let
                        previousVersion =
                            Maybe.withDefault (Version.create 0 0 0) model.forkOfPackageVersion

                        createFormOption kmPackage =
                            let
                                id =
                                    KnowledgeModelUtils.getPackageId kmPackage

                                optionText =
                                    kmPackage.name ++ " " ++ Version.toString kmPackage.version ++ " (" ++ id ++ ")"
                            in
                            ( id, optionText )

                        kmOptions =
                            results.items
                                |> List.filter (Version.greaterThan previousVersion << .version)
                                |> List.sortWith (\a b -> Version.compare a.version b.version)
                                |> List.map createFormOption

                        options =
                            ( "", gettext "- select parent knowledge model -" appState.locale ) :: kmOptions
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
