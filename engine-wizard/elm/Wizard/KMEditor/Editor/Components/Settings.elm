module Wizard.KMEditor.Editor.Components.Settings exposing
    ( Model
    , Msg
    , initialModel
    , setBranchDetail
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Html exposing (Html, br, button, div, h2, hr, p, strong)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Api.Branches as BranchesApi
import Shared.Data.Branch.BranchState as BranchState exposing (BranchState)
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lx)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchEditForm as BranchEditForm exposing (BranchEditForm)
import Wizard.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.KMEditor.Common.UpgradeModal as UpgradeModal
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.KnowledgeModels.Routes as KnowledgeModelsRoute
import Wizard.Ports as Ports
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.Components.Settings"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.Components.Settings"


type alias Model =
    { form : Form FormError BranchEditForm
    , savingBranch : ActionResult String
    , deleteModal : DeleteModal.Model
    , upgradeModal : UpgradeModal.Model
    }


initialModel : Model
initialModel =
    { form = BranchEditForm.initEmpty
    , savingBranch = ActionResult.Unset
    , deleteModal = DeleteModal.initialModel
    , upgradeModal = UpgradeModal.initialModel
    }


setBranchDetail : BranchDetail -> Model -> Model
setBranchDetail branch model =
    { model | form = BranchEditForm.init branch }


type Msg
    = FormMsg Form.Msg
    | PutBranchComplete (Result ApiError ())
    | DeleteModalMsg DeleteModal.Msg
    | UpgradeModalMsg UpgradeModal.Msg


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , cmdNavigate : AppState -> Routes.Route -> Cmd msg
    , branchUuid : Uuid
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just form ) ->
                    let
                        cmd =
                            Cmd.map cfg.wrapMsg <|
                                BranchesApi.putBranch cfg.branchUuid form.name form.kmId appState PutBranchComplete
                    in
                    ( { model | savingBranch = ActionResult.Loading }, cmd )

                _ ->
                    ( { model | form = Form.update BranchEditForm.validation formMsg model.form }, Cmd.none )

        PutBranchComplete result ->
            case result of
                Ok _ ->
                    ( { model | savingBranch = ActionResult.Unset }
                    , Ports.refresh ()
                    )

                Err error ->
                    ( { model | savingBranch = ApiError.toActionResult appState (lg "apiError.branches.putError" appState) error }
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
                    { cmdUpgraded = cfg.cmdNavigate appState << Routes.KMEditorRoute << MigrationRoute
                    , wrapMsg = cfg.wrapMsg << UpgradeModalMsg
                    }

                ( upgradeModal, cmd ) =
                    UpgradeModal.update updateConfig appState upgradeModalMsg model.upgradeModal
            in
            ( { model | upgradeModal = upgradeModal }, cmd )


view : AppState -> BranchDetail -> Model -> Html Msg
view appState branchDetail model =
    let
        parentKnowledgeModelView =
            case branchDetail.forkOfPackage of
                Just forkOfPackage ->
                    [ hr [ class "separator" ] []
                    , parentKnowledgeModel appState branchDetail.state forkOfPackage branchDetail
                    ]

                Nothing ->
                    []
    in
    div [ class "KMEditor__Editor__SettingsEditor", dataCy "km-editor_settings" ]
        [ div [ detailClass "" ]
            ([ Page.header (l_ "title" appState) []
             , div []
                [ FormResult.errorOnlyView appState model.savingBranch
                , Html.map FormMsg <| FormGroup.input appState model.form "name" (l_ "form.name" appState)
                , Html.map FormMsg <| FormGroup.input appState model.form "kmId" (l_ "form.kmId" appState)
                , FormActions.viewActionOnly appState
                    (ActionButton.ButtonConfig (l_ "form.save" appState) model.savingBranch (FormMsg Form.Submit) False)
                ]
             ]
                ++ parentKnowledgeModelView
                ++ [ hr [ class "separator" ] []
                   , dangerZone appState branchDetail
                   ]
            )
        , Html.map DeleteModalMsg <| DeleteModal.view appState model.deleteModal
        , Html.map UpgradeModalMsg <| UpgradeModal.view appState model.upgradeModal
        ]


parentKnowledgeModel : AppState -> BranchState -> PackageSuggestion -> BranchDetail -> Html Msg
parentKnowledgeModel appState branchState forkOfPackage branchDetail =
    let
        outdatedWarning =
            case branchState of
                BranchState.Outdated ->
                    div [ class "alert alert-warning mt-2 d-flex justify-content-between align-items-center" ]
                        [ div [] [ lx_ "parent.warning" appState ]
                        , button
                            [ class "btn btn-warning"
                            , onClick (UpgradeModalMsg (UpgradeModal.open branchDetail.uuid branchDetail.name forkOfPackage.id))
                            ]
                            [ lx_ "parent.upgrade" appState ]
                        ]

                _ ->
                    emptyNode
    in
    div []
        [ h2 [] [ lx_ "parent.title" appState ]
        , linkTo appState
            (Routes.KnowledgeModelsRoute (KnowledgeModelsRoute.DetailRoute forkOfPackage.id))
            [ class "package-link" ]
            [ TypeHintItem.packageSuggestion forkOfPackage ]
        , outdatedWarning
        ]


dangerZone : AppState -> BranchDetail -> Html Msg
dangerZone appState branchDetail =
    div []
        [ h2 [] [ lx_ "dangerZone.title" appState ]
        , div [ class "card border-danger mt-3" ]
            [ div [ class "card-body d-flex justify-content-between align-items-center" ]
                [ p [ class "card-text" ]
                    [ strong [] [ lx_ "dangerZone.deleteText" appState ]
                    , br [] []
                    , lx_ "dangerZone.deleteWarning" appState
                    ]
                , button
                    [ class "btn btn-outline-danger"
                    , onClick (DeleteModalMsg (DeleteModal.open branchDetail.uuid branchDetail.name))
                    ]
                    [ lx_ "dangerZone.delete" appState ]
                ]
            ]
        ]
