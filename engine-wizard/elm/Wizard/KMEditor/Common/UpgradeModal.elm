module Wizard.KMEditor.Common.UpgradeModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (Html, p, strong, text)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Api.Branches as BranchesApi
import Shared.Api.Packages as PackagesApi
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lg, lh)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm exposing (BranchUpgradeForm)


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Common.UpgradeModal"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KMEditor.Common.UpgradeModal"


type alias Model =
    { branch : Maybe ( Uuid, String )
    , branchUpgradeForm : Form FormError BranchUpgradeForm
    , creatingMigration : ActionResult String
    , package : ActionResult PackageDetail
    }


initialModel : Model
initialModel =
    { branch = Nothing
    , branchUpgradeForm = BranchUpgradeForm.init
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
                | branch = Just ( uuid, name )
                , creatingMigration = ActionResult.Unset
                , package = ActionResult.Loading
              }
            , Cmd.map cfg.wrapMsg <| PackagesApi.getPackage forkOfPackageId appState GetPackageComplete
            )

        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.branchUpgradeForm, model.branch ) of
                ( Form.Submit, Just branchUpgradeForm, Just ( uuid, _ ) ) ->
                    let
                        body =
                            BranchUpgradeForm.encode branchUpgradeForm
                    in
                    ( { model | creatingMigration = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg <| BranchesApi.postMigration uuid body appState UpgradeComplete
                    )

                _ ->
                    ( { model | branchUpgradeForm = Form.update BranchUpgradeForm.validation formMsg model.branchUpgradeForm }
                    , Cmd.none
                    )

        UpgradeComplete result ->
            case result of
                Ok _ ->
                    let
                        kmUuid =
                            Maybe.unwrap Uuid.nil Tuple.first model.branch
                    in
                    ( { model | branch = Nothing }, cfg.cmdUpgraded kmUuid )

                Err error ->
                    ( { model | creatingMigration = ApiError.toActionResult appState (lg "apiError.branches.migrations.postError" appState) error }
                    , Cmd.none
                    )

        GetPackageComplete result ->
            case result of
                Ok package ->
                    ( { model | package = ActionResult.Success package }, Cmd.none )

                Err error ->
                    ( { model | package = ApiError.toActionResult appState (lg "apiError.packages.getError" appState) error }
                    , Cmd.none
                    )

        Close ->
            ( { model | branch = Nothing }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        ( visible, name ) =
            case model.branch of
                Just ( _, branchName ) ->
                    ( True, branchName )

                Nothing ->
                    ( False, "" )

        options =
            case model.package of
                Success package ->
                    ( "", l_ "form.defaultOption" appState ) :: PackageDetail.createFormOptions package

                _ ->
                    []

        modalContent =
            case model.package of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ Page.loader appState ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success _ ->
                    [ p [ class "alert alert-info" ]
                        (lh_ "text" [ strong [] [ text name ] ] appState)
                    , FormGroup.select appState options model.branchUpgradeForm "targetPackageId" (l_ "form.targetPackageId" appState)
                        |> Html.map FormMsg
                    ]

        modalConfig =
            { modalTitle = l_ "title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingMigration
            , actionName = l_ "action" appState
            , actionMsg = FormMsg Form.Submit
            , cancelMsg = Just Close
            , dangerous = False
            , dataCy = "km-editor-upgrade"
            }
    in
    Modal.confirm appState modalConfig
