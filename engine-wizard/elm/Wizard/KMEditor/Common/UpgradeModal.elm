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
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Api.Branches as BranchesApi
import Shared.Api.Packages as PackagesApi
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm exposing (BranchUpgradeForm)


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

        modalContent =
            case model.package of
                Unset ->
                    [ emptyNode ]

                Loading ->
                    [ Page.loader appState ]

                Error error ->
                    [ p [ class "alert alert-danger" ] [ text error ] ]

                Success _ ->
                    let
                        options =
                            case model.package of
                                Success package ->
                                    ( "", gettext "- select parent package -" appState.locale ) :: PackageDetail.createFormOptions package

                                _ ->
                                    []
                    in
                    [ p [ class "alert alert-info" ]
                        (String.formatHtml (gettext "Select the new parent knowledge model for %s." appState.locale) [ strong [] [ text name ] ])
                    , FormGroup.select appState options model.branchUpgradeForm "targetPackageId" (gettext "New parent package" appState.locale)
                        |> Html.map FormMsg
                    ]

        modalConfig =
            { modalTitle = gettext "Create migration" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.creatingMigration
            , actionName = gettext "Create" appState.locale
            , actionMsg = FormMsg Form.Submit
            , cancelMsg = Just Close
            , dangerous = False
            , dataCy = "km-editor-upgrade"
            }
    in
    Modal.confirm appState modalConfig
