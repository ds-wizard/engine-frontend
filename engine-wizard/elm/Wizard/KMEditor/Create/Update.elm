module Wizard.KMEditor.Create.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Result exposing (Result)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Utils.RequestHelpers as RequestHelpers
import String.Normalize as Normalize
import Version exposing (Version)
import Wizard.Api.Branches as BranchesApi
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Packages as PackagesApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.KMEditor.Common.BranchCreateForm as BranchCreateForm exposing (BranchCreateForm)
import Wizard.KMEditor.Create.Models exposing (Model)
import Wizard.KMEditor.Create.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case ( model.selectedPackage, model.edit ) of
        ( Just packageId, True ) ->
            PackagesApi.getPackage appState packageId GetPackageCompleted

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl Routes.kmEditorIndex) )

        FormMsg formMsg ->
            handleFormMsg wrapMsg formMsg appState model

        FormSetVersion version ->
            handleFormSetVersion appState version model

        PostBranchCompleted result ->
            handlePostBranchCompleted appState model result

        PackageTypeHintInputMsg typeHintInputMsg ->
            handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model

        GetPackageCompleted result ->
            case result of
                Ok package ->
                    let
                        form =
                            model.form
                                |> setBranchCreateFormValue appState "name" package.name
                                |> setBranchCreateFormValue appState "kmId" package.kmId
                    in
                    ( { model | package = Success package, form = form }, Cmd.none )

                Err error ->
                    ( { model | package = ApiError.toActionResult appState (gettext "Unable to get the Knowledge Model." appState.locale) error }, Cmd.none )


handleFormMsg : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                body =
                    BranchCreateForm.encode kmCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        BranchesApi.postBranch appState body PostBranchCompleted
            in
            ( { model | savingBranch = Loading }, cmd )

        _ ->
            let
                newForm =
                    Form.update (BranchCreateForm.validation appState) formMsg model.form

                kmIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "kmId" model.form).value

                formWithKmId =
                    case ( formMsg, kmIdEmpty ) of
                        ( Form.Blur "name", True ) ->
                            let
                                suggestedKmId =
                                    (Form.getFieldAsString "name" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            setBranchCreateFormValue appState "kmId" suggestedKmId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithKmId }, Cmd.none )


handleFormSetVersion : AppState -> Version -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormSetVersion appState version model =
    let
        form =
            model.form
                |> setBranchCreateFormValue appState "versionMajor" (String.fromInt (Version.getMajor version))
                |> setBranchCreateFormValue appState "versionMinor" (String.fromInt (Version.getMinor version))
                |> setBranchCreateFormValue appState "versionPatch" (String.fromInt (Version.getPatch version))
    in
    ( { model | form = form }, Cmd.none )


handlePostBranchCompleted : AppState -> Model -> Result ApiError Branch -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostBranchCompleted appState model result =
    case result of
        Ok km ->
            ( model
            , cmdNavigate appState (Routes.kmEditorEditor km.uuid Nothing)
            )

        Err error ->
            ( { model
                | form = Form.setFormErrors appState error model.form
                , savingBranch = ApiError.toActionResult appState (gettext "Knowledge model could not be created." appState.locale) error
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg PackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        formMsg =
            wrapMsg << FormMsg << Form.Input "previousPackageId" Form.Select << Field.String

        cfg =
            { wrapMsg = wrapMsg << PackageTypeHintInputMsg
            , getTypeHints = PackagesApi.getPackagesSuggestions appState (Just False)
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg model.packageTypeHintInputModel
    in
    ( { model | packageTypeHintInputModel = packageTypeHintInputModel }, cmd )


setBranchCreateFormValue : AppState -> String -> String -> Form FormError BranchCreateForm -> Form FormError BranchCreateForm
setBranchCreateFormValue appState field value =
    Form.update (BranchCreateForm.validation appState) (Form.Input field Form.Text (Field.String value))
