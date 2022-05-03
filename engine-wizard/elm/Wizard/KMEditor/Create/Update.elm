module Wizard.KMEditor.Create.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Field as Field
import Maybe.Extra as Maybe
import Result exposing (Result)
import Shared.Api.Branches as BranchesApi
import Shared.Api.Packages as PackagesApi
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (lg)
import String.Normalize as Normalize
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.KMEditor.Common.BranchCreateForm as BranchCreateForm exposing (BranchCreateForm)
import Wizard.KMEditor.Create.Models exposing (Model)
import Wizard.KMEditor.Create.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case ( model.selectedPackage, model.edit ) of
        ( Just packageId, True ) ->
            PackagesApi.getPackage packageId appState GetPackageCompleted

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg wrapMsg formMsg appState model

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
                                |> setBranchCreateFormValue "name" package.name
                                |> setBranchCreateFormValue "kmId" package.kmId
                    in
                    ( { model | package = Success package, form = form }, Cmd.none )

                Err error ->
                    ( { model | package = ApiError.toActionResult appState (lg "apiError.packages.getError" appState) error }, Cmd.none )


handleFormMsg : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just kmCreateForm ) ->
            let
                body =
                    BranchCreateForm.encode kmCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        BranchesApi.postBranch body appState PostBranchCompleted
            in
            ( { model | savingBranch = Loading }, cmd )

        _ ->
            let
                newForm =
                    Form.update BranchCreateForm.validation formMsg model.form

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
                            setBranchCreateFormValue "kmId" suggestedKmId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithKmId }, Cmd.none )


handlePostBranchCompleted : AppState -> Model -> Result ApiError Branch -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostBranchCompleted appState model result =
    case result of
        Ok km ->
            ( model
            , cmdNavigate appState (Routes.kmEditorEditor km.uuid Nothing)
            )

        Err error ->
            ( { model
                | form = setFormErrors appState error model.form
                , savingBranch = ApiError.toActionResult appState (lg "apiError.branches.postError" appState) error
              }
            , getResultCmd result
            )


handlePackageTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg PackageSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePackageTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        formMsg =
            wrapMsg << FormMsg << Form.Input "previousPackageId" Form.Select << Field.String

        cfg =
            { wrapMsg = wrapMsg << PackageTypeHintInputMsg
            , getTypeHints = PackagesApi.getPackagesSuggestions
            , getError = lg "apiError.packages.getListError" appState
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg appState model.packageTypeHintInputModel
    in
    ( { model | packageTypeHintInputModel = packageTypeHintInputModel }, cmd )


setBranchCreateFormValue : String -> String -> Form FormError BranchCreateForm -> Form FormError BranchCreateForm
setBranchCreateFormValue field value =
    Form.update BranchCreateForm.validation (Form.Input field Form.Text (Field.String value))
