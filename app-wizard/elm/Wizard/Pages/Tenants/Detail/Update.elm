module Wizard.Pages.Tenants.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Utils.Form as Form
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setTenant)
import Form
import Uuid exposing (Uuid)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Tenants.Common.TenantEditForm as TenantEditForm
import Wizard.Pages.Tenants.Common.TenantLimitsForm as TenantLimitsForm
import Wizard.Pages.Tenants.Detail.Models exposing (Model)
import Wizard.Pages.Tenants.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    TenantsApi.getTenant appState uuid GetTenantComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTenantComplete result ->
            RequestHelpers.applyResult
                { setResult = setTenant
                , defaultError = "Unable to get tenant."
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        EditModalOpen ->
            ( { model | editForm = ActionResult.unwrap Nothing (Just << TenantEditForm.init) model.tenant }, Cmd.none )

        EditModalClose ->
            ( { model | editForm = Nothing }, Cmd.none )

        EditModalFormMsg formMsg ->
            handleEditFormMsg formMsg wrapMsg appState model

        PutTenantComplete result ->
            handlePutAppComplete appState model result

        EditLimitsModalOpen ->
            ( { model | limitsForm = ActionResult.unwrap Nothing (Just << TenantLimitsForm.init) model.tenant }, Cmd.none )

        EditLimitsModalClose ->
            ( { model | limitsForm = Nothing }, Cmd.none )

        EditLimitsModalFormMsg formMsg ->
            handleEditLimitsFormMsg formMsg wrapMsg appState model

        PutTenantLimitsComplete result ->
            handlePutAppLimitsComplete appState model result


handleEditFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleEditFormMsg formMsg wrapMsg appState model =
    case model.editForm of
        Just form ->
            case ( formMsg, Form.getOutput form ) of
                ( Form.Submit, Just appEditForm ) ->
                    let
                        body =
                            TenantEditForm.encode appEditForm

                        cmd =
                            Cmd.map wrapMsg <|
                                TenantsApi.putTenant appState model.uuid body PutTenantComplete
                    in
                    ( { model | savingTenant = Loading }, cmd )

                _ ->
                    let
                        newModel =
                            { model | editForm = Just <| Form.update TenantEditForm.validation formMsg form }
                    in
                    ( newModel, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handlePutAppComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutAppComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.tenantsDetail model.uuid) )

        Err error ->
            ( { model
                | savingTenant = ApiError.toActionResult appState "Tenant could not be saved." error
                , editForm = Maybe.map (Form.setFormErrors appState error) model.editForm
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleEditLimitsFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleEditLimitsFormMsg formMsg wrapMsg appState model =
    case model.limitsForm of
        Just form ->
            case ( formMsg, Form.getOutput form ) of
                ( Form.Submit, Just appLimitsForm ) ->
                    let
                        body =
                            TenantLimitsForm.encode appLimitsForm

                        cmd =
                            Cmd.map wrapMsg <|
                                TenantsApi.putTenantLimits appState model.uuid body PutTenantLimitsComplete
                    in
                    ( { model | savingTenant = Loading }, cmd )

                _ ->
                    let
                        newModel =
                            { model | limitsForm = Just <| Form.update TenantLimitsForm.validation formMsg form }
                    in
                    ( newModel, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handlePutAppLimitsComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutAppLimitsComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.tenantsDetail model.uuid) )

        Err error ->
            ( { model
                | savingTenant = ApiError.toActionResult appState "Tenant limits could not be saved." error
                , limitsForm = Maybe.map (Form.setFormErrors appState error) model.limitsForm
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
