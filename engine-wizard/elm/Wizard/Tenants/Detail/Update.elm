module Wizard.Tenants.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Api.Tenants as TenantsApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Shared.Setters exposing (setTenant)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Tenants.Common.TenantEditForm as TenantEditForm
import Wizard.Tenants.Common.TenantLimitsForm as TenantLimitsForm
import Wizard.Tenants.Detail.Models exposing (Model)
import Wizard.Tenants.Detail.Msgs exposing (Msg(..))


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    TenantsApi.getTenant uuid appState GetTenantComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTenantComplete result ->
            applyResult appState
                { setResult = setTenant
                , defaultError = "Unable to get tenant."
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
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
                                TenantsApi.putTenant model.uuid body appState PutTenantComplete
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
                , editForm = Maybe.map (setFormErrors appState error) model.editForm
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
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
                                TenantsApi.putTenantLimits model.uuid body appState PutTenantLimitsComplete
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
                , limitsForm = Maybe.map (setFormErrors appState error) model.limitsForm
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
