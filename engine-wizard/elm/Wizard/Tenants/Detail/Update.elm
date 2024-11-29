module Wizard.Tenants.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
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
import Wizard.Tenants.Common.TenantEditForm as AppEditForm
import Wizard.Tenants.Detail.Models exposing (Model)
import Wizard.Tenants.Detail.Msgs exposing (Msg(..))


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    TenantsApi.getTenant uuid appState GetAppComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetAppComplete result ->
            applyResult appState
                { setResult = setTenant
                , defaultError = gettext "Unable to get tenant." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        EditModalOpen ->
            ( { model | editForm = ActionResult.unwrap Nothing (Just << AppEditForm.init) model.tenant }, Cmd.none )

        EditModalClose ->
            ( { model | editForm = Nothing }, Cmd.none )

        EditModalFormMsg formMsg ->
            handleEditFormMsg formMsg wrapMsg appState model

        PutAppComplete result ->
            handlePutAppComplete appState model result


handleEditFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleEditFormMsg formMsg wrapMsg appState model =
    case model.editForm of
        Just form ->
            case ( formMsg, Form.getOutput form ) of
                ( Form.Submit, Just appEditForm ) ->
                    let
                        body =
                            AppEditForm.encode appEditForm

                        cmd =
                            Cmd.map wrapMsg <|
                                TenantsApi.putTenant model.uuid body appState PutAppComplete
                    in
                    ( { model | savingTenant = Loading }, cmd )

                _ ->
                    let
                        newModel =
                            { model | editForm = Just <| Form.update AppEditForm.validation formMsg form }
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
                | savingTenant = ApiError.toActionResult appState (gettext "Tenant could not be saved." appState.locale) error
                , editForm = Maybe.map (setFormErrors appState error) model.editForm
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
