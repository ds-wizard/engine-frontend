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
import Wizard.Tenants.Common.PlanForm as PlanForm
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

        AddPlanModalOpen ->
            ( { model | addPlanForm = Just PlanForm.initEmpty }, Cmd.none )

        AddPlanModalClose ->
            ( { model | addPlanForm = Nothing }, Cmd.none )

        AddPlanModalFormMsg formMsg ->
            handleAddPlanModalFormMsg formMsg wrapMsg appState model

        PostPlanComplete result ->
            handlePostPlanComplete appState model result

        EditPlanModalOpen plan ->
            ( { model | editPlanForm = Just ( plan.uuid, PlanForm.init appState plan ) }, Cmd.none )

        EditPlanModalClose ->
            ( { model | editPlanForm = Nothing }, Cmd.none )

        EditPlanModalFormMsg formMsg ->
            handleEditPlanModalFormMsg formMsg wrapMsg appState model

        PutPlanComplete result ->
            handlePutPlanComplete appState model result

        DeletePlanModalOpen plan ->
            ( { model | deletePlan = Just plan }, Cmd.none )

        DeletePlanModalClose ->
            ( { model | deletePlan = Nothing }, Cmd.none )

        DeletePlanModalConfirm ->
            handleDeletePlanModalConfirm wrapMsg appState model

        DeletePlanComplete result ->
            handleDeletePlanComplete appState model result


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


handleAddPlanModalFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleAddPlanModalFormMsg formMsg wrapMsg appState model =
    case model.addPlanForm of
        Just form ->
            case ( formMsg, Form.getOutput form ) of
                ( Form.Submit, Just addPlanForm ) ->
                    let
                        body =
                            PlanForm.encode appState addPlanForm

                        cmd =
                            Cmd.map wrapMsg <|
                                TenantsApi.postPlan model.uuid body appState PostPlanComplete
                    in
                    ( { model | addingPlan = Loading }, cmd )

                _ ->
                    let
                        newModel =
                            { model | addPlanForm = Just <| Form.update PlanForm.validation formMsg form }
                    in
                    ( newModel, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handlePostPlanComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostPlanComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.tenantsDetail model.uuid) )

        Err error ->
            ( { model
                | addingPlan = ApiError.toActionResult appState (gettext "Plan could not be created." appState.locale) error
                , addPlanForm = Maybe.map (setFormErrors appState error) model.addPlanForm
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleEditPlanModalFormMsg : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleEditPlanModalFormMsg formMsg wrapMsg appState model =
    case model.editPlanForm of
        Just ( planUuid, form ) ->
            case ( formMsg, Form.getOutput form ) of
                ( Form.Submit, Just editPlanForm ) ->
                    let
                        body =
                            PlanForm.encode appState editPlanForm

                        cmd =
                            Cmd.map wrapMsg <|
                                TenantsApi.putPlan model.uuid planUuid body appState PutPlanComplete
                    in
                    ( { model | editingPlan = Loading }, cmd )

                _ ->
                    let
                        newModel =
                            { model | editPlanForm = Just ( planUuid, Form.update PlanForm.validation formMsg form ) }
                    in
                    ( newModel, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


handlePutPlanComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutPlanComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.tenantsDetail model.uuid) )

        Err error ->
            ( { model
                | editingPlan = ApiError.toActionResult appState (gettext "Plan could not be saved." appState.locale) error
                , editPlanForm = Maybe.map (Tuple.mapSecond (setFormErrors appState error)) model.editPlanForm
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleDeletePlanModalConfirm : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePlanModalConfirm wrapMsg appState model =
    case model.deletePlan of
        Just plan ->
            let
                cmd =
                    Cmd.map wrapMsg <|
                        TenantsApi.deletePlan model.uuid plan.uuid appState DeletePlanComplete
            in
            ( { model | deletingPlan = Loading }, cmd )

        Nothing ->
            ( model, Cmd.none )


handleDeletePlanComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePlanComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.tenantsDetail model.uuid) )

        Err error ->
            ( { model
                | deletingPlan = ApiError.toActionResult appState (gettext "Plan could not be deleted." appState.locale) error
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
