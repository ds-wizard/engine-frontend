module Wizard.Apps.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Api.Apps as AppsApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setApp)
import Uuid exposing (Uuid)
import Wizard.Apps.Common.AppEditForm as AppEditForm
import Wizard.Apps.Common.PlanForm as PlanForm
import Wizard.Apps.Detail.Models exposing (Model)
import Wizard.Apps.Detail.Msgs exposing (Msg(..))
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState uuid =
    AppsApi.getApp uuid appState GetAppComplete


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetAppComplete result ->
            applyResult appState
                { setResult = setApp
                , defaultError = lg "apiError.apps.getError" appState
                , model = model
                , result = result
                }

        EditModalOpen ->
            ( { model | editForm = ActionResult.unwrap Nothing (Just << AppEditForm.init) model.app }, Cmd.none )

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
                                AppsApi.putApp model.uuid body appState PutAppComplete
                    in
                    ( { model | savingApp = Loading }, cmd )

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
            ( model, cmdNavigate appState (Routes.appsDetail model.uuid) )

        Err error ->
            ( { model
                | savingApp = ApiError.toActionResult appState (lg "apiError.apps.putError" appState) error
                , editForm = Maybe.map (setFormErrors appState error) model.editForm
              }
            , getResultCmd result
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
                                AppsApi.postPlan model.uuid body appState PostPlanComplete
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
            ( model, cmdNavigate appState (Routes.appsDetail model.uuid) )

        Err error ->
            ( { model
                | addingPlan = ApiError.toActionResult appState (lg "apiError.apps.postPlanError" appState) error
                , addPlanForm = Maybe.map (setFormErrors appState error) model.addPlanForm
              }
            , getResultCmd result
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
                                AppsApi.putPlan model.uuid planUuid body appState PutPlanComplete
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
            ( model, cmdNavigate appState (Routes.appsDetail model.uuid) )

        Err error ->
            ( { model
                | editingPlan = ApiError.toActionResult appState (lg "apiError.apps.putPlanError" appState) error
                , editPlanForm = Maybe.map (Tuple.mapSecond (setFormErrors appState error)) model.editPlanForm
              }
            , getResultCmd result
            )


handleDeletePlanModalConfirm : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePlanModalConfirm wrapMsg appState model =
    case model.deletePlan of
        Just plan ->
            let
                cmd =
                    Cmd.map wrapMsg <|
                        AppsApi.deletePlan model.uuid plan.uuid appState DeletePlanComplete
            in
            ( { model | deletingPlan = Loading }, cmd )

        Nothing ->
            ( model, Cmd.none )


handleDeletePlanComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePlanComplete appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState (Routes.appsDetail model.uuid) )

        Err error ->
            ( { model
                | deletingPlan = ApiError.toActionResult appState (lg "apiError.apps.deletePlanError" appState) error
              }
            , getResultCmd result
            )
