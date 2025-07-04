module Wizard.Settings.Generic.Update exposing
    ( UpdateProps
    , fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate exposing (Validation)
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Models.EditableConfig as EditableConfig exposing (EditableConfig)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Settings.Generic.Model exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))


type alias UpdateProps form =
    { initForm : EditableConfig -> Form FormError form
    , formToConfig : form -> EditableConfig -> EditableConfig
    , formValidation : Validation FormError form
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    TenantsApi.getCurrentConfig appState GetConfigCompleted


update :
    UpdateProps form
    -> (Msg -> Wizard.Msgs.Msg)
    -> Msg
    -> AppState
    -> Model form
    -> ( Model form, Cmd Wizard.Msgs.Msg )
update props wrapMsg msg appState model =
    case msg of
        GetConfigCompleted result ->
            handleGetConfigCompleted props appState model result

        PutConfigCompleted result ->
            handlePutConfigCompleted props appState model result

        FormMsg formMsg ->
            handleForm props formMsg wrapMsg appState model


handleGetConfigCompleted :
    UpdateProps form
    -> AppState
    -> Model form
    -> Result ApiError EditableConfig
    -> ( Model form, Cmd Wizard.Msgs.Msg )
handleGetConfigCompleted props appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = props.initForm config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult appState (gettext "Unable to load settings." appState.locale) error }

        cmd =
            RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
    in
    ( newModel, cmd )


handlePutConfigCompleted :
    UpdateProps form
    -> AppState
    -> Model form
    -> Result ApiError ()
    -> ( Model form, Cmd Wizard.Msgs.Msg )
handlePutConfigCompleted _ appState model result =
    let
        ( newResult, cmd ) =
            case result of
                Ok _ ->
                    ( Success ()
                    , Ports.refresh ()
                    )

                Err error ->
                    ( ApiError.toActionResult appState (gettext "Settings could not be saved." appState.locale) error
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )
    in
    ( { model | savingConfig = newResult }, Cmd.batch [ cmd, Ports.scrollToTop ".Settings__content" ] )


handleForm :
    UpdateProps form
    -> Form.Msg
    -> (Msg -> Wizard.Msgs.Msg)
    -> AppState
    -> Model form
    -> ( Model form, Cmd Wizard.Msgs.Msg )
handleForm props formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form, model.config ) of
        ( Form.Submit, Just form, Success config ) ->
            let
                body =
                    EditableConfig.encode <| props.formToConfig form config

                cmd =
                    Cmd.map wrapMsg <|
                        TenantsApi.putCurrentConfig appState body PutConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                formRemoved =
                    case formMsg of
                        Form.RemoveItem _ _ ->
                            True

                        _ ->
                            model.formRemoved

                form =
                    Form.update props.formValidation formMsg model.form
            in
            ( { model
                | form = form
                , formRemoved = formRemoved
              }
            , Cmd.none
            )
