module Wizard.Public.ForgottenPassword.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api.ActionKeys as ActionKeysApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (setFormErrors)
import Wizard.Msgs
import Wizard.Public.Common.ForgottenPasswordForm as ForgottenPasswordForm
import Wizard.Public.ForgottenPassword.Models exposing (..)
import Wizard.Public.ForgottenPassword.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostForgottenPasswordCompleted result ->
            handlePostPasswordActionKeyCompleted appState result model


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just forgottenPasswordForm ) ->
            let
                body =
                    ForgottenPasswordForm.encode forgottenPasswordForm

                cmd =
                    Cmd.map wrapMsg <|
                        ActionKeysApi.postActionKey body appState PostForgottenPasswordCompleted
            in
            ( { model | submitting = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update ForgottenPasswordForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handlePostPasswordActionKeyCompleted : AppState -> Result ApiError () -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostPasswordActionKeyCompleted appState result model =
    case result of
        Ok _ ->
            ( { model | submitting = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    setFormErrors error model.form

                errorMessage =
                    ApiError.toActionResult (lg "apiError.actionKey.passwordRecoveryError" appState) error
            in
            ( { model | submitting = errorMessage, form = form }, Cmd.none )
