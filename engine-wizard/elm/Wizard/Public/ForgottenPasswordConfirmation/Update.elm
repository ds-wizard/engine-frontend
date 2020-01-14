module Wizard.Public.ForgottenPasswordConfirmation.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (setFormErrors)
import Wizard.Msgs
import Wizard.Public.Common.PasswordForm as PasswordForm
import Wizard.Public.ForgottenPasswordConfirmation.Models exposing (..)
import Wizard.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PutPasswordCompleted result ->
            handlePutUserPasswordCompleted appState result model


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                body =
                    PasswordForm.encode passwordForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.putUserPasswordPublic model.userId model.hash body appState PutPasswordCompleted
            in
            ( { model | submitting = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update PasswordForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handlePutUserPasswordCompleted : AppState -> Result ApiError () -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePutUserPasswordCompleted appState result model =
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
