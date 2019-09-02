module Public.ForgottenPasswordConfirmation.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Common.Locale exposing (lg)
import Form
import Msgs
import Public.Common.PasswordForm as PasswordForm
import Public.ForgottenPasswordConfirmation.Models exposing (..)
import Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PutPasswordCompleted result ->
            handlePutUserPasswordCompleted appState result model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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


handlePutUserPasswordCompleted : AppState -> Result ApiError () -> Model -> ( Model, Cmd Msgs.Msg )
handlePutUserPasswordCompleted appState result model =
    case result of
        Ok _ ->
            ( { model | submitting = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    setFormErrors error model.form

                errorMessage =
                    getServerError error <| lg "apiError.actionKey.passwordRecoveryError" appState
            in
            ( { model | submitting = errorMessage, form = form }, Cmd.none )
