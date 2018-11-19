module Public.ForgottenPasswordConfirmation.Update exposing (handleForm, handlePutUserPasswordCompleted, putUserPasswordCmd, update)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (setFormErrors)
import Common.Models exposing (getServerError)
import Form
import Http
import Msgs
import Public.ForgottenPasswordConfirmation.Models exposing (..)
import Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))
import Public.ForgottenPasswordConfirmation.Requests exposing (putUserPassword)


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg model

        PutPasswordCompleted result ->
            handlePutUserPasswordCompleted result model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                cmd =
                    passwordForm
                        |> putUserPasswordCmd model
                        |> Cmd.map wrapMsg
            in
            ( { model | submitting = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update passwordFormValidation formMsg model.form }
            in
            ( newModel, Cmd.none )


putUserPasswordCmd : Model -> PasswordForm -> Cmd Msg
putUserPasswordCmd { userId, hash } form =
    form
        |> encodePasswordForm
        |> putUserPassword userId hash
        |> Http.send PutPasswordCompleted


handlePutUserPasswordCompleted : Result Http.Error String -> Model -> ( Model, Cmd Msgs.Msg )
handlePutUserPasswordCompleted result model =
    case result of
        Ok _ ->
            ( { model | submitting = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    setFormErrors error model.form

                errorMessage =
                    getServerError error "Forgotten password recovery failed."
            in
            ( { model | submitting = errorMessage, form = form }, Cmd.none )
