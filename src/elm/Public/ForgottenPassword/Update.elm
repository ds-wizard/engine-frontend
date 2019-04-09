module Public.ForgottenPassword.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ActionKeys as ActionKeysApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Form
import Msgs
import Public.ForgottenPassword.Models exposing (..)
import Public.ForgottenPassword.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostForgottenPasswordCompleted result ->
            handlePostPasswordActionKeyCompleted result model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just forgottenPasswordForm ) ->
            let
                body =
                    encodeForgottenPasswordForm forgottenPasswordForm

                cmd =
                    Cmd.map wrapMsg <|
                        ActionKeysApi.postActionKey body appState PostForgottenPasswordCompleted
            in
            ( { model | submitting = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update forgottenPasswordFormValidation formMsg model.form }
            in
            ( newModel, Cmd.none )


handlePostPasswordActionKeyCompleted : Result ApiError () -> Model -> ( Model, Cmd Msgs.Msg )
handlePostPasswordActionKeyCompleted result model =
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
