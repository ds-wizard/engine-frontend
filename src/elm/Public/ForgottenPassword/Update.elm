module Public.ForgottenPassword.Update exposing (..)

import Common.Form exposing (setFormErrors)
import Common.Models exposing (getServerError)
import Common.Types exposing (ActionResult(..))
import Form
import Http
import Msgs
import Public.ForgottenPassword.Models exposing (..)
import Public.ForgottenPassword.Msgs exposing (Msg(..))
import Public.ForgottenPassword.Requests exposing (postPasswordActionKey)


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg model

        PostForgottenPasswordCompleted result ->
            handlePostPasswordActionKeyCompleted result model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just forgottenPasswordForm ) ->
            let
                cmd =
                    postPasswordActionKeyCmd forgottenPasswordForm |> Cmd.map wrapMsg
            in
            ( { model | submitting = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update forgottenPasswordFormValidation formMsg model.form }
            in
            ( newModel, Cmd.none )


postPasswordActionKeyCmd : ForgottenPasswordForm -> Cmd Msg
postPasswordActionKeyCmd form =
    form
        |> encodeForgottenPasswordForm
        |> postPasswordActionKey
        |> Http.send PostForgottenPasswordCompleted


handlePostPasswordActionKeyCompleted : Result Http.Error String -> Model -> ( Model, Cmd Msgs.Msg )
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
