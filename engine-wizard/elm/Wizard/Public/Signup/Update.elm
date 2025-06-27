module Wizard.Public.Signup.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Wizard.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.Common.SignupForm as SignupForm
import Wizard.Public.Signup.Models exposing (Model)
import Wizard.Public.Signup.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostSignupCompleted result ->
            handlePostSignupCompleted appState result model


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just signupForm ) ->
            if signupForm.acceptFake then
                ( { model | signingUp = Success "" }, Cmd.none )

            else
                let
                    body =
                        SignupForm.encode signupForm

                    cmd =
                        Cmd.map wrapMsg <|
                            UsersApi.postUserPublic appState body PostSignupCompleted
                in
                ( { model | signingUp = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update (SignupForm.validation appState) formMsg model.form }
            in
            ( newModel, Cmd.none )


handlePostSignupCompleted : AppState -> Result ApiError () -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostSignupCompleted appState result model =
    case result of
        Ok _ ->
            ( { model | signingUp = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    Form.setFormErrors appState error model.form

                errorMessage =
                    ApiError.toActionResult appState (gettext "Sign up process failed." appState.locale) error
            in
            ( { model | signingUp = errorMessage, form = form }, Cmd.none )
