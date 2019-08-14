module Public.Signup.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Common.Locale exposing (lg)
import Form
import Msgs
import Public.Common.SignupForm as SignupForm
import Public.Signup.Models exposing (..)
import Public.Signup.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostSignupCompleted result ->
            handlePostSignupCompleted appState result model


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just signupForm ) ->
            let
                body =
                    SignupForm.encode signupForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.postUserPublic body appState PostSignupCompleted
            in
            ( { model | signingUp = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update SignupForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


handlePostSignupCompleted : AppState -> Result ApiError () -> Model -> ( Model, Cmd Msgs.Msg )
handlePostSignupCompleted appState result model =
    case result of
        Ok _ ->
            ( { model | signingUp = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    setFormErrors error model.form

                errorMessage =
                    getServerError error <| lg "apiError.users.public.postError" appState
            in
            ( { model | signingUp = errorMessage, form = form }, Cmd.none )
