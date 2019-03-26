module Public.Signup.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Form
import Msgs
import Public.Signup.Models exposing (..)
import Public.Signup.Msgs exposing (Msg(..))
import Random exposing (Seed, step)
import Utils exposing (tuplePrepend)
import Uuid


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostSignupCompleted result ->
            handlePostSignupCompleted result model |> tuplePrepend appState.seed


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just signupForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator appState.seed

                body =
                    encodeSignupForm (Uuid.toString newUuid) signupForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.postUserPublic body appState PostSignupCompleted
            in
            ( newSeed, { model | signingUp = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update signupFormValidation formMsg model.form }
            in
            ( appState.seed, newModel, Cmd.none )


handlePostSignupCompleted : Result ApiError () -> Model -> ( Model, Cmd Msgs.Msg )
handlePostSignupCompleted result model =
    case result of
        Ok _ ->
            ( { model | signingUp = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    setFormErrors error model.form

                errorMessage =
                    getServerError error "Sign up process failed."
            in
            ( { model | signingUp = errorMessage, form = form }, Cmd.none )
