module Public.Signup.Update exposing (..)

import Common.Form exposing (getErrorMessage, setFormErrors)
import Common.Types exposing (ActionResult(..))
import Form
import Http
import Msgs
import Public.Signup.Models exposing (..)
import Public.Signup.Msgs exposing (Msg(..))
import Public.Signup.Requests exposing (postSignup)
import Random.Pcg exposing (Seed, step)
import Utils exposing (tuplePrepend)
import Uuid


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg seed model

        PostSignupCompleted result ->
            handlePostSignupCompleted result model |> tuplePrepend seed


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Seed -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg seed model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just signupForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                cmd =
                    Uuid.toString newUuid
                        |> postSignupCmd signupForm
                        |> Cmd.map wrapMsg
            in
            ( newSeed, { model | signingUp = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update signupFormValidation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


postSignupCmd : SignupForm -> String -> Cmd Msg
postSignupCmd form uuid =
    form
        |> encodeSignupForm uuid
        |> postSignup
        |> Http.send PostSignupCompleted


handlePostSignupCompleted : Result Http.Error String -> Model -> ( Model, Cmd Msgs.Msg )
handlePostSignupCompleted result model =
    case result of
        Ok _ ->
            ( { model | signingUp = Success "" }, Cmd.none )

        Err error ->
            let
                form =
                    setFormErrors error model.form

                errorMessage =
                    getErrorMessage error "Sign up process failed."
            in
            ( { model | signingUp = Error errorMessage, form = form }, Cmd.none )
