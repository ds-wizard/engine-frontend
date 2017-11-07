module UserManagement.Create.Update exposing (..)

import Auth.Models exposing (Session)
import Form exposing (Form)
import Jwt
import Msgs
import Random.Pcg exposing (Seed, step)
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import UserManagement.Create.Models exposing (Model)
import UserManagement.Create.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)
import UserManagement.Requests exposing (postUser)
import Utils exposing (tuplePrepend)
import Uuid


postUserCmd : Session -> UserCreateForm -> String -> Cmd Msgs.Msg
postUserCmd session form uuid =
    form
        |> encodeUserCreateForm uuid
        |> postUser session
        |> toCmd PostUserCompleted Msgs.UserManagementCreateMsg


postUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postUserCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate UserManagement )

        Err error ->
            ( { model | error = "User could not be created.", savingUser = False }, Cmd.none )


handleForm : Form.Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg seed session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just userCreateForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                cmd =
                    Uuid.toString newUuid |> postUserCmd session userCreateForm
            in
            ( newSeed, { model | savingUser = True }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update userCreateFormValidation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


update : Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg seed session model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg seed session model

        PostUserCompleted result ->
            postUserCompleted model result |> tuplePrepend seed
