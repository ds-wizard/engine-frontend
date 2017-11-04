module UserManagement.Create.Update exposing (..)

import Auth.Models exposing (Session)
import Form exposing (Form)
import Jwt
import Msgs
import Random.Pcg exposing (step)
import Routing exposing (Route(..), cmdNavigate)
import UserManagement.Create.Models exposing (Model)
import UserManagement.Create.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)
import UserManagement.Requests exposing (postUser)
import Uuid


postUserCmd : Session -> UserCreateForm -> String -> Cmd Msgs.Msg
postUserCmd session form uuid =
    form
        |> encodeUserCreateForm uuid
        |> postUser session
        |> Jwt.send PostUserCompleted
        |> Cmd.map Msgs.UserManagementCreateMsg


postUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postUserCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate UserManagement )

        Err error ->
            ( { model | error = "User could not be created.", savingUser = False }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just userCreateForm ) ->
                    let
                        ( newUuid, newSeed ) =
                            step Uuid.uuidGenerator model.currentSeed

                        cmd =
                            postUserCmd session userCreateForm (Uuid.toString newUuid)
                    in
                    ( { model | currentSeed = newSeed, savingUser = True }, cmd )

                _ ->
                    ( { model | form = Form.update userCreateFormValidation formMsg model.form }, Cmd.none )

        PostUserCompleted result ->
            postUserCompleted model result
