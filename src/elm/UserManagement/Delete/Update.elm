module UserManagement.Delete.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import UserManagement.Delete.Models exposing (Model)
import UserManagement.Delete.Msgs exposing (Msg(..))
import UserManagement.Models exposing (User)
import UserManagement.Requests exposing (deleteUser, getUser)


getUserCmd : String -> Session -> Cmd Msgs.Msg
getUserCmd uuid session =
    getUser uuid session
        |> toCmd GetUserCompleted Msgs.UserManagementDeleteMsg


deleteUserCmd : String -> Session -> Cmd Msgs.Msg
deleteUserCmd uuid session =
    deleteUser uuid session
        |> toCmd DeleteUserCompleted Msgs.UserManagementDeleteMsg


getUserCompleted : Model -> Result Jwt.JwtError User -> ( Model, Cmd Msgs.Msg )
getUserCompleted model result =
    let
        newModel =
            case result of
                Ok user ->
                    { model | user = Success user }

                Err error ->
                    { model | user = Error "Unable to get user profile." }
    in
    ( newModel, Cmd.none )


deleteUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteUserCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate UserManagement )

        Err error ->
            ( { model | deletingUser = Error "User could not be deleted." }, Cmd.none )


handleDeleteUser : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteUser session model =
    case model.user of
        Success user ->
            ( { model | deletingUser = Loading }, deleteUserCmd user.uuid session )

        _ ->
            ( model, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetUserCompleted result ->
            getUserCompleted model result

        DeleteUser ->
            handleDeleteUser session model

        DeleteUserCompleted result ->
            deleteUserCompleted model result
