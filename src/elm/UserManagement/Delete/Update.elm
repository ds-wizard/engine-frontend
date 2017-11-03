module UserManagement.Delete.Update exposing (..)

import Auth.Models exposing (Session)
import Jwt
import Msgs
import Routing exposing (Route(..), cmdNavigate)
import UserManagement.Delete.Models exposing (Model)
import UserManagement.Delete.Msgs exposing (Msg(..))
import UserManagement.Models exposing (User)
import UserManagement.Requests exposing (deleteUser, getUser)


getUserCmd : String -> Session -> Cmd Msgs.Msg
getUserCmd uuid session =
    Jwt.send GetUserCompleted (getUser uuid session) |> Cmd.map Msgs.UserManagementDeleteMsg


deleteUserCmd : String -> Session -> Cmd Msgs.Msg
deleteUserCmd uuid session =
    Jwt.send DeleteUserCompleted (deleteUser uuid session) |> Cmd.map Msgs.UserManagementDeleteMsg


getUserCompleted : Model -> Result Jwt.JwtError User -> ( Model, Cmd Msgs.Msg )
getUserCompleted model result =
    let
        newModel =
            case result of
                Ok user ->
                    { model | user = Just user, loadingUser = False }

                Err error ->
                    { model | error = "Unable to get user information,", loadingUser = False }
    in
    ( newModel, Cmd.none )


deleteUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteUserCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate UserManagement )

        Err error ->
            ( { model | error = "User could not be deleted." }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetUserCompleted result ->
            getUserCompleted model result

        DeleteUser ->
            case model.user of
                Just user ->
                    ( { model | deletingUser = True }, deleteUserCmd user.uuid session )

                Nothing ->
                    ( model, Cmd.none )

        DeleteUserCompleted result ->
            deleteUserCompleted model result
