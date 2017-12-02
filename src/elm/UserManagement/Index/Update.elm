module UserManagement.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import Requests exposing (toCmd)
import UserManagement.Index.Models exposing (Model)
import UserManagement.Index.Msgs exposing (Msg(..))
import UserManagement.Models exposing (User)
import UserManagement.Requests exposing (deleteUser, getUsers)


getUsersCmd : Session -> Cmd Msgs.Msg
getUsersCmd session =
    getUsers session
        |> toCmd GetUsersCompleted Msgs.UserManagementIndexMsg


deleteUserCmd : String -> Session -> Cmd Msgs.Msg
deleteUserCmd uuid session =
    deleteUser uuid session
        |> toCmd DeleteUserCompleted Msgs.UserManagementIndexMsg


getUsersCompleted : Model -> Result Jwt.JwtError (List User) -> ( Model, Cmd Msgs.Msg )
getUsersCompleted model result =
    let
        newModel =
            case result of
                Ok users ->
                    { model | users = Success users }

                Err error ->
                    { model | users = Error "Unable to fetch user list" }
    in
    ( newModel, Cmd.none )


handleDeleteUser : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteUser session model =
    case model.userToBeDeleted of
        Just user ->
            ( { model | deletingUser = Loading }
            , deleteUserCmd user.uuid session
            )

        _ ->
            ( model, Cmd.none )


deleteUserCompleted : Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteUserCompleted session model result =
    case result of
        Ok user ->
            ( { model | deletingUser = Success "User was sucessfully deleted", users = Loading, userToBeDeleted = Nothing }
            , getUsersCmd session
            )

        Err error ->
            ( { model | deletingUser = Error "User could not be deleted" }
            , Cmd.none
            )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetUsersCompleted result ->
            getUsersCompleted model result

        ShowHideDeleteUser user ->
            ( { model | userToBeDeleted = user, deletingUser = Unset }, Cmd.none )

        DeleteUser ->
            handleDeleteUser session model

        DeleteUserCompleted result ->
            deleteUserCompleted session model result
