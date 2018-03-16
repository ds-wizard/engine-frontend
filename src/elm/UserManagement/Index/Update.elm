module UserManagement.Index.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import UserManagement.Common.Models exposing (User)
import UserManagement.Index.Models exposing (Model)
import UserManagement.Index.Msgs exposing (Msg(..))
import UserManagement.Requests exposing (deleteUser, getUsers)


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getUsersCmd session |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetUsersCompleted result ->
            getUsersCompleted model result

        ShowHideDeleteUser user ->
            ( { model | userToBeDeleted = user, deletingUser = Unset }, Cmd.none )

        DeleteUser ->
            handleDeleteUser wrapMsg session model |> Debug.log "Deleting user "

        DeleteUserCompleted result ->
            deleteUserCompleted wrapMsg session model result


getUsersCmd : Session -> Cmd Msg
getUsersCmd session =
    getUsers session |> Jwt.send GetUsersCompleted


deleteUserCmd : String -> Session -> Cmd Msg
deleteUserCmd uuid session =
    deleteUser uuid session |> Jwt.send DeleteUserCompleted


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


handleDeleteUser : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteUser wrapMsg session model =
    case model.userToBeDeleted of
        Just user ->
            ( { model | deletingUser = Loading }
            , deleteUserCmd user.uuid session |> Cmd.map wrapMsg
            )

        _ ->
            ( model, Cmd.none ) |> Debug.log "This awkward situation"


deleteUserCompleted : (Msg -> Msgs.Msg) -> Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteUserCompleted wrapMsg session model result =
    case result of
        Ok user ->
            ( { model | deletingUser = Success "User was sucessfully deleted", users = Loading, userToBeDeleted = Nothing }
            , getUsersCmd session |> Cmd.map wrapMsg
            )

        Err error ->
            ( { model | deletingUser = Error "User could not be deleted" }
            , Cmd.none
            )
