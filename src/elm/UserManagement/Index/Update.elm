module UserManagement.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import Requests exposing (toCmd)
import UserManagement.Index.Models exposing (Model)
import UserManagement.Index.Msgs exposing (Msg(..))
import UserManagement.Models exposing (User)
import UserManagement.Requests exposing (getUsers)


getUsersCmd : Session -> Cmd Msgs.Msg
getUsersCmd session =
    getUsers session
        |> toCmd GetUsersCompleted Msgs.UserManagementIndexMsg


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


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetUsersCompleted result ->
            getUsersCompleted model result
