module Users.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (lg)
import Common.Setters exposing (setUsers)
import Msgs
import Users.Index.Models exposing (Model)
import Users.Index.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getUsers appState GetUsersCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetUsersCompleted result ->
            applyResult
                { setResult = setUsers
                , defaultError = lg "apiError.users.getListError" appState
                , model = model
                , result = result
                }

        ShowHideDeleteUser user ->
            ( { model | userToBeDeleted = user, deletingUser = Unset }, Cmd.none )

        DeleteUser ->
            handleDeleteUser wrapMsg appState model

        DeleteUserCompleted result ->
            deleteUserCompleted wrapMsg appState model result


handleDeleteUser : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteUser wrapMsg appState model =
    case model.userToBeDeleted of
        Just user ->
            ( { model | deletingUser = Loading }
            , Cmd.map wrapMsg <|
                UsersApi.deleteUser user.uuid appState DeleteUserCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteUserCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteUserCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingUser = Success <| lg "apiSuccess.users.delete" appState, users = Loading, userToBeDeleted = Nothing }
            , Cmd.map wrapMsg <| UsersApi.getUsers appState GetUsersCompleted
            )

        Err error ->
            ( { model | deletingUser = getServerError error <| lg "apiError.users.deleteError" appState }
            , getResultCmd result
            )
