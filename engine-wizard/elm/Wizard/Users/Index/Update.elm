module Wizard.Users.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
import Wizard.Common.Setters exposing (setUsers)
import Wizard.Msgs
import Wizard.Users.Index.Models exposing (Model)
import Wizard.Users.Index.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getUsers appState GetUsersCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


handleDeleteUser : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteUser wrapMsg appState model =
    case model.userToBeDeleted of
        Just user ->
            ( { model | deletingUser = Loading }
            , Cmd.map wrapMsg <|
                UsersApi.deleteUser user.uuid appState DeleteUserCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteUserCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteUserCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | deletingUser = Success <| lg "apiSuccess.users.delete" appState, users = Loading, userToBeDeleted = Nothing }
            , Cmd.map wrapMsg <| UsersApi.getUsers appState GetUsersCompleted
            )

        Err error ->
            ( { model | deletingUser = ApiError.toActionResult (lg "apiError.users.deleteError" appState) error }
            , getResultCmd result
            )
