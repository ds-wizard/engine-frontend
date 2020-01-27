module Wizard.Users.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (applyResultTransform, getResultCmd)
import Wizard.Common.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing
import Wizard.Common.Setters exposing (setUsers)
import Wizard.Msgs
import Wizard.Users.Common.User as User
import Wizard.Users.Index.Models exposing (Model)
import Wizard.Users.Index.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getUsers appState GetUsersCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetUsersCompleted result ->
            applyResultTransform
                { setResult = setUsers
                , defaultError = lg "apiError.users.getListError" appState
                , model = model
                , result = result
                , transform = Listing.modelFromList << List.sortWith User.compare
                }

        ShowHideDeleteUser user ->
            ( { model | userToBeDeleted = user, deletingUser = Unset }, Cmd.none )

        DeleteUser ->
            handleDeleteUser wrapMsg appState model

        DeleteUserCompleted result ->
            deleteUserCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            ( { model | users = ActionResult.map (Listing.update listingMsg) model.users }
            , Cmd.none
            )


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
