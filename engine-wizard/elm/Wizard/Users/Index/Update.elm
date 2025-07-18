module Wizard.Users.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Uuid
import Wizard.Api.Models.User exposing (User)
import Wizard.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Users.Index.Models exposing (Model)
import Wizard.Users.Index.Msgs exposing (Msg(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeleteUser user ->
            ( { model | userToBeDeleted = user, deletingUser = Unset }, Cmd.none )

        DeleteUser ->
            handleDeleteUser wrapMsg appState model

        DeleteUserCompleted result ->
            deleteUserCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


handleDeleteUser : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteUser wrapMsg appState model =
    case model.userToBeDeleted of
        Just user ->
            ( { model | deletingUser = Loading }
            , Cmd.map wrapMsg <|
                UsersApi.deleteUser appState (Uuid.toString user.uuid) DeleteUserCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteUserCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteUserCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | userToBeDeleted = Nothing }
            , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
            )

        Err error ->
            ( { model | deletingUser = ApiError.toActionResult appState (gettext "User could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg User -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( users, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.users
    in
    ( { model | users = users }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig User
listingUpdateConfig wrapMsg appState =
    { getRequest = UsersApi.getUsers appState
    , getError = gettext "Unable to get users." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.usersIndexWithFilters
    }
