module Wizard.Pages.Users.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role exposing (Role)
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Task.Extra as Task
import Uuid
import Wizard.Api.Models.User exposing (User)
import Wizard.Api.Roles as RolesApi
import Wizard.Api.Users as UsersApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Users.Index.Models exposing (Model)
import Wizard.Pages.Users.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch
        [ Cmd.map ListingMsg Listing.fetchData
        , RolesApi.getRoles appState GetRolesCompleted
        ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetRolesCompleted result ->
            getRolesCompleted appState model result

        ShowHideDeleteUser user ->
            ( { model | userToBeDeleted = user, deletingUser = Unset }, Cmd.none )

        DeleteUser ->
            handleDeleteUser wrapMsg appState model

        DeleteUserCompleted result ->
            deleteUserCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


getRolesCompleted : AppState -> Model -> Result ApiError (Pagination Role) -> ( Model, Cmd msg )
getRolesCompleted appState model result =
    let
        newModel =
            case result of
                Ok pagination ->
                    { model | roles = ActionResult.Success pagination.items }

                Err error ->
                    { model | roles = ApiError.toActionResult appState (gettext "Unable to get the roles." appState.locale) error }
    in
    ( newModel, Cmd.none )


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
