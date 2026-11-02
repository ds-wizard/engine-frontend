module Common.Pages.Users.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Models.User as User exposing (User)
import Common.Api.Models.UserIdentity as UserIdentity
import Common.Api.Roles as RolesApi
import Common.Api.Users as UsersApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.EntityCard as EntityCard
import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.Page as Page
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.Users.Routes exposing (indexRouteRoleFilterId)
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Task.Extra as Task
import Ui.Badge as Badge
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { roles : ActionResult (List Role)
    , users : Listing.Model User
    , userToBeDeleted : Maybe User
    , deletingUser : ActionResult String
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbRole =
    { roles = ActionResult.Loading
    , users = Listing.initialModelWithFilters paginationQueryString (PaginationQueryFilters.fromValues [ ( indexRouteRoleFilterId, mbRole ) ])
    , userToBeDeleted = Nothing
    , deletingUser = Unset
    }


init : AppState -> PaginationQueryString -> Maybe String -> ( Model, Cmd Msg )
init appState paginationQueryString mbRole =
    let
        ( users, listingCmd ) =
            Listing.init paginationQueryString (PaginationQueryFilters.fromValues [ ( indexRouteRoleFilterId, mbRole ) ])
    in
    ( { roles = ActionResult.Loading
      , users = users
      , userToBeDeleted = Nothing
      , deletingUser = Unset
      }
    , Cmd.batch
        [ Cmd.map ListingMsg listingCmd
        , RolesApi.getRoles appState GetRolesCompleted
        ]
    )



-- UPDATE


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg User)
    | NoOp


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetRolesCompleted result ->
            case result of
                Ok pagination ->
                    ( { model | roles = Success pagination.items }, Cmd.none )

                Err error ->
                    ( { model | roles = ApiError.toActionResult appState (gettext "Unable to get the roles." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ShowHideDeleteUser mbUser ->
            -- Opening: the dialog takes focus, and the row's menu closes under
            -- it without taking it back. Dismissing: focus returns to the menu.
            let
                ( users, listingCmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus model.users
            in
            ( { model | userToBeDeleted = mbUser, deletingUser = Unset, users = users }
            , Cmd.batch
                [ listingCmd
                , case ( mbUser, model.userToBeDeleted ) of
                    ( Nothing, Just user ) ->
                        focusRowActions cfg user.uuid

                    _ ->
                        Cmd.none
                ]
            )

        DeleteUser ->
            case model.userToBeDeleted of
                Just user ->
                    ( { model | deletingUser = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        UsersApi.deleteUser appState (Uuid.toString user.uuid) DeleteUserCompleted
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteUserCompleted result ->
            case result of
                Ok _ ->
                    ( { model | userToBeDeleted = Nothing, deletingUser = Unset }
                    , Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    )

                Err error ->
                    ( { model | deletingUser = ApiError.toActionResult appState (gettext "User could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ListingMsg listingMsg ->
            let
                ( users, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.users
            in
            ( { model | users = users }, cmd )

        NoOp ->
            ( model, Cmd.none )


{-| The delete dialog a row's action opened was dismissed: focus goes back to
the row's menu, which the dialog had made inert. A task, so it runs once the
dialog has closed.
-}
focusRowActions : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusRowActions cfg uuid =
    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString uuid)))


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig User msg
listingUpdateConfig cfg appState =
    { getRequest = UsersApi.getUsers appState
    , getError = gettext "Unable to get users." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.usersIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.users)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewList appState model) model.roles


viewList : AppState -> Model -> List Role -> Html Msg
viewList appState model roles =
    Listing.viewWith appState
        { notices = [], overlays = [ deleteModal appState model ] }
        (listingConfig appState roles)
        model.users


{-| "Create", or the ways of adding users the client offers instead
(`AppState.addUserLinks`).
-}
addButtons : AppState -> List (Button msg)
addButtons appState =
    case appState.addUserLinks of
        [] ->
            [ Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState Routes.usersCreate)
                |> Button.withIcon (Icon.solid "fa-plus")
                |> Button.withTestId "users_create-button"
            ]

        links ->
            -- The others first and the main action last, as on every list.
            let
                ( primary, others ) =
                    List.partition .primary links

                button toButton link =
                    toButton link.label link.url
                        |> Button.withIcon (Icon.solid link.icon)
                        |> Button.withTestId link.testId
            in
            List.map (button Button.link) others ++ List.map (button Button.linkPrimary) primary


listingConfig : AppState -> List Role -> Listing.ViewConfig User Msg
listingConfig appState roles =
    { title = gettext "Users" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = addButtons appState
    , searchPlaceholder = gettext "Search users..." appState.locale
    , sortOptions =
        [ ( "firstName", gettext "First name" appState.locale )
        , ( "lastName", gettext "Last name" appState.locale )
        , ( "email", gettext "Email" appState.locale )
        , ( "createdAt", gettext "Created" appState.locale )
        , ( "updatedAt", gettext "Updated" appState.locale )
        , ( "lastVisitedAt", gettext "Last online" appState.locale )
        ]
    , filters =
        [ Listing.SingleFilter indexRouteRoleFilterId
            { name = gettext "Role" appState.locale
            , options = Role.toFormOptions appState.locale roles
            }
        ]
    , item = listingItem appState
    , emptyTitle = gettext "No users" appState.locale
    , emptyDescription =
        if List.isEmpty appState.addUserLinks then
            gettext "Click \"Create\" button to add a new User." appState.locale

        else
            ""
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> User -> Listing.Item Msg
listingItem appState user =
    let
        editUrl =
            Routes.toUrl appState (Routes.usersEdit (UuidOrCurrent.uuid user.uuid))

        base =
            Listing.item
                { id = Uuid.toString user.uuid
                , title = Link.toHtml (Link.link { label = User.fullName user, url = editUrl })
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ User.fullName user ]
                }

        activeBadge =
            if user.active then
                []

            else
                [ Badge.danger (gettext "inactive" appState.locale) ]

        affiliationFragment =
            case user.affiliation of
                Just affiliation ->
                    [ Html.text affiliation ]

                Nothing ->
                    []

        externalIdentitiesFragment =
            if List.isEmpty user.externalIdentities then
                []

            else
                [ externalIdentities user ]

        editAction =
            ItemActions.link
                { icon = Icon.solid "fa-pen-to-square"
                , label = gettext "Edit" appState.locale
                , url = editUrl
                }
                |> ItemActions.withTestId "listing-item_action_edit"

        deleteAction =
            ItemActions.dangerAction
                { icon = Icon.solid "fa-trash"
                , label = gettext "Delete" appState.locale
                , onSelect = ShowHideDeleteUser (Just user)
                }
                |> ItemActions.withTestId "listing-item_action_delete"
    in
    { base
        | icon = Just (ItemIcon.toHtml (avatar user))
        , badges = roleBadge appState user :: activeBadge
        , fragments = email user :: affiliationFragment ++ externalIdentitiesFragment
        , timestamp = Just (Timestamp.updated appState user.updatedAt)
        , actions =
            ItemActions.groups
                [ [ ( editAction, True ) ]
                , [ ( deleteAction, True ) ]
                ]
    }


avatar : User -> ItemIcon.ItemIcon
avatar user =
    UserAvatar.user
        { firstName = user.firstName
        , lastName = user.lastName
        , imageUrl = user.imageUrl
        , gravatarHash = UserAvatar.emailHash user.email
        }


roleBadge : AppState -> User -> Badge.Badge msg
roleBadge appState user =
    Badge.neutral (Role.localizedName appState.locale user.role)


{-| The providers the user has connected an account from, each in its own
colors, as the login screen shows them.
-}
externalIdentities : User -> Html msg
externalIdentities user =
    user.externalIdentities
        |> List.sortWith UserIdentity.compare
        |> List.map (\identity -> { name = identity.providerName, style = identity.providerStyle })
        |> ExternalLoginButton.badges


email : User -> Html msg
email user =
    Html.text user.email


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        -- The same element whether or not a user is chosen: the dialog must not
        -- gain or lose children while it is on the page.
        entity =
            EntityCard.slot
                (Maybe.map
                    (\user ->
                        { icon = avatar user
                        , name = User.fullName user
                        , badges = [ roleBadge appState user ]
                        , code = Nothing
                        , detail = Just user.email
                        }
                    )
                    model.userToBeDeleted
                )
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete user" appState.locale
        , open = Maybe.isJust model.userToBeDeleted
        , question = gettext "Are you sure you want to permanently delete the following user?" appState.locale
        , entity = entity
        , onConfirm = DeleteUser
        , onClose = ShowHideDeleteUser Nothing
        , result = model.deletingUser
        , testId = Just "users-delete"
        }
