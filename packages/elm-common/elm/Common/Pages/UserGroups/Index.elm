module Common.Pages.UserGroups.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| The user groups of a workspace. The reader's own are badged, and the
membership filter narrows to them or to the others; the ones the reader may
change can be edited and deleted from their row.
-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.UserGroup exposing (UserGroup)
import Common.Api.UserGroups as UserGroupsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.EntityCard as EntityCard
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.UserGroups.Routes as UserGroupsRoutes exposing (indexRouteMembershipFilterId)
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
import Uuid


type alias Model =
    { userGroups : Listing.Model UserGroup
    , userGroupToBeDeleted : Maybe UserGroup
    , deletingUserGroup : ActionResult String
    }


type Msg
    = ListingMsg (Listing.Msg UserGroup)
    | ShowHideDelete (Maybe UserGroup)
    | Delete
    | DeleteCompleted (Result ApiError ())
    | NoOp


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel pqs mbMember =
    { userGroups = Listing.initialModelWithFilters pqs (PaginationQueryFilters.fromValues [ ( indexRouteMembershipFilterId, mbMember ) ])
    , userGroupToBeDeleted = Nothing
    , deletingUserGroup = Unset
    }


init : PaginationQueryString -> Maybe String -> ( Model, Cmd Msg )
init pqs mbMember =
    let
        ( userGroups, cmd ) =
            Listing.init pqs (PaginationQueryFilters.fromValues [ ( indexRouteMembershipFilterId, mbMember ) ])
    in
    ( { userGroups = userGroups
      , userGroupToBeDeleted = Nothing
      , deletingUserGroup = Unset
      }
    , Cmd.map ListingMsg cmd
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        listing listingMsg =
            Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.userGroups
    in
    case msg of
        ListingMsg listingMsg ->
            let
                ( userGroups, cmd ) =
                    listing listingMsg
            in
            ( { model | userGroups = userGroups }, cmd )

        ShowHideDelete mbUserGroup ->
            ( { model
                | userGroupToBeDeleted = mbUserGroup
                , deletingUserGroup = Unset
                , userGroups = Tuple.first (listing Listing.closeMenus)
              }
            , case ( mbUserGroup, model.userGroupToBeDeleted ) of
                ( Nothing, Just userGroup ) ->
                    -- The dialog is gone and the row is still there: back to
                    -- the menu it was opened from.
                    Task.attempt (always (cfg.wrapMsg NoOp)) (Dom.focus (Listing.itemActionsTriggerId (Uuid.toString userGroup.uuid)))

                _ ->
                    Cmd.none
            )

        Delete ->
            case model.userGroupToBeDeleted of
                Just userGroup ->
                    ( { model | deletingUserGroup = Loading }
                    , UserGroupsApi.deleteUserGroup appState userGroup.uuid (cfg.wrapMsg << DeleteCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted result ->
            case result of
                Ok _ ->
                    ( { model | userGroupToBeDeleted = Nothing, deletingUserGroup = Unset }
                    , Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete))
                    )

                Err error ->
                    ( { model | deletingUserGroup = ApiError.toActionResult appState (gettext "User group could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        NoOp ->
            ( model, Cmd.none )


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig UserGroup msg
listingUpdateConfig cfg appState =
    { getRequest = UserGroupsApi.getUserGroups appState
    , getError = gettext "Unable to get user groups." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl =
        \filters pqs ->
            Routes.toUrl appState (Routes.UserGroupsRoute (UserGroupsRoutes.Index pqs (PaginationQueryFilters.getValue indexRouteMembershipFilterId filters)))
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.userGroups)


view : AppState -> Model -> Html Msg
view appState model =
    let
        toolbarActions =
            if Feature.userGroupsCreate appState then
                [ createButton appState ]

            else
                []
    in
    Listing.viewWith appState
        { notices = [], overlays = [ deleteModal appState model ] }
        { title = gettext "User Groups" appState.locale
        , description = Nothing
        , breadcrumbs = Nothing
        , toolbarActions = toolbarActions
        , searchPlaceholder = gettext "Search user groups..." appState.locale
        , sortOptions =
            [ ( "name", gettext "Name" appState.locale )
            , ( "updatedAt", gettext "Updated" appState.locale )
            ]
        , filters =
            [ Listing.SingleFilter indexRouteMembershipFilterId
                { name = gettext "Membership" appState.locale
                , options =
                    [ ( "true", gettext "Member" appState.locale )
                    , ( "false", gettext "Non-member" appState.locale )
                    ]
                }
            ]
        , item = listingItem appState
        , emptyTitle = gettext "No user groups" appState.locale
        , emptyDescription =
            if Feature.userGroupsCreate appState then
                gettext "Click \"Create\" button to create a new user group." appState.locale

            else
                gettext "There are no user groups." appState.locale
        , wrapMsg = ListingMsg
        }
        model.userGroups


createButton : AppState -> Button msg
createButton appState =
    Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState Routes.userGroupsCreate)
        |> Button.withIcon (Icon.solid "fa-plus")
        |> Button.withTestId "user-groups_create-button"


listingItem : AppState -> UserGroup -> Listing.Item Msg
listingItem appState userGroup =
    let
        detailUrl =
            Routes.toUrl appState (Routes.UserGroupsRoute (UserGroupsRoutes.Detail userGroup.uuid UserGroupsRoutes.Info))

        base =
            Listing.item
                { id = Uuid.toString userGroup.uuid
                , title = Link.toHtml (Link.link { label = userGroup.name, url = detailUrl })
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ userGroup.name ]
                }

        viewAction =
            ItemActions.link
                { icon = Icon.regular "fa-eye"
                , label = gettext "View" appState.locale
                , url = detailUrl
                }
                |> ItemActions.withTestId "listing-item_action_view"

        editAction =
            ItemActions.link
                { icon = Icon.solid "fa-pen-to-square"
                , label = gettext "Edit" appState.locale
                , url = detailUrl
                }
                |> ItemActions.withTestId "listing-item_action_edit"

        deleteAction =
            ItemActions.dangerAction
                { icon = Icon.regular "fa-trash-can"
                , label = gettext "Delete" appState.locale
                , onSelect = ShowHideDelete (Just userGroup)
                }
                |> ItemActions.withTestId "listing-item_action_delete"
    in
    { base
        | icon = Just (ItemIcon.toHtml (UserAvatar.item userGroup.name Nothing))
        , badges = badges appState userGroup
        , description = Maybe.withDefault "" userGroup.description
        , timestamp = Just (Timestamp.updated appState userGroup.updatedAt)
        , actions =
            ItemActions.groups
                [ [ ( viewAction, not userGroup.editable ) ]
                , [ ( editAction, userGroup.editable ) ]
                , [ ( deleteAction, userGroup.editable ) ]
                ]
    }


badges : AppState -> UserGroup -> List (Badge.Badge msg)
badges appState userGroup =
    Maybe.values
        [ if userGroup.member then
            Just (Badge.info (gettext "member" appState.locale))

          else
            Nothing
        , if userGroup.private then
            Just (Badge.neutral (gettext "private" appState.locale))

          else
            Nothing
        ]


deleteModal : AppState -> Model -> Html Msg
deleteModal appState model =
    let
        entity =
            EntityCard.slot
                (Maybe.map
                    (\userGroup ->
                        { icon = UserAvatar.group userGroup.name
                        , name = userGroup.name
                        , badges =
                            if userGroup.private then
                                [ Badge.neutral (gettext "private" appState.locale) ]

                            else
                                []
                        , code = Nothing
                        , detail = userGroup.description
                        }
                    )
                    model.userGroupToBeDeleted
                )
    in
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete User Group" appState.locale
        , open = Maybe.isJust model.userGroupToBeDeleted
        , question = gettext "Are you sure you want to permanently delete the following user group?" appState.locale
        , entity = entity
        , onConfirm = Delete
        , onClose = ShowHideDelete Nothing
        , result = model.deletingUserGroup
        , testId = Just "user-groups-delete"
        }
