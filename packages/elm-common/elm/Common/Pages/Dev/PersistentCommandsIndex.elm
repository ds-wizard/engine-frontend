module Common.Pages.Dev.PersistentCommandsIndex exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState
import Common.Api.PersistentCommands as PersistentCommandsApi
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.Listing as Listing
import Common.Components.PersistentCommand as PersistentCommandBadge
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Dev.Components.PersistentCommandActions as PersistentCommandActions
import Common.Pages.Dev.Routes exposing (persistentCommandIndexRouteStateFilterId)
import Common.Ports.Window as Window
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Task.Extra as Task
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Text as Text
import Uuid


type alias Model =
    { persistentCommands : Listing.Model PersistentCommand
    , updating : ActionResult String
    }


type Msg
    = ListingMsg (Listing.Msg PersistentCommand)
    | RetryFailed
    | RetryFailedComplete (Result ApiError ())
    | RerunCommand PersistentCommand
    | SetIgnored PersistentCommand
    | UpdateComplete (Result ApiError ())
    | NoOp


initialFilters : Maybe String -> PaginationQueryFilters
initialFilters mbState =
    PaginationQueryFilters.create [ ( persistentCommandIndexRouteStateFilterId, mbState ) ] []


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbState =
    { persistentCommands = Listing.initialModelWithFilters paginationQueryString (initialFilters mbState)
    , updating = Unset
    }


init : PaginationQueryString -> Maybe String -> ( Model, Cmd Msg )
init paginationQueryString mbState =
    let
        ( persistentCommands, cmd ) =
            Listing.init paginationQueryString (initialFilters mbState)
    in
    ( { persistentCommands = persistentCommands, updating = Unset }
    , Cmd.map ListingMsg cmd
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( persistentCommands, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.persistentCommands
            in
            ( { model | persistentCommands = persistentCommands }, cmd )

        RetryFailed ->
            ( { model | updating = Loading }
            , PersistentCommandsApi.retryAllFailed appState (cfg.wrapMsg << RetryFailedComplete)
            )

        RetryFailedComplete result ->
            case result of
                Ok _ ->
                    ( model, Window.refresh () )

                Err error ->
                    ( { model | updating = ApiError.toActionResult appState (gettext "Unable to retry all failed persistent commands." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , ApiErrorAlert.focus (cfg.wrapMsg NoOp)
                        ]
                    )

        RerunCommand persistentCommand ->
            ( { model | persistentCommands = closeMenus cfg appState model.persistentCommands }
            , PersistentCommandsApi.retry appState persistentCommand.uuid (cfg.wrapMsg << UpdateComplete)
            )

        SetIgnored persistentCommand ->
            ( { model | persistentCommands = closeMenus cfg appState model.persistentCommands }
            , PersistentCommandsApi.updateState appState persistentCommand.uuid PersistentCommandState.Ignore (cfg.wrapMsg << UpdateComplete)
            )

        UpdateComplete result ->
            case result of
                Ok _ ->
                    ( model, Task.dispatch (cfg.wrapMsg (ListingMsg Listing.onAfterDelete)) )

                Err error ->
                    ( { model | updating = ApiError.toActionResult appState (gettext "Persistent command update failed." appState.locale) error }
                    , Cmd.batch
                        [ RequestHelpers.getResultCmd cfg.logoutMsg result
                        , ApiErrorAlert.focus (cfg.wrapMsg NoOp)
                        ]
                    )

        NoOp ->
            ( model, Cmd.none )


{-| A row action closes the menu it was chosen from, without moving focus: the
list reloads when it succeeds, and the error is shown above the list when it
fails.
-}
closeMenus : UpdateConfig Msg msg -> AppState -> Listing.Model PersistentCommand -> Listing.Model PersistentCommand
closeMenus cfg appState persistentCommands =
    Tuple.first (Listing.update appState (listingUpdateConfig cfg appState) Listing.closeMenus persistentCommands)


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig PersistentCommand msg
listingUpdateConfig cfg appState =
    { getRequest = PersistentCommandsApi.getPersistentCommands appState
    , getError = gettext "Unable to get persistent commands." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.persistentCommandsIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.persistentCommands)



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Listing.viewWith appState
        { notices = [ ApiErrorAlert.view appState model.updating ]
        , overlays = []
        }
        (listingConfig appState model)
        model.persistentCommands


listingConfig : AppState -> Model -> Listing.ViewConfig PersistentCommand Msg
listingConfig appState model =
    let
        retryFailedButton =
            Button.secondary (gettext "Retry failed" appState.locale) RetryFailed
                |> Button.withIcon (Icon.solid "fa-rotate")
                |> Button.withLoading (ActionResult.isLoading model.updating)
    in
    { title = gettext "Persistent Commands" appState.locale
    , description = Nothing
    , breadcrumbs = Nothing
    , toolbarActions = [ retryFailedButton ]
    , searchPlaceholder = gettext "Search commands..." appState.locale
    , sortOptions = [ ( "createdAt", gettext "Created" appState.locale ) ]
    , filters =
        [ Listing.MultiFilter persistentCommandIndexRouteStateFilterId
            { name = gettext "State" appState.locale
            , options =
                [ ( "NewPersistentCommandState", gettext "New" appState.locale )
                , ( "DonePersistentCommandState", gettext "Done" appState.locale )
                , ( "ErrorPersistentCommandState", gettext "Error" appState.locale )
                , ( "IgnorePersistentCommandState", gettext "Ignore" appState.locale )
                ]
            , maxVisibleValues = 2
            }
        ]
    , item = listingItem appState
    , emptyTitle = gettext "There are no persistent commands" appState.locale
    , emptyDescription = ""
    , wrapMsg = ListingMsg
    }


listingItem : AppState -> PersistentCommand -> Listing.Item Msg
listingItem appState persistentCommand =
    let
        name =
            PersistentCommand.visibleName persistentCommand

        base =
            Listing.item
                { id = Uuid.toString persistentCommand.uuid
                , title =
                    Link.link { label = name, url = Routes.toUrl appState (Routes.persistentCommandsDetail persistentCommand.uuid) }
                        |> Link.toHtml
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ name ]
                }

        tenantFragment =
            Link.link
                { label = persistentCommand.tenant.name
                , url = Routes.toUrl appState (Routes.tenantsDetail persistentCommand.tenant.uuid)
                }
                |> Link.toHtml

        createdByFragment =
            case persistentCommand.createdBy of
                Just createdBy ->
                    [ Layout.inline
                        [ UserAvatar.user
                            { firstName = createdBy.firstName
                            , lastName = createdBy.lastName
                            , imageUrl = createdBy.imageUrl
                            , gravatarHash = Just createdBy.gravatarHash
                            }
                            |> ItemIcon.withSize ItemIcon.Small
                            |> ItemIcon.toHtml
                        , Text.muted (createdBy.firstName ++ " " ++ createdBy.lastName)
                        ]
                    ]

                Nothing ->
                    []

        attempts =
            Text.muted
                (String.format (gettext "Attempts: %s/%s" appState.locale)
                    [ String.fromInt persistentCommand.attempts, String.fromInt persistentCommand.maxAttempts ]
                )
    in
    { base
        | icon =
            Just
                (ItemIcon.toHtml
                    (UserAvatar.tenant { name = persistentCommand.tenant.name, logoUrl = persistentCommand.tenant.logoUrl })
                )
        , badges = [ PersistentCommandBadge.badge appState persistentCommand.state ]
        , fragments = tenantFragment :: createdByFragment ++ [ attempts ]
        , timestamp = Just (Timestamp.updated appState persistentCommand.updatedAt)
        , actions =
            PersistentCommandActions.actions appState
                { retryMsg = RerunCommand
                , setIgnoredMsg = SetIgnored
                , viewActionVisible = True
                }
                persistentCommand
    }
