module Common.Pages.Tenants.Index exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Tenant exposing (Tenant)
import Common.Api.Models.TenantState as TenantState exposing (TenantState)
import Common.Api.Tenants as TenantsApi
import Common.Components.Listing as Listing
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Tenants.Routes exposing (indexRouteEnabledFilterId, indexRouteStatesFilterId)
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Uuid


type alias Model =
    { tenants : Listing.Model Tenant
    }


type Msg
    = ListingMsg (Listing.Msg Tenant)


initialModel : PaginationQueryString -> Maybe String -> Maybe String -> Model
initialModel paginationQueryString mbEnabled mbStates =
    { tenants = Listing.initialModelWithFilters paginationQueryString (initialFilters mbEnabled mbStates) }


initialFilters : Maybe String -> Maybe String -> PaginationQueryFilters
initialFilters mbEnabled mbStates =
    PaginationQueryFilters.fromValues
        [ ( indexRouteEnabledFilterId, mbEnabled )
        , ( indexRouteStatesFilterId, mbStates )
        ]


init : PaginationQueryString -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init paginationQueryString mbEnabled mbStates =
    let
        ( tenants, cmd ) =
            Listing.init paginationQueryString (initialFilters mbEnabled mbStates)
    in
    ( { tenants = tenants }, Cmd.map ListingMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        ListingMsg listingMsg ->
            let
                ( tenants, cmd ) =
                    Listing.update appState (listingUpdateConfig cfg appState) listingMsg model.tenants
            in
            ( { model | tenants = tenants }, cmd )


listingUpdateConfig : UpdateConfig Msg msg -> AppState -> Listing.UpdateConfig Tenant msg
listingUpdateConfig cfg appState =
    { getRequest = TenantsApi.getTenants appState
    , getError = gettext "Unable to get tenants." appState.locale
    , wrapMsg = cfg.wrapMsg << ListingMsg
    , logoutMsg = cfg.logoutMsg
    , toUrl = \filters pqs -> Routes.toUrl appState (Routes.tenantsIndexWithFilters filters pqs)
    , key = appState.key
    , validateFilters = Nothing
    , searchFilters = []
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg (Listing.subscriptions model.tenants)


view : AppState -> Model -> Html Msg
view appState model =
    let
        createButton =
            Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState Routes.tenantsCreate)
                |> Button.withIcon (Icon.solid "fa-plus")
    in
    Listing.viewWith appState
        { notices = [], overlays = [] }
        { title = gettext "Tenants" appState.locale
        , description = Nothing
        , breadcrumbs = Nothing
        , toolbarActions = [ createButton ]
        , searchPlaceholder = gettext "Search tenants..." appState.locale
        , sortOptions =
            [ ( "name", gettext "Name" appState.locale )
            , ( "tenantId", gettext "Tenant ID" appState.locale )
            , ( "createdAt", gettext "Created at" appState.locale )
            , ( "updatedAt", gettext "Updated at" appState.locale )
            ]
        , filters =
            [ Listing.SingleFilter indexRouteEnabledFilterId
                { name = gettext "Enabled" appState.locale
                , options =
                    [ ( "true", gettext "Enabled only" appState.locale )
                    , ( "false", gettext "Disabled only" appState.locale )
                    ]
                }
            , Listing.MultiFilter indexRouteStatesFilterId
                { name = gettext "State" appState.locale
                , options = stateFilterOptions appState
                , maxVisibleValues = 1
                }
            ]
        , item = listingItem appState
        , emptyTitle = gettext "No tenants" appState.locale
        , emptyDescription = gettext "Click \"Create\" button to add a new tenant." appState.locale
        , wrapMsg = ListingMsg
        }
        model.tenants


{-| The values are the API's state names, as `TenantState.filterOptions` has
them; the labels are translated here, because that module's are not.
-}
stateFilterOptions : AppState -> List ( String, String )
stateFilterOptions appState =
    [ ( "NotSeededTenantState", stateLabel appState TenantState.NotSeeded )
    , ( "PendingHousekeepingTenantState", stateLabel appState TenantState.PendingHousekeeping )
    , ( "HousekeepingInProgressTenantState", stateLabel appState TenantState.HousekeepingInProgress )
    , ( "ReadyForUseTenantState", stateLabel appState TenantState.ReadyForUse )
    ]


stateLabel : AppState -> TenantState -> String
stateLabel appState state =
    case state of
        TenantState.NotSeeded ->
            gettext "Not seeded" appState.locale

        TenantState.PendingHousekeeping ->
            gettext "Pending housekeeping" appState.locale

        TenantState.HousekeepingInProgress ->
            gettext "Housekeeping in progress" appState.locale

        TenantState.ReadyForUse ->
            gettext "Ready for use" appState.locale


listingItem : AppState -> Tenant -> Listing.Item Msg
listingItem appState tenant =
    let
        base =
            Listing.item
                { id = Uuid.toString tenant.uuid
                , title =
                    Link.link { label = tenant.name, url = Routes.toUrl appState (Routes.tenantsDetail tenant.uuid) }
                        |> Link.toHtml
                , actionsLabel = String.format (gettext "Actions for %s" appState.locale) [ tenant.name ]
                }

        badges =
            if tenant.enabled then
                []

            else
                [ Badge.danger (gettext "Disabled" appState.locale) ]

        urlFragment =
            Link.link { label = String.replace "https://" "" tenant.clientUrl, url = tenant.clientUrl }
                |> Link.external
                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                |> Link.toHtml

        stateFragment =
            if tenant.state /= TenantState.ReadyForUse then
                [ Badge.toHtml (Badge.neutral (stateLabel appState tenant.state)) ]

            else
                []
    in
    { base
        | icon = Just (ItemIcon.toHtml (UserAvatar.tenant { name = tenant.name, logoUrl = tenant.logoUrl }))
        , badges = badges
        , fragments = urlFragment :: stateFragment
        , timestamp = Just (Timestamp.updated appState tenant.updatedAt)
    }
