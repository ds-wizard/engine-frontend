module Common.Components.Listing exposing
    ( Model, Msg, initialModel, initialModelWithFilters, init
    , UpdateConfig, SearchFilterSource, Suggestion, update, subscriptions
    , onAfterDelete, reload, reloadBackground, setFilters, closeMenus, updateItems
    , itemActionsTriggerId
    , items, pagination, paginationQueryString, filters, searchFilterAvailable
    , ViewConfig, Filter(..), Item, item, view, viewWith, viewWithinPageWith
    , TableViewConfig, viewTable, viewTableWith
    )

{-| A listing page: the search, the filters, the sort, the items and the pages —
and the state behind all of them.

The arrangement is `Ui.Page.List`'s; what this adds is everything a template cannot
know: the query string, the fetch, the debounced search, the filters that
survive a reload, and where focus goes when the thing it was on disappears.


# State

The listing is the source of truth for its own state. Every change — a search,
a filter, a sort, a page — updates the model, replaces the URL
(`Navigation.replaceUrl`, so Back does not walk through every keystroke) and
fetches. The application recognizes that URL change as the same listing
(`isSameListingRoute`) and does not re-initialize the page, which is why the
model cannot be rebuilt from the URL on each change.

Responses are tagged with the query string and filters they were asked for, and
one that no longer matches is dropped, so a slow response to an old search never
replaces a newer one.

A typed search is applied once typing has paused, and then only from the page:
the pause ends in a pending query, which `subscriptions` applies on the next
frame. A page that has been left is no longer subscribed to, so a search still
waiting when someone moved on never replaces the URL of where they went.

@docs Model, Msg, initialModel, initialModelWithFilters, init


# Update

@docs UpdateConfig, SearchFilterSource, Suggestion, update, subscriptions


## Messages a page sends

@docs onAfterDelete, reload, reloadBackground, setFilters, closeMenus, updateItems
@docs itemActionsTriggerId


## Reading the state

@docs items, pagination, paginationQueryString, filters, searchFilterAvailable


# View

@docs ViewConfig, Filter, Item, item, view, viewWith, viewWithinPageWith


## As a table

@docs TableViewConfig, viewTable, viewTableWith


# Accessibility

  - **Announced results.** The count ("Showing 1–20 of 134", "No results") is in
    `Ui.Page.List`'s polite status region. It is emptied while a request is out
    and filled when the answer arrives, so each answer is a change and is
    announced once — including one that happens to say the same as the last.
    The search box keeps focus while typing; the status is what tells a screen
    reader user the search did something.
  - **Focus is owned.** A page change, clearing the search and filters, and a
    delete all remove the control that was focused (the pagination and the rows
    are replaced by the loading state). Each of them moves focus to the page's
    `h1` (`Ui.AppShell.mainHeadingId`), so the keyboard is never left on
    nothing and the next Tab starts from the top of the results.
  - **Menus are one tab stop each** (`Ui.Menu`). A single-choice filter and the
    sort close after a choice and return focus to their trigger; a multi-choice
    or search filter stays open, because choosing several is the point.
  - **Filters say their value in words and fill** (`Ui.FilterMenu`), never by
    color alone.
  - **Errors** are a `Ui.Alert.danger`, which is an assertive `alert`.
  - **Empty is said twice, and differently.** A listing with nothing in it yet
    and one whose search or filters matched nothing have their own titles and
    illustrations, and only the second offers to clear the filters.


# Search filters

A filter whose values come from a server search: the projects listing's users,
user groups, project tags and knowledge models. The listing owns everything that
is the same about them — the text typed into each filter's search box, the
debounced request (500 ms), the suggestions and their loading and error state,
the AND/OR operator in the query string — and the page supplies only what
differs, in `UpdateConfig.searchFilters`:

  - `search` — the request, given the typed query and the ids already selected
    (so the page can exclude them server-side), answering with
    `{ id, label }` suggestions the page maps from its own API type.
  - `resolve` — how to find labels for ids that are selected when the page
    loads (they arrive from the URL as bare ids). `Nothing` means an id is its
    own label, which is the case for project tags.
  - `getError` — the message for a failed search.

Labels are then kept per filter for the life of the listing: those resolved at
load and those of every suggestion chosen since. So choosing a user shows their
name on the button at once, with no second request, and an id whose label could
not be resolved is shown as the id rather than hidden — a selection that is in
force must always be visible and removable.

The view half is `SearchFilter` in `ViewConfig.filters`, with the same id.

-}

import ActionResult exposing (ActionResult)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.ItemActions as ItemActions
import Common.Components.Listing.Internal as Internal
import Common.Components.UiWords as UiWords
import Common.Data.Context exposing (WithLocale)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator(..))
import Common.Data.PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Common.Utils.RequestHelpers as RequestHelpers
import Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import List.Extra as List
import Maybe.Extra as Maybe
import Shortcut
import String.Format as String
import Task
import Task.Extra as TaskExtra
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Badge as Badge exposing (Badge)
import Ui.Button as Button exposing (Button)
import Ui.ButtonGroup as ButtonGroup
import Ui.EmptyState as EmptyState
import Ui.Field as Field
import Ui.FilterMenu as FilterMenu
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.ListItem as ListItem
import Ui.Menu as Menu
import Ui.Page.List as PageList
import Ui.Page.Loading as PageLoading
import Ui.Pagination as Pagination
import Ui.Table as Table
import Ui.Toolbar as Toolbar



-- MODEL


type Model a
    = Model (State a)


type alias State a =
    { pagination : ActionResult (Pagination a)
    , paginationQueryString : PaginationQueryString
    , filters : PaginationQueryFilters
    , queryInput : String
    , queryDebouncer : Debouncer (Msg a)
    , pendingQuery : Maybe String
    , searchDebouncer : Debouncer (Msg a)
    , searches : Dict String SearchState
    , menus : Dict String Menu.Model
    }


type alias SearchState =
    { query : String
    , suggestions : ActionResult (List Suggestion)
    , labels : Dict String String
    , anyExist : Maybe Bool
    }


{-| What a search filter offers: the value that goes into the query string, and
the words shown for it.
-}
type alias Suggestion =
    { id : String
    , label : String
    }


initialModel : PaginationQueryString -> Model a
initialModel pqs =
    initialModelWithFilters pqs PaginationQueryFilters.empty


initialModelWithFilters : PaginationQueryString -> PaginationQueryFilters -> Model a
initialModelWithFilters pqs initialFilters =
    Model
        { pagination = ActionResult.Loading
        , paginationQueryString = pqs
        , filters = initialFilters
        , queryInput = Maybe.withDefault "" pqs.q
        , queryDebouncer = Debouncer.toDebouncer (Debouncer.debounce 500)
        , pendingQuery = Nothing
        , searchDebouncer = Debouncer.toDebouncer (Debouncer.debounce 500)
        , searches = Dict.empty
        , menus = Dict.empty
        }


{-| The model and the first load: the items, and for each search filter its
initial suggestions and the labels of whatever the URL already selects.
-}
init : PaginationQueryString -> PaginationQueryFilters -> ( Model a, Cmd (Msg a) )
init pqs initialFilters =
    ( initialModelWithFilters pqs initialFilters, TaskExtra.dispatch Initialize )


emptySearch : SearchState
emptySearch =
    { query = ""
    , suggestions = ActionResult.Loading
    , labels = Dict.empty
    , anyExist = Nothing
    }



-- READING


{-| The items currently shown; empty while loading.
-}
items : Model a -> List a
items (Model state) =
    ActionResult.unwrap [] .items state.pagination


pagination : Model a -> ActionResult (Pagination a)
pagination (Model state) =
    state.pagination


paginationQueryString : Model a -> PaginationQueryString
paginationQueryString (Model state) =
    state.paginationQueryString


filters : Model a -> PaginationQueryFilters
filters (Model state) =
    state.filters


{-| Whether a search filter has anything to offer: it is in use, or its first,
empty search found something. The projects listing hides the tags filter on an
instance where nobody has tagged anything.
-}
searchFilterAvailable : String -> Model a -> Bool
searchFilterAvailable filterId (Model state) =
    PaginationQueryFilters.isFilterActive filterId state.filters
        || (Dict.get filterId state.searches
                |> Maybe.andThen .anyExist
                |> Maybe.withDefault False
           )


{-| Changes the items in place without a fetch — a document whose state was
pushed over a websocket, say.
-}
updateItems : (a -> a) -> Model a -> Model a
updateItems fn (Model state) =
    Model { state | pagination = ActionResult.map (\p -> { p | items = List.map fn p.items }) state.pagination }



-- UPDATE


type Msg a
    = Initialize
    | Reload
    | ReloadBackground
    | GetItemsComplete Bool PaginationQueryString PaginationQueryFilters (Result ApiError (Pagination a))
    | PageSelected Int
    | SortSelected String
    | SortDirectionToggled
    | FiltersChanged (Maybe String) PaginationQueryFilters
    | Cleared
    | QueryInput String
    | QueryApply String
    | PendingQueryApplied
    | QueryDebouncerMsg (Debouncer.Msg (Msg a))
    | SearchInput String String
    | SearchApply String String
    | SearchDebouncerMsg (Debouncer.Msg (Msg a))
    | SearchComplete String String (Result ApiError (List Suggestion))
    | ResolveComplete String (Result ApiError (List Suggestion))
    | SearchToggled String Suggestion
    | MenuMsg String Int Menu.Msg
    | OnAfterDelete
    | MenusClosed
    | FocusSearch
    | Noop


{-| Send after deleting an item. Steps back a page if that emptied the last
one, reloads, and moves focus to the heading — the row that had it is gone.
-}
onAfterDelete : Msg a
onAfterDelete =
    OnAfterDelete


{-| Closes every menu the listing has open — a row's actions, a filter, the
sort — and leaves focus where it is.

Send it with whatever a row action does when that action opens a dialog
("Delete", "Clone"): the dialog takes focus, and a menu left open under it is
still open when the dialog closes. It does not move focus, for the reason
`Ui.Menu.closeWithoutFocus` gives; once the dialog closes, put focus back on the
row's trigger or, if the row is gone, let `onAfterDelete` move it to the heading.

-}
closeMenus : Msg a
closeMenus =
    MenusClosed


{-| The id of the button that opens a row's actions menu, for the row with the
given id (the `id` of its `Item`, or of `rowActions` in a table).

Focus it (`Browser.Dom.focus`) when a dialog one of the row's actions opened is
closed without removing the row, so focus returns to where the user left it.

-}
itemActionsTriggerId : String -> String
itemActionsTriggerId itemId =
    Menu.triggerId (itemMenuPrefix ++ itemId)


{-| Fetch again, showing the loading state.
-}
reload : Msg a
reload =
    Reload


{-| Fetch again without the loading state and without closing open menus — for
polling a list whose items change on their own.
-}
reloadBackground : Msg a
reloadBackground =
    ReloadBackground


{-| Replace the filters, for a page's own `CustomFilter` items. Resets the page
and runs `validateFilters` like any other filter change. The filter's menu stays
open.
-}
setFilters : PaginationQueryFilters -> Msg a
setFilters =
    FiltersChanged Nothing


{-| `getRequest` is shaped like the API layer's list functions, so it is usually
one partially applied: `ProjectsApi.getList appState`.

`searchFilters` pairs a `SearchFilter`'s id with where its suggestions come
from; leave it empty when there are none.

-}
type alias UpdateConfig a msg =
    { getRequest : PaginationQueryFilters -> PaginationQueryString -> (Result ApiError (Pagination a) -> msg) -> Cmd msg
    , getError : String
    , wrapMsg : Msg a -> msg
    , logoutMsg : msg
    , toUrl : PaginationQueryFilters -> PaginationQueryString -> String
    , key : Navigation.Key
    , validateFilters : Maybe (PaginationQueryFilters -> PaginationQueryFilters)
    , searchFilters : List ( String, SearchFilterSource msg )
    }


{-| Where one search filter's suggestions come from. See "Search filters" above.
-}
type alias SearchFilterSource msg =
    { search : { query : String, selected : List String } -> (Result ApiError (List Suggestion) -> msg) -> Cmd msg
    , resolve : Maybe (List String -> (Result ApiError (List Suggestion) -> msg) -> Cmd msg)
    , getError : String
    }


update : WithLocale b -> UpdateConfig a msg -> Msg a -> Model a -> ( Model a, Cmd msg )
update appState cfg msg ((Model state) as model) =
    case msg of
        Initialize ->
            let
                ( searches, searchCmds ) =
                    List.foldl
                        (\( filterId, source ) ( acc, cmds ) ->
                            ( Dict.insert filterId (Maybe.withDefault emptySearch (Dict.get filterId state.searches)) acc
                            , searchCmd cfg filterId source "" state.filters
                                :: resolveCmd cfg filterId source state.filters
                                :: cmds
                            )
                        )
                        ( state.searches, [] )
                        cfg.searchFilters
            in
            ( Model { state | searches = searches, pagination = ActionResult.Loading }
            , Cmd.batch (fetch cfg False state.paginationQueryString state.filters :: searchCmds)
            )

        Reload ->
            ( Model { state | pagination = ActionResult.Loading }
            , fetch cfg False state.paginationQueryString state.filters
            )

        ReloadBackground ->
            ( model
            , fetch cfg True state.paginationQueryString state.filters
            )

        GetItemsComplete background pqs requestFilters result ->
            if Internal.isCurrent state pqs requestFilters then
                case result of
                    Ok data ->
                        ( Model
                            { state
                                | pagination = ActionResult.Success data
                                , menus =
                                    if background then
                                        state.menus

                                    else
                                        Dict.filter (\menuId _ -> not (String.startsWith itemMenuPrefix menuId)) state.menus
                            }
                        , Cmd.none
                        )

                    Err error ->
                        ( Model { state | pagination = ApiError.toActionResult appState cfg.getError error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

            else
                -- A newer request is out. Still log out on a 401: the session
                -- is gone whichever request noticed.
                ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        PageSelected page ->
            let
                pqs =
                    state.paginationQueryString
            in
            navigate cfg state.filters { pqs | page = Just page } model
                |> withFocusOnHeading cfg

        SortSelected sortBy ->
            navigate cfg state.filters (Internal.setSortBy sortBy state.paginationQueryString) model
                |> withMenuClosed cfg sortMenuId

        SortDirectionToggled ->
            navigate cfg state.filters (Internal.toggleDirection state.paginationQueryString) model

        FiltersChanged closeMenuId newFilters ->
            let
                ( validFilters, pqs ) =
                    Internal.applyFilters cfg.validateFilters newFilters state.paginationQueryString

                result =
                    navigate cfg validFilters pqs model
            in
            case closeMenuId of
                Just menuId ->
                    withMenuClosed cfg menuId result

                Nothing ->
                    result

        Cleared ->
            let
                ( validFilters, pqs ) =
                    Internal.clearAll cfg.validateFilters state.paginationQueryString

                ( Model navigated, cmd ) =
                    navigate cfg validFilters pqs model
            in
            ( Model { navigated | queryInput = "", pendingQuery = Nothing }, cmd )
                |> withFocusOnHeading cfg

        QueryInput value ->
            update appState
                cfg
                (QueryDebouncerMsg (Debouncer.provideInput (QueryApply value)))
                (Model { state | queryInput = value })

        QueryApply value ->
            ( Model { state | pendingQuery = Just value }, Cmd.none )

        PendingQueryApplied ->
            case state.pendingQuery of
                Just value ->
                    navigate cfg state.filters (Internal.applyQuery value state.paginationQueryString) (Model { state | pendingQuery = Nothing })

                Nothing ->
                    ( model, Cmd.none )

        QueryDebouncerMsg debouncerMsg ->
            Debouncer.update (update appState cfg)
                { mapMsg = cfg.wrapMsg << QueryDebouncerMsg
                , getDebouncer = \(Model s) -> s.queryDebouncer
                , setDebouncer = \debouncer (Model s) -> Model { s | queryDebouncer = debouncer }
                }
                debouncerMsg
                model

        SearchInput filterId value ->
            update appState
                cfg
                (SearchDebouncerMsg (Debouncer.provideInput (SearchApply filterId value)))
                (Model { state | searches = updateSearch filterId (\s -> { s | query = value }) state.searches })

        SearchApply filterId value ->
            case List.find (Tuple.first >> (==) filterId) cfg.searchFilters of
                Just ( _, source ) ->
                    -- The previous suggestions stay until the answer arrives,
                    -- so the menu does not collapse and re-grow under the
                    -- pointer on every pause in typing.
                    ( model, searchCmd cfg filterId source value state.filters )

                Nothing ->
                    ( model, Cmd.none )

        SearchDebouncerMsg debouncerMsg ->
            Debouncer.update (update appState cfg)
                { mapMsg = cfg.wrapMsg << SearchDebouncerMsg
                , getDebouncer = \(Model s) -> s.searchDebouncer
                , setDebouncer = \debouncer (Model s) -> Model { s | searchDebouncer = debouncer }
                }
                debouncerMsg
                model

        SearchComplete filterId query result ->
            let
                current =
                    Maybe.withDefault emptySearch (Dict.get filterId state.searches)
            in
            if current.query == query then
                let
                    newSearch =
                        case result of
                            Ok suggestions ->
                                { current
                                    | suggestions = ActionResult.Success suggestions
                                    , anyExist =
                                        if String.isEmpty query then
                                            Just (not (List.isEmpty suggestions))

                                        else
                                            current.anyExist
                                }

                            Err error ->
                                let
                                    getError =
                                        List.find (Tuple.first >> (==) filterId) cfg.searchFilters
                                            |> Maybe.map (Tuple.second >> .getError)
                                            |> Maybe.withDefault ""
                                in
                                { current | suggestions = ApiError.toActionResult appState getError error }
                in
                ( Model { state | searches = Dict.insert filterId newSearch state.searches }
                , RequestHelpers.getResultCmd cfg.logoutMsg result
                )

            else
                ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        ResolveComplete filterId result ->
            case result of
                Ok suggestions ->
                    ( Model { state | searches = updateSearch filterId (rememberLabels suggestions) state.searches }
                    , Cmd.none
                    )

                Err _ ->
                    -- The ids stay selected and are shown as themselves.
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

        SearchToggled filterId suggestion ->
            let
                withLabel =
                    Model { state | searches = updateSearch filterId (rememberLabels [ suggestion ]) state.searches }
            in
            update appState
                cfg
                (FiltersChanged Nothing (Internal.toggleValue filterId suggestion.id state.filters))
                withLabel

        MenuMsg menuId count menuMsg ->
            let
                ( menu, menuCmd ) =
                    Menu.update menuId count menuMsg (getMenu menuId state.menus)
            in
            ( Model { state | menus = Dict.insert menuId menu state.menus }
            , Cmd.map (cfg.wrapMsg << MenuMsg menuId count) menuCmd
            )

        OnAfterDelete ->
            navigate cfg state.filters (Internal.pageAfterDelete state.pagination state.paginationQueryString) model
                |> withFocusOnHeading cfg

        FocusSearch ->
            -- Browser.Dom, which takes the id: the port beside it takes a CSS
            -- selector, and an id given to it matches an element named after it
            -- rather than the one carrying it.
            ( model, Task.attempt (always (cfg.wrapMsg Noop)) (Browser.Dom.focus searchFieldId) )

        MenusClosed ->
            ( Model
                { state
                    | menus =
                        Dict.map
                            (\menuId menu -> Tuple.first (Menu.update menuId 0 Menu.closeWithoutFocus menu))
                            state.menus
                }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )


{-| The one path every change takes: new state, the loading state, the URL
replaced, the request sent.
-}
navigate : UpdateConfig a msg -> PaginationQueryFilters -> PaginationQueryString -> Model a -> ( Model a, Cmd msg )
navigate cfg newFilters pqs (Model state) =
    ( Model
        { state
            | pagination = ActionResult.Loading
            , paginationQueryString = pqs
            , filters = newFilters
        }
    , Cmd.batch
        [ fetch cfg False pqs newFilters
        , Navigation.replaceUrl cfg.key (cfg.toUrl newFilters pqs)
        ]
    )


fetch : UpdateConfig a msg -> Bool -> PaginationQueryString -> PaginationQueryFilters -> Cmd msg
fetch cfg background pqs requestFilters =
    cfg.getRequest requestFilters pqs (cfg.wrapMsg << GetItemsComplete background pqs requestFilters)


withFocusOnHeading : UpdateConfig a msg -> ( Model a, Cmd msg ) -> ( Model a, Cmd msg )
withFocusOnHeading cfg ( model, cmd ) =
    ( model, Cmd.batch [ cmd, AppShell.focusOnRouteChange (cfg.wrapMsg Noop) ] )


withMenuClosed : UpdateConfig a msg -> String -> ( Model a, Cmd msg ) -> ( Model a, Cmd msg )
withMenuClosed cfg menuId ( Model state, cmd ) =
    let
        ( menu, menuCmd ) =
            Menu.update menuId 0 Menu.close (getMenu menuId state.menus)
    in
    ( Model { state | menus = Dict.insert menuId menu state.menus }
    , Cmd.batch [ cmd, Cmd.map (cfg.wrapMsg << MenuMsg menuId 0) menuCmd ]
    )


searchCmd : UpdateConfig a msg -> String -> SearchFilterSource msg -> String -> PaginationQueryFilters -> Cmd msg
searchCmd cfg filterId source query currentFilters =
    source.search
        { query = query, selected = Internal.values filterId currentFilters }
        (cfg.wrapMsg << SearchComplete filterId query)


resolveCmd : UpdateConfig a msg -> String -> SearchFilterSource msg -> PaginationQueryFilters -> Cmd msg
resolveCmd cfg filterId source currentFilters =
    case ( source.resolve, Internal.values filterId currentFilters ) of
        ( Just resolve, (_ :: _) as ids ) ->
            resolve ids (cfg.wrapMsg << ResolveComplete filterId)

        _ ->
            Cmd.none


updateSearch : String -> (SearchState -> SearchState) -> Dict String SearchState -> Dict String SearchState
updateSearch filterId fn searches =
    Dict.insert filterId (fn (Maybe.withDefault emptySearch (Dict.get filterId searches))) searches


rememberLabels : List Suggestion -> SearchState -> SearchState
rememberLabels suggestions search =
    { search | labels = List.foldl (\s -> Dict.insert s.id s.label) search.labels suggestions }


getMenu : String -> Dict String Menu.Model -> Menu.Model
getMenu menuId menus =
    Maybe.withDefault Menu.init (Dict.get menuId menus)



-- SUBSCRIPTIONS


{-| The open menus' Escape and outside-click handling.
-}
subscriptions : Model a -> Sub (Msg a)
subscriptions (Model state) =
    let
        pendingQuery =
            case state.pendingQuery of
                Just _ ->
                    Browser.Events.onAnimationFrame (always PendingQueryApplied)

                Nothing ->
                    Sub.none
    in
    state.menus
        |> Dict.toList
        |> List.map (\( menuId, menu ) -> Sub.map (MenuMsg menuId 0) (Menu.subscriptions menuId menu))
        |> (::) pendingQuery
        |> Sub.batch



-- VIEW


{-| `toolbarActions` are the page's own actions ("Create", "Import"), shown in
the header before the toolbar: the others first as secondary buttons, the main
one last as the primary, each with its icon. The primary one is also the one
thing to do about an empty listing, offered in its empty state — only when it is
empty because nothing exists yet, not when a search or filter emptied it. A list
without a primary action offers nothing there.
-}
type alias ViewConfig a msg =
    { title : String
    , description : Maybe String
    , breadcrumbs : Maybe (Html msg)
    , toolbarActions : List (Button msg)
    , searchPlaceholder : String
    , sortOptions : List ( String, String )
    , filters : List (Filter msg)
    , item : a -> Item msg
    , emptyTitle : String
    , emptyDescription : String
    , wrapMsg : Msg a -> msg
    }


{-| The kinds of filter. The first argument of each is the filter's id — its key
in `PaginationQueryFilters` and therefore in the URL.

  - `SingleFilter` — one value of a fixed set, as radio items. Closes on a
    choice.
  - `SingleNonEmptyFilter` — the same; kept for the pages that name it.
  - `MultiFilter` — any number of a fixed set, as check items, stored joined
    with ",". The button names up to `maxVisibleValues` of them and counts the
    rest. Stays open.
  - `SearchFilter` — any number of values found by a server search, with an
    optional AND/OR choice (`operator`) once more than one is chosen. Its source
    is in `UpdateConfig.searchFilters` under the same id. `noResults` is what
    the menu says when a search finds nothing ("No users found"). Stays open.

Every filter but `CustomFilter` ends with "Clear filter" once it is on; a
custom filter's page adds its own.

  - `CustomFilter` — anything else; the page supplies the button's value, its
    active state and the menu items, and changes filters with `setFilters`.

-}
type Filter msg
    = SingleFilter String { name : String, options : List ( String, String ) }
    | SingleNonEmptyFilter String { name : String, options : List ( String, String ) }
    | MultiFilter String { name : String, options : List ( String, String ), maxVisibleValues : Int }
    | SearchFilter String { name : String, searchPlaceholder : String, noResults : String, operator : Bool }
    | CustomFilter String { name : String, value : Maybe String, active : Bool, items : List (Menu.Item msg) }


{-| One row.

`title` is usually a link to the item, and the row itself is not one (see
`Ui.ListItem`). `badges` sit beside the title. `actionsLabel` names the kebab
menu and should name the item — "Actions for Horizon Europe DMP". `timestamp`
comes from `Common.Components.Timestamp`. `id` has to be unique on the page:
the row's menu id is made from it. `description` is the item's own sentence, the
last thing on the fragments' line and cut with an ellipsis, so a row stays two
lines (`Ui.ListItem.withDescription`); empty means none. `detail` is a
full-width area under the row for what cannot be cut short — a state that needs
a sentence — through `Ui.ListItem.withDetail`.

-}
type alias Item msg =
    { id : String
    , title : Html msg
    , icon : Maybe (Html msg)
    , badges : List (Badge msg)
    , fragments : List (Html msg)
    , description : String
    , timestamp : Maybe { relative : String, absolute : String, machine : String }
    , actions : List (ItemActions.Action msg)
    , actionsLabel : String

    -- The actions behind a button that shows what they set — a member's role —
    -- instead of the kebab: what it shows, and its accessible name, which
    -- starts with that (`ItemActions.labeledMenu`).
    , actionsTrigger : Maybe { content : List (Html msg), label : String }
    , highlight : Bool
    , detail : Maybe (Html msg)
    }


{-| A row with only its id, title and actions label, to fill the rest into.
-}
item : { id : String, title : Html msg, actionsLabel : String } -> Item msg
item config =
    { id = config.id
    , title = config.title
    , icon = Nothing
    , badges = []
    , fragments = []
    , description = ""
    , timestamp = Nothing
    , actions = []
    , actionsLabel = config.actionsLabel
    , actionsTrigger = Nothing
    , highlight = False
    , detail = Nothing
    }


view : WithLocale b -> ViewConfig a msg -> Model a -> Html msg
view appState cfg model =
    viewPage appState False cfg [] (viewList cfg model) model


{-| `view`, with the two things a listing page often has around it.

  - `notices` are shown between the toolbar and the items — the result of
    something the page did ("The migration was canceled"), a warning about the
    whole list. Usually `Ui.Alert`s. They sit inside the page, after the search
    and the filters, so the reading order is title, controls, what is worth
    knowing, then the rows; and they stay while the list loads, since they are
    not about one answer.
  - `overlays` are rendered after the page — the page's dialogs. They are
    siblings of the page rather than inside it, so a closed dialog takes no room
    in the scrolling region, and the element around both has no class of its
    own, so the page still fills the main region.

The wrapper is there whether or not `overlays` is empty, so a dialog appearing
in the list does not remount the page under it.

-}
viewWith :
    WithLocale b
    -> { notices : List (Html msg), overlays : List (Html msg) }
    -> ViewConfig a msg
    -> Model a
    -> Html msg
viewWith appState extras cfg model =
    Html.div [] (viewPage appState False cfg extras.notices (viewList cfg model) model :: extras.overlays)


{-| `viewWith`, for a listing that is a region of a page that already has a
heading — the documents tab of a project.

The difference is `Ui.Page.List.withinPage`: no scroll container of its own, and
the title becomes a visually hidden `h2` instead of the page's `h1`, because the
tab that was pressed already names the listing on screen. Everything else,
including where focus goes after a delete (the containing page's `h1`), is
`viewWith`'s.

-}
viewWithinPageWith :
    WithLocale b
    -> { notices : List (Html msg), overlays : List (Html msg) }
    -> ViewConfig a msg
    -> Model a
    -> Html msg
viewWithinPageWith appState extras cfg model =
    Html.div [] (viewPage appState True cfg extras.notices (viewList cfg model) model :: extras.overlays)


{-| What `ViewConfig` and `TableViewConfig` have in common: everything but the
body.
-}
type alias Chrome c a msg =
    { c
        | title : String
        , description : Maybe String
        , breadcrumbs : Maybe (Html msg)
        , toolbarActions : List (Button msg)
        , searchPlaceholder : String
        , sortOptions : List ( String, String )
        , filters : List (Filter msg)
        , emptyTitle : String
        , emptyDescription : String
        , wrapMsg : Msg a -> msg
    }


{-| A listing whose items are rows of a `Ui.Table` rather than list items — for
the pages whose items are records compared across a row: documents, users.

Everything but the body is `view`'s: the header, the search, the filters and the
sort in the toolbar, the pagination, both empty states, the status region and
where focus goes. The table's header row is `columns`, and the table is named by
`caption` when there is one — visually and to a screen reader's table list —
and otherwise by the page's `h1` above it. `rowId` identifies a row — its
uuid — for its test hook, `data-cy="listing-item_<rowId>"`, as an `Item`'s `id`
does in a list.

`rowActions` adds a last column, "Actions", aligned to the end of the row, with
each row's actions as buttons (`ItemActions.buttons` in `Ui.Table.actions`) — a
table always shows its actions, never a menu. `label` names the row's subject
("Horizon Europe DMP"), which each button's accessible name ends with. `id`
identifies the row. A row whose actions were all filtered out gets an empty
cell.

    Listing.viewTable appState
        { title = gettext "Documents" appState.locale
        , description = Nothing
        , breadcrumbs = Nothing
        , toolbarActions = []
        , searchPlaceholder = gettext "Search documents..." appState.locale
        , sortOptions = [ ( "name", gettext "Name" appState.locale ) ]
        , filters = []
        , caption = Nothing
        , columns =
            [ Table.column (gettext "Name" appState.locale) (\document -> text document.name)
            , Table.column (gettext "Format" appState.locale) (\document -> text document.format)
            ]
        , rowId = \document -> Uuid.toString document.uuid
        , rowActions = Just (\document -> { id = document.uuid, label = …, actions = … })
        , emptyTitle = gettext "No documents" appState.locale
        , emptyDescription = …
        , wrapMsg = ListingMsg
        }
        model.documents

-}
type alias TableViewConfig a msg =
    { title : String
    , description : Maybe String
    , breadcrumbs : Maybe (Html msg)
    , toolbarActions : List (Button msg)
    , searchPlaceholder : String
    , sortOptions : List ( String, String )
    , filters : List (Filter msg)
    , caption : Maybe String
    , columns : List (Table.Column a msg)
    , rowId : a -> String
    , rowActions : Maybe (a -> { id : String, label : String, actions : List (ItemActions.Action msg) })
    , emptyTitle : String
    , emptyDescription : String
    , wrapMsg : Msg a -> msg
    }


{-| The listing, with its items as the rows of a table. See `TableViewConfig`.
-}
viewTable : WithLocale b -> TableViewConfig a msg -> Model a -> Html msg
viewTable appState cfg model =
    viewPage appState False cfg [] (viewRows appState cfg) model


{-| `viewTable`, with notices and overlays. See `viewWith`.
-}
viewTableWith :
    WithLocale b
    -> { notices : List (Html msg), overlays : List (Html msg) }
    -> TableViewConfig a msg
    -> Model a
    -> Html msg
viewTableWith appState extras cfg model =
    Html.div [] (viewPage appState False cfg extras.notices (viewRows appState cfg) model :: extras.overlays)


viewPage : WithLocale b -> Bool -> Chrome c a msg -> List (Html msg) -> (List a -> Html msg) -> Model a -> Html msg
viewPage appState within cfg notices viewItems ((Model state) as model) =
    let
        page =
            PageList.list
                { title = cfg.title
                , toolbar = viewToolbar appState cfg model
                , content = viewSearchShortcut cfg :: notices ++ [ viewContent appState cfg viewItems model ]
                }
                |> PageList.withActions cfg.toolbarActions
                |> PageList.withStatus (status appState state)
                |> withMaybe PageList.withDescription cfg.description
                |> withMaybe PageList.withBreadcrumbs cfg.breadcrumbs
                |> (if within then
                        PageList.withinPage

                    else
                        identity
                   )
    in
    case state.pagination of
        ActionResult.Success data ->
            PageList.toHtml
                (PageList.withPagination
                    (Pagination.toHtmlWithWords (UiWords.pagination appState)
                        { current = Maybe.withDefault 1 state.paginationQueryString.page
                        , total = data.page.totalPages
                        , onSelect = cfg.wrapMsg << PageSelected
                        }
                    )
                    page
                )

        _ ->
            PageList.toHtml page


withMaybe : (x -> page -> page) -> Maybe x -> page -> page
withMaybe fn maybe page =
    case maybe of
        Just value ->
            fn value page

        Nothing ->
            page


{-| Empty while a request is out, so that the answer is always a change to the
region and is announced.
-}
status : WithLocale b -> State a -> String
status appState state =
    case state.pagination of
        ActionResult.Success data ->
            if List.isEmpty data.items then
                gettext "No results" appState.locale

            else
                let
                    shown =
                        Internal.range data.page (List.length data.items)
                in
                String.format (gettext "Showing %s–%s of %s" appState.locale)
                    [ String.fromInt shown.from, String.fromInt shown.to, String.fromInt shown.total ]

        _ ->
            ""


viewContent : WithLocale b -> Chrome c a msg -> (List a -> Html msg) -> Model a -> Html msg
viewContent appState cfg viewItems (Model state) =
    case state.pagination of
        ActionResult.Success data ->
            if List.isEmpty data.items then
                viewEmpty appState cfg state

            else
                viewItems data.items

        ActionResult.Error message ->
            Alert.toHtml (Alert.danger message)

        _ ->
            PageLoading.toHtml (gettext "Loading..." appState.locale)


viewEmpty : WithLocale b -> Chrome c a msg -> State a -> Html msg
viewEmpty appState cfg state =
    if Internal.hasQueryOrFilters state.queryInput state.filters then
        EmptyState.emptyState
            { title = gettext "No results" appState.locale
            , description = gettext "There are no results matching your search and filters." appState.locale
            }
            |> EmptyState.withIllustration (Illustration.toHtml Illustration.noResults)
            |> EmptyState.withAction
                (Button.secondary (gettext "Clear search and filters" appState.locale) (cfg.wrapMsg Cleared))
            |> EmptyState.withTestId "listing-no-results"
            |> EmptyState.toHtml

    else
        EmptyState.emptyState { title = cfg.emptyTitle, description = cfg.emptyDescription }
            |> EmptyState.withIllustration (Illustration.toHtml Illustration.empty)
            |> withMaybe EmptyState.withAction (primaryAction cfg.toolbarActions)
            |> EmptyState.withTestId "listing-empty"
            |> EmptyState.toHtml


{-| The list's main action: the last primary one, the main action coming last.
-}
primaryAction : List (Button msg) -> Maybe (Button msg)
primaryAction actions =
    List.last (List.filter Button.isPrimary actions)


viewList : ViewConfig a msg -> Model a -> List a -> Html msg
viewList cfg (Model state) data =
    ListItem.list (List.map (viewItem cfg state << cfg.item) data)


viewRows : WithLocale b -> TableViewConfig a msg -> List a -> Html msg
viewRows appState cfg rows =
    let
        actionsColumn toActions =
            Table.actions (gettext "Actions" appState.locale)
                (\row ->
                    let
                        rowActions =
                            toActions row
                    in
                    ItemActions.buttons rowActions.label rowActions.actions
                )

        columns =
            case cfg.rowActions of
                Just toActions ->
                    cfg.columns ++ [ actionsColumn toActions ]

                Nothing ->
                    cfg.columns
    in
    Table.table columns rows
        |> Table.withRowTestId (itemTestId << cfg.rowId)
        |> withMaybe Table.withCaption cfg.caption
        |> Table.withWords (UiWords.table appState)
        |> Table.toHtml


{-| Every row's test hook, in a list and in a table alike: `listing-item_<id>`.
-}
itemTestId : String -> String
itemTestId id =
    "listing-item_" ++ id


itemMenuPrefix : String
itemMenuPrefix =
    "listing-item-actions-"


sortMenuId : String
sortMenuId =
    "listing-sort"


{-| The sort's test hooks: `listing_toolbar_sort` on
the menu's button, `listing_toolbar_sort_<key>` on each option and
`listing_toolbar_sort-direction` on the direction's button.
-}
sortTestId : String
sortTestId =
    "listing_toolbar_sort"


filterMenuId : String -> String
filterMenuId filterId =
    "listing-filter-" ++ filterId


{-| The test hook of one value in a filter's menu: the filter and the value it
sets, never the words shown for it.
-}
filterOptionTestId : String -> String -> String
filterOptionTestId filterId value =
    filterMenuId filterId ++ "_option_" ++ value


viewItem : ViewConfig a msg -> State a -> Item msg -> ListItem.ListItem msg
viewItem cfg state row =
    let
        title =
            if List.isEmpty row.badges then
                row.title

            else
                Layout.inline (row.title :: List.map Badge.toHtml row.badges)
    in
    ListItem.item title
        |> withMaybe ListItem.withIcon row.icon
        |> ListItem.withFragments row.fragments
        |> ListItem.withDescription row.description
        |> withMaybe ListItem.withTimestamp row.timestamp
        |> ListItem.withHighlight row.highlight
        |> withMaybe ListItem.withDetail row.detail
        |> ListItem.withTestId (itemTestId row.id)
        |> (if List.isEmpty row.actions then
                identity

            else
                let
                    menuId =
                        itemMenuPrefix ++ row.id
                in
                ListItem.withActions
                    (case row.actionsTrigger of
                        Just trigger ->
                            ItemActions.labeledMenu
                                { id = menuId
                                , label = trigger.label
                                , content = trigger.content
                                , actions = row.actions
                                , model = getMenu menuId state.menus
                                , toMsg = cfg.wrapMsg << MenuMsg menuId (ItemActions.focusableCount row.actions)
                                }

                        Nothing ->
                            ItemActions.menu
                                { id = menuId
                                , label = row.actionsLabel
                                , actions = row.actions
                                , model = getMenu menuId state.menus
                                , toMsg = cfg.wrapMsg << MenuMsg menuId (ItemActions.focusableCount row.actions)
                                }
                    )
           )



-- TOOLBAR


{-| The one search box on a listing page, named so that the shortcut can find
it again.
-}
searchFieldId : String
searchFieldId =
    "listing-search"


{-| Cmd+F on a Mac, Ctrl+F elsewhere, puts the caret in the search box: on a
listing the find worth having is the one over everything there is, not the
browser's over the rows that happen to be on this page.

Both modifiers are registered rather than asking which platform this is. The
element ignores a press made inside a text field, which is where the two differ
on a Mac — Ctrl+F there is a caret movement, and it stays one.

It listens on the document and holds nothing, so it sits beside the toolbar
rather than around the page: an unknown element is an inline box, and a page's
layout does not belong inside one.

-}
viewSearchShortcut : Chrome c a msg -> Html msg
viewSearchShortcut cfg =
    Shortcut.shortcutElement
        [ Shortcut.metaShortcut (Shortcut.Regular "f") (cfg.wrapMsg FocusSearch)
        , Shortcut.ctrlShortcut (Shortcut.Regular "f") (cfg.wrapMsg FocusSearch)
        ]
        -- Out of the layout: it listens on the document, so it does not need a
        -- box, and an empty one in a column with a gap is a gap.
        [ style "display" "none" ]
        []


viewToolbar : WithLocale b -> Chrome c a msg -> Model a -> Html msg
viewToolbar appState cfg (Model state) =
    Toolbar.toHtml
        { label = gettext "Search and filters" appState.locale
        , search =
            -- The label is hidden, not absent: the placeholder says the same
            -- words, and a field without a name is "edit, blank".
            Just
                (Field.input
                    { id = searchFieldId
                    , label = cfg.searchPlaceholder
                    , value = state.queryInput
                    , onInput = cfg.wrapMsg << QueryInput
                    }
                    |> Field.withPlaceholder cfg.searchPlaceholder
                    |> Field.withHiddenLabel
                    |> Field.toHtml
                )
        , controls =
            List.map (viewFilter appState cfg state) cfg.filters
                ++ viewSort appState cfg state
        }


{-| A menu naming the order, and a button that flips it. The button's label says
both the current direction and the one it will change to, because an arrow
alone says neither to a screen reader.
-}
viewSort : WithLocale b -> Chrome c a msg -> State a -> List (Html msg)
viewSort appState cfg state =
    if List.isEmpty cfg.sortOptions then
        []

    else
        let
            sortItems =
                Menu.header (gettext "Order by" appState.locale)
                    :: List.map
                        (\( key, label ) ->
                            Menu.radioItem (state.paginationQueryString.sortBy == Just key)
                                label
                                (cfg.wrapMsg (SortSelected key))
                                |> Menu.withTestId (sortTestId ++ "_" ++ key)
                        )
                        cfg.sortOptions

            currentLabel =
                cfg.sortOptions
                    |> List.find (\( key, _ ) -> state.paginationQueryString.sortBy == Just key)
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault (gettext "Order by" appState.locale)

            ( directionIcon, directionLabel ) =
                if state.paginationQueryString.sortDirection == SortASC then
                    ( "fa-arrow-down-a-z", gettext "Sorted ascending. Sort descending" appState.locale )

                else
                    ( "fa-arrow-up-a-z", gettext "Sorted descending. Sort ascending" appState.locale )
        in
        [ ButtonGroup.toHtml
            { label = gettext "Sort" appState.locale
            , items =
                [ Menu.toHtml
                    { id = sortMenuId
                    , trigger = Menu.withTriggerTestId sortTestId (Menu.labeled currentLabel)
                    , alignment = Menu.End
                    , items = sortItems
                    , toMsg = cfg.wrapMsg << MenuMsg sortMenuId (Menu.focusableCount sortItems)
                    }
                    (getMenu sortMenuId state.menus)
                , Button.iconOnly (Icon.solid directionIcon) directionLabel (cfg.wrapMsg SortDirectionToggled)
                    |> Button.withTestId (sortTestId ++ "-direction")
                    |> Button.toHtml
                ]
            }
        ]


viewFilter : WithLocale b -> Chrome c a msg -> State a -> Filter msg -> Html msg
viewFilter appState cfg state filter =
    case filter of
        SingleFilter filterId filterCfg ->
            viewSingleFilter appState cfg state filterId filterCfg

        SingleNonEmptyFilter filterId filterCfg ->
            viewSingleFilter appState cfg state filterId filterCfg

        MultiFilter filterId filterCfg ->
            viewMultiFilter appState cfg state filterId filterCfg

        SearchFilter filterId filterCfg ->
            viewSearchFilter appState cfg state filterId filterCfg

        CustomFilter filterId filterCfg ->
            filterMenu cfg state filterId filterCfg.name filterCfg.value filterCfg.active filterCfg.items


{-| "Clear filter" at the foot of a filter that is on: the filter's value and
its AND/OR choice go.
-}
clearItems : WithLocale b -> Chrome c a msg -> State a -> String -> List (Menu.Item msg)
clearItems appState cfg state filterId =
    if PaginationQueryFilters.isFilterActive filterId state.filters then
        [ Menu.separator
        , Menu.item (gettext "Clear filter" appState.locale)
            (cfg.wrapMsg
                (FiltersChanged (Just (filterMenuId filterId))
                    (PaginationQueryFilters.removeFilter filterId state.filters)
                )
            )
            |> Menu.withTestId (filterMenuId filterId ++ "_clear")
        ]

    else
        []


filterMenu : Chrome c a msg -> State a -> String -> String -> Maybe String -> Bool -> List (Menu.Item msg) -> Html msg
filterMenu cfg state filterId name value active menuItems =
    let
        menuId =
            filterMenuId filterId
    in
    FilterMenu.toHtml
        { id = menuId
        , name = name
        , value = value
        , active = active
        , items = menuItems

        -- FilterMenu prepends a header, which is not a stop.
        , menu = getMenu menuId state.menus
        , toMsg = cfg.wrapMsg << MenuMsg menuId (Menu.focusableCount menuItems)
        }


viewSingleFilter : WithLocale b -> Chrome c a msg -> State a -> String -> { name : String, options : List ( String, String ) } -> Html msg
viewSingleFilter appState cfg state filterId filterCfg =
    let
        currentValue =
            PaginationQueryFilters.getValue filterId state.filters

        selected =
            List.find (\( value, _ ) -> currentValue == Just value) filterCfg.options

        choose newFilters =
            cfg.wrapMsg (FiltersChanged (Just (filterMenuId filterId)) newFilters)

        optionItems =
            List.map
                (\( value, label ) ->
                    Menu.radioItem (currentValue == Just value)
                        label
                        (choose (PaginationQueryFilters.insertValue filterId value state.filters))
                        |> Menu.withTestId (filterOptionTestId filterId value)
                )
                filterCfg.options
    in
    filterMenu cfg
        state
        filterId
        filterCfg.name
        (Maybe.map Tuple.second selected)
        (Maybe.isJust selected)
        (optionItems ++ clearItems appState cfg state filterId)


viewMultiFilter : WithLocale b -> Chrome c a msg -> State a -> String -> { name : String, options : List ( String, String ), maxVisibleValues : Int } -> Html msg
viewMultiFilter appState cfg state filterId filterCfg =
    let
        selectedValues =
            Internal.values filterId state.filters

        labelOf value =
            List.find (Tuple.first >> (==) value) filterCfg.options
                |> Maybe.map Tuple.second
                |> Maybe.withDefault value

        optionItems =
            List.map
                (\( value, label ) ->
                    Menu.checkItem (List.member value selectedValues)
                        label
                        (cfg.wrapMsg (FiltersChanged Nothing (Internal.toggleValue filterId value state.filters)))
                        |> Menu.withTestId (filterOptionTestId filterId value)
                )
                filterCfg.options
    in
    filterMenu cfg
        state
        filterId
        filterCfg.name
        (summary (max 1 filterCfg.maxVisibleValues) (List.map labelOf selectedValues))
        (not (List.isEmpty selectedValues))
        (optionItems ++ clearItems appState cfg state filterId)


{-| The button's words for several values: as many names as fit, and a count of
the rest.
-}
summary : Int -> List String -> Maybe String
summary maxVisible labels =
    case labels of
        [] ->
            Nothing

        _ ->
            let
                rest =
                    List.length labels - maxVisible
            in
            Just
                (String.join ", " (List.take maxVisible labels)
                    ++ (if rest > 0 then
                            " +" ++ String.fromInt rest

                        else
                            ""
                       )
                )


{-| A search box, the AND/OR choice once there is something to combine, what is
already chosen (kept at the top so it cannot scroll away), then what the search
found.
-}
viewSearchFilter :
    WithLocale b
    -> Chrome c a msg
    -> State a
    -> String
    -> { name : String, searchPlaceholder : String, noResults : String, operator : Bool }
    -> Html msg
viewSearchFilter appState cfg state filterId filterCfg =
    let
        search =
            Maybe.withDefault emptySearch (Dict.get filterId state.searches)

        selectedIds =
            Internal.values filterId state.filters

        selected =
            List.map (\id -> { id = id, label = Maybe.withDefault id (Dict.get id search.labels) }) selectedIds

        toggle suggestion =
            cfg.wrapMsg (SearchToggled filterId suggestion)

        operatorItems =
            if filterCfg.operator && List.length selectedIds > 1 then
                let
                    current =
                        Internal.operator filterId state.filters

                    option op label =
                        { label = label
                        , icon = Nothing
                        , selected = current == op
                        , onSelect = cfg.wrapMsg (FiltersChanged Nothing (Internal.setOperator filterId op state.filters))
                        }
                in
                [ Menu.segmented
                    { label = gettext "Match" appState.locale
                    , options = [ option OR (gettext "OR" appState.locale), option AND (gettext "AND" appState.locale) ]
                    }
                , Menu.separator
                ]

            else
                []

        selectedItems =
            List.map (\s -> Menu.checkItem True s.label (toggle s) |> Menu.withTestId (filterOptionTestId filterId s.id)) selected

        foundItems =
            case search.suggestions of
                ActionResult.Success suggestions ->
                    case List.filter (\s -> List.notMember s.id selectedIds) suggestions of
                        [] ->
                            if String.isEmpty search.query then
                                []

                            else
                                [ Menu.note [ text filterCfg.noResults ] ]

                        found ->
                            List.map (\s -> Menu.checkItem False s.label (toggle s) |> Menu.withTestId (filterOptionTestId filterId s.id)) found

                ActionResult.Error message ->
                    [ Menu.note [ text message ] ]

                _ ->
                    [ Menu.note [ text (gettext "Loading..." appState.locale) ] ]
    in
    filterMenu cfg
        state
        filterId
        filterCfg.name
        (summary 1 (List.map .label selected))
        (not (List.isEmpty selectedIds))
        (Menu.searchInput
            { placeholder = filterCfg.searchPlaceholder
            , value = search.query
            , onInput = cfg.wrapMsg << SearchInput filterId
            }
            :: Menu.separator
            :: operatorItems
            ++ selectedItems
            ++ foundItems
            ++ clearItems appState cfg state filterId
        )
