module Wizard.Common.Components.Listing.View exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingDropdownItem
    , UpdatedTimeConfig
    , ViewConfig
    , dropdownAction
    , dropdownSeparator
    , view
    , viewItem
    )

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (Html, a, div, input, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Pagination.Page exposing (Page)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lx)
import Time exposing (Month(..))
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models exposing (Item, Model)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))
import Wizard.Common.Components.ListingDropdown as ListingDropdown
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes as Routes exposing (Route)
import Wizard.Routing as Routing


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.Listing.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.Listing.View"


type alias ViewConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , dropdownItems : a -> List (ListingDropdownItem msg)
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedTimeConfig a)
    , iconView : Maybe (a -> Html msg)
    , sortOptions : List ( String, String )
    , wrapMsg : Msg a -> msg
    , toRoute : PaginationQueryString -> Route
    , toolbarExtra : Maybe (Html msg)
    }


type alias UpdatedTimeConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


type ListingDropdownItem msg
    = ListingDropdownAction (ListingActionConfig msg)
    | ListingDropdownSeparator


type alias ListingActionConfig msg =
    { extraClass : Maybe String
    , icon : Html msg
    , label : String
    , msg : ListingActionType msg
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routes.Route
    | ListingActionExternalLink String


dropdownAction =
    ListingDropdownAction


dropdownSeparator =
    ListingDropdownSeparator


view : AppState -> ViewConfig a msg -> Model a -> Html msg
view appState config model =
    div [ class "Listing" ]
        [ viewToolbar appState config model
        , Page.actionResultView appState (viewList appState config model) model.pagination
        ]


viewToolbar : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbar appState cfg model =
    div [ class "listing-toolbar mb-3 form-inline" ]
        [ div [ class "filter-sort" ]
            [ viewToolbarFilter appState cfg model
            , viewToolbarSort appState cfg model
            ]
        , Maybe.withDefault emptyNode cfg.toolbarExtra
        ]


viewToolbarFilter : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbarFilter appState cfg model =
    input
        [ type_ "text"
        , placeholder (l_ "toolbarFilter.placeholder" appState)
        , onInput (cfg.wrapMsg << QueryInput)
        , value model.qInput
        , class "form-control"
        , id "filter"
        ]
        []


viewToolbarSort : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbarSort appState cfg model =
    let
        paginationQueryString =
            model.paginationQueryString

        currentSort =
            List.find (\( k, _ ) -> paginationQueryString.sortBy == Just k) cfg.sortOptions
                |> Maybe.unwrap "" Tuple.second

        sortOption ( name, visibleName ) =
            let
                route =
                    cfg.toRoute { paginationQueryString | sortBy = Just name, page = Just 1 }
            in
            Dropdown.anchorItem [ href <| Routing.toUrl appState route ]
                [ text visibleName ]

        ( sortDirectionButtonUrl, sortDirectionButtonIcon ) =
            if paginationQueryString.sortDirection == SortASC then
                ( cfg.toRoute { paginationQueryString | sortDirection = SortDESC, page = Just 1 }
                , faSet "_global.sortAsc" appState
                )

            else
                ( cfg.toRoute { paginationQueryString | sortDirection = SortASC, page = Just 1 }
                , faSet "_global.sortDesc" appState
                )
    in
    div [ class "btn-group" ]
        [ Dropdown.dropdown model.sortDropdownState
            { options = []
            , toggleMsg = cfg.wrapMsg << SortDropdownMsg
            , toggleButton =
                Dropdown.toggle [ Button.outlineSecondary ] [ text currentSort ]
            , items =
                Dropdown.header [ lx_ "toolbarSort.orderBy" appState ] :: List.map sortOption cfg.sortOptions
            }
        , linkTo appState
            sortDirectionButtonUrl
            [ class "btn btn-outline-secondary", attribute "data-cy" "sort-direction" ]
            [ sortDirectionButtonIcon ]
        ]


viewList : AppState -> ViewConfig a msg -> Model a -> Pagination a -> Html msg
viewList appState cfg model pagination =
    if List.length pagination.items > 0 then
        div []
            [ div [ class "list-group list-group-flush" ]
                (List.indexedMap (viewItem appState cfg) model.items)
            , viewPagination appState cfg model pagination.page
            ]

    else
        viewEmpty appState cfg


viewPagination : AppState -> ViewConfig a msg -> Model a -> Page -> Html msg
viewPagination appState cfg model page =
    let
        paginationQueryString =
            model.paginationQueryString

        currentPage =
            Maybe.withDefault 1 model.paginationQueryString.page

        viewPageLink pageNumber attributes content =
            li (class "page-item" :: attributes)
                [ linkTo appState
                    (cfg.toRoute { paginationQueryString | page = Just pageNumber })
                    [ class "page-link" ]
                    content
                ]

        viewNavLink number =
            viewPageLink number
                [ classList [ ( "active", number == currentPage ) ] ]
                [ text (String.fromInt number) ]

        firstLink =
            if currentPage > 1 then
                viewPageLink 1
                    [ class "icon-left" ]
                    [ fa "fas fa-angle-double-left"
                    , lx_ "pagination.first" appState
                    ]

            else
                emptyNode

        prevLink =
            viewPageLink (currentPage - 1)
                [ class "icon-left", classList [ ( "disabled", currentPage == 1 ) ] ]
                [ fa "fas fa-angle-left"
                , lx_ "pagination.prev" appState
                ]

        dots =
            li [ class "page-item disabled" ] [ a [ class "page-link" ] [ text "..." ] ]

        ( left, leftDots ) =
            if currentPage - 4 > 1 then
                ( currentPage - 4, dots )

            else
                ( 1, emptyNode )

        ( right, rightDots ) =
            if currentPage + 4 < page.totalPages then
                ( currentPage + 4, dots )

            else
                ( page.totalPages, emptyNode )

        pageLinks =
            List.map viewNavLink (List.range left right)

        nextLink =
            viewPageLink (currentPage + 1)
                [ class "icon-right", classList [ ( "disabled", currentPage == page.totalPages ) ] ]
                [ lx_ "pagination.next" appState
                , fa "fas fa-angle-right"
                ]

        lastLink =
            if currentPage < page.totalPages then
                viewPageLink page.totalPages
                    [ class "icon-right" ]
                    [ lx_ "pagination.last" appState
                    , fa "fas fa-angle-double-right"
                    ]

            else
                emptyNode

        links =
            [ firstLink, prevLink, leftDots ] ++ pageLinks ++ [ rightDots, nextLink, lastLink ]
    in
    if page.totalPages > 1 then
        nav [] [ ul [ class "pagination" ] links ]

    else
        emptyNode


viewEmpty : AppState -> ViewConfig a msg -> Html msg
viewEmpty appState config =
    Page.illustratedMessage
        { image = "no_data"
        , heading = l_ "empty.heading" appState
        , lines = [ config.emptyText ]
        }


viewItem : AppState -> ViewConfig a msg -> Int -> Item a -> Html msg
viewItem appState config index item =
    let
        actions =
            config.dropdownItems item.item

        dropdown =
            if List.length actions > 0 then
                ListingDropdown.dropdown appState
                    { dropdownState = item.dropdownState
                    , toggleMsg = config.wrapMsg << ItemDropdownMsg index
                    , items = List.map (viewAction appState) actions
                    }

            else
                emptyNode

        icon =
            config.iconView
                |> Maybe.andMap (Just item.item)
                |> Maybe.withDefault (ItemIcon.view { text = config.textTitle item.item, image = Nothing })
    in
    div [ class "list-group-item" ]
        [ icon
        , div [ class "content" ]
            [ div [ class "title-row" ]
                [ span [ class "title" ] [ config.title item.item ]
                ]
            , div [ class "extra" ]
                [ div [ class "description" ]
                    [ config.description item.item ]
                ]
            ]
        , div [ class "updated" ]
            [ viewUpdated appState config item.item ]
        , div [ class "actions" ]
            [ dropdown ]
        ]


viewUpdated : AppState -> ViewConfig a msg -> a -> Html msg
viewUpdated appState config item =
    case config.updated of
        Just updated ->
            let
                time =
                    updated.getTime item

                readableTime =
                    TimeUtils.toReadableDateTime appState.timeZone time
            in
            span [ title readableTime ]
                [ text <| l_ "item.updated" appState ++ inWordsWithConfig { withAffix = True } (locale appState) time updated.currentTime ]

        Nothing ->
            emptyNode


viewAction : AppState -> ListingDropdownItem msg -> Dropdown.DropdownItem msg
viewAction appState dropdownItem =
    case dropdownItem of
        ListingDropdownAction action ->
            let
                attrs =
                    case action.msg of
                        ListingActionLink route ->
                            [ href <| Routing.toUrl appState route ]

                        ListingActionExternalLink url ->
                            [ href url, target "_blank" ]

                        ListingActionMsg msg ->
                            [ onClick msg ]
            in
            Dropdown.anchorItem
                ([ class <| Maybe.withDefault "" action.extraClass ] ++ attrs)
                [ action.icon, text action.label ]

        ListingDropdownSeparator ->
            Dropdown.divider
