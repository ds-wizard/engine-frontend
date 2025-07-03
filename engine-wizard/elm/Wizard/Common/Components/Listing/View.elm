module Wizard.Common.Components.Listing.View exposing
    ( CustomFilterConfig
    , Filter(..)
    , SimpleFilterConfig
    , SimpleMultiFilterConfig
    , UpdatedTimeConfig
    , ViewConfig
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, input, li, nav, span, text, ul)
import Html.Attributes exposing (class, classList, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Components.FontAwesome exposing (fa, faListingFilterMultiNotSelected, faListingFilterMultiSelected, faListingFilterSingleNotSelected, faListingFilterSingleSelected, faSortAsc, faSortDesc)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Pagination.Page exposing (Page)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Shared.Undraw as Undraw
import String.Format as String
import Time
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models exposing (Item, Model)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))
import Wizard.Common.Components.ListingDropdown as ListingDropdown exposing (ListingDropdownItem)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Common.View.Page as Page
import Wizard.Routes exposing (Route)


type alias ViewConfig a msg =
    { title : a -> Html msg
    , description : a -> Html msg
    , itemAdditionalData : a -> Maybe (List (Html msg))
    , dropdownItems : a -> List (ListingDropdownItem msg)
    , textTitle : a -> String
    , emptyText : String
    , updated : Maybe (UpdatedTimeConfig a)
    , iconView : Maybe (a -> Html msg)
    , searchPlaceholderText : Maybe String
    , sortOptions : List ( String, String )
    , filters : List (Filter msg)
    , wrapMsg : Msg a -> msg
    , toRoute : PaginationQueryFilters -> PaginationQueryString -> Route
    , toolbarExtra : Maybe (Html msg)
    }


type alias UpdatedTimeConfig a =
    { getTime : a -> Time.Posix
    , currentTime : Time.Posix
    }


type Filter msg
    = SimpleFilter String SimpleFilterConfig
    | SimpleMultiFilter String SimpleMultiFilterConfig
    | CustomFilter String (CustomFilterConfig msg)


type alias SimpleFilterConfig =
    { name : String
    , options : List ( String, String )
    }


type alias SimpleMultiFilterConfig =
    { name : String
    , options : List ( String, String )
    , maxVisibleValues : Int
    }


type alias CustomFilterConfig msg =
    { label : List (Html msg)
    , items : List (Dropdown.DropdownItem msg)
    }


view : AppState -> ViewConfig a msg -> Model a -> Html msg
view appState config model =
    div [ class "Listing" ]
        [ viewToolbar appState config model
        , Page.actionResultView appState (viewList appState config model) model.pagination
        ]


viewToolbar : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbar appState cfg model =
    div [ class "listing-toolbar mb-2" ]
        [ div [ class "filter-sort" ]
            ([ viewToolbarSearch appState cfg model
             , viewToolbarSort appState cfg model
             ]
                ++ viewToolbarFilters appState cfg model
            )
        , Maybe.unwrap Html.nothing (div [ class "ms-4" ] << List.singleton) cfg.toolbarExtra
        ]


viewToolbarSearch : AppState -> ViewConfig a msg -> Model a -> Html msg
viewToolbarSearch appState cfg model =
    let
        placeholderText =
            Maybe.withDefault (gettext "Filter by name..." appState.locale) cfg.searchPlaceholderText
    in
    input
        [ type_ "text"
        , placeholder placeholderText
        , onInput (cfg.wrapMsg << QueryInput)
        , value model.qInput
        , class "form-control d-inline w-auto align-top me-3"
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

        updateMsg =
            cfg.wrapMsg << UpdatePaginationQueryString

        sortOption ( name, visibleName ) =
            let
                msg =
                    updateMsg { paginationQueryString | sortBy = Just name, page = Just 1 }
            in
            Dropdown.buttonItem [ onClick msg ]
                [ text visibleName ]

        ( sortDirectionButtonMsg, sortDirectionButtonIcon ) =
            if paginationQueryString.sortDirection == SortASC then
                ( updateMsg { paginationQueryString | sortDirection = SortDESC, page = Just 1 }
                , faSortAsc
                )

            else
                ( updateMsg { paginationQueryString | sortDirection = SortASC, page = Just 1 }
                , faSortDesc
                )
    in
    div [ class "btn-group" ]
        [ Dropdown.dropdown model.sortDropdownState
            { options = []
            , toggleMsg = cfg.wrapMsg << SortDropdownMsg
            , toggleButton =
                Dropdown.toggle [ Button.outlineSecondary ] [ text currentSort ]
            , items =
                Dropdown.header [ text (gettext "Order by" appState.locale) ] :: List.map sortOption cfg.sortOptions
            }
        , button
            [ class "btn btn-outline-secondary"
            , dataCy "listing_toolbar_sort-direction"
            , onClick sortDirectionButtonMsg
            ]
            [ sortDirectionButtonIcon ]
        ]


viewToolbarFilters : AppState -> ViewConfig a msg -> Model a -> List (Html msg)
viewToolbarFilters appState cfg model =
    List.map (viewToolbarFilter appState cfg model) cfg.filters


viewToolbarFilter : AppState -> ViewConfig a msg -> Model a -> Filter msg -> Html msg
viewToolbarFilter appState cfg model filter =
    case filter of
        SimpleFilter filterId filterCfg ->
            viewToolbarSimpleFilter appState cfg model filterId filterCfg

        SimpleMultiFilter filterId filterCfg ->
            viewToolbarSimpleMultiFilter appState cfg model filterId filterCfg

        CustomFilter filterId filterCfg ->
            viewToolbarCustomFilter appState cfg model filterId filterCfg


viewToolbarSimpleFilter : AppState -> ViewConfig a msg -> Model a -> String -> SimpleFilterConfig -> Html msg
viewToolbarSimpleFilter appState cfg model filterId filterCfg =
    let
        item ( value, visibleName ) =
            let
                newFiltersMsg =
                    (cfg.wrapMsg << UpdatePaginationQueryFilters Nothing)
                        (PaginationQueryFilters.insertValue filterId value model.filters)

                icon =
                    if Maybe.unwrap False ((==) value << Tuple.first) maybeFilterValue then
                        faListingFilterSingleSelected

                    else
                        faListingFilterSingleNotSelected
            in
            Dropdown.buttonItem [ onClick newFiltersMsg, class "dropdown-item-icon" ]
                [ icon, text visibleName ]

        maybeFilterValue =
            PaginationQueryFilters.getValue filterId model.filters
                |> Maybe.andThen (\value -> List.find (Tuple.first >> (==) value) filterCfg.options)

        filterLabel =
            maybeFilterValue
                |> Maybe.map Tuple.second
                |> Maybe.withDefault filterCfg.name

        label =
            [ span [ class "filter-text-label" ] [ text filterLabel ] ]

        items =
            List.map item filterCfg.options
    in
    viewFilter appState cfg model filterId label items


viewToolbarSimpleMultiFilter : AppState -> ViewConfig a msg -> Model a -> String -> SimpleMultiFilterConfig -> Html msg
viewToolbarSimpleMultiFilter appState cfg model filterId filterCfg =
    let
        item ( value, visibleName ) =
            let
                ( icon, newFilterValue ) =
                    if List.member value filterValues then
                        ( faListingFilterMultiSelected
                        , removeValue value
                        )

                    else
                        ( faListingFilterMultiNotSelected
                        , addValue value
                        )

                newFilters =
                    if String.isEmpty newFilterValue then
                        PaginationQueryFilters.removeFilter filterId model.filters

                    else
                        PaginationQueryFilters.insertValue filterId newFilterValue model.filters

                newFiltersMsg =
                    (cfg.wrapMsg << UpdatePaginationQueryFilters (Just filterId)) newFilters
            in
            Dropdown.buttonItem [ onClick newFiltersMsg, class "dropdown-item-icon" ]
                [ icon, text visibleName ]

        addValue value =
            String.join "," <|
                filterValues
                    ++ [ value ]

        removeValue value =
            String.join "," <|
                List.filter ((/=) value) filterValues

        filterValues =
            PaginationQueryFilters.getValue filterId model.filters
                |> Maybe.unwrap [] (String.split ",")

        filterValuesCount =
            List.length filterValues

        filterValueToLabel value =
            List.find ((==) value << Tuple.first) filterCfg.options
                |> Maybe.unwrap value Tuple.second

        filterLabel =
            if filterValuesCount == 0 then
                filterCfg.name

            else
                List.take filterCfg.maxVisibleValues filterValues
                    |> List.map filterValueToLabel
                    |> String.join ", "

        filterBadge =
            if filterValuesCount > filterCfg.maxVisibleValues then
                Badge.dark [ class "rounded-pill" ] [ text ("+" ++ String.fromInt (filterValuesCount - filterCfg.maxVisibleValues)) ]

            else
                Html.nothing

        label =
            [ span [ class "filter-text-label" ] [ text filterLabel ], filterBadge ]

        items =
            List.map item filterCfg.options
    in
    viewFilter appState cfg model filterId label items


viewToolbarCustomFilter : AppState -> ViewConfig a msg -> Model a -> String -> CustomFilterConfig msg -> Html msg
viewToolbarCustomFilter appState cfg model filterId filterCfg =
    viewFilter appState cfg model filterId filterCfg.label filterCfg.items


viewFilter : AppState -> ViewConfig a msg -> Model a -> String -> List (Html msg) -> List (Dropdown.DropdownItem msg) -> Html msg
viewFilter appState cfg model filterId label items =
    let
        state =
            Maybe.withDefault Dropdown.initialState (Dict.get filterId model.filterDropdownStates)

        filterActive =
            PaginationQueryFilters.isFilterActive filterId model.filters

        buttonClass =
            if filterActive then
                Button.secondary

            else
                Button.outlineSecondary

        clearSelection =
            if filterActive then
                let
                    clearAllMsg =
                        (cfg.wrapMsg << UpdatePaginationQueryFilters Nothing)
                            (PaginationQueryFilters.removeFilter filterId model.filters)

                    clearAllItem =
                        Dropdown.buttonItem [ onClick clearAllMsg ]
                            [ text (gettext "Clear selection" appState.locale) ]
                in
                [ Dropdown.divider
                , clearAllItem
                ]

            else
                []
    in
    Dropdown.dropdown state
        { options = [ Dropdown.attrs [ class "btn-group-filter", id ("filter-" ++ filterId) ] ]
        , toggleMsg = cfg.wrapMsg << FilterDropdownMsg filterId
        , toggleButton =
            Dropdown.toggle [ buttonClass ] label
        , items = items ++ clearSelection
        }


viewList : AppState -> ViewConfig a msg -> Model a -> Pagination a -> Html msg
viewList appState cfg model pagination =
    if List.length pagination.items > 0 then
        div []
            [ div [ class "list-group list-group-flush", dataCy "listing_list" ]
                (List.indexedMap (viewItem appState cfg) model.items)
            , viewPagination appState cfg model pagination.page
            ]

    else
        viewEmpty appState cfg model


viewPagination : AppState -> ViewConfig a msg -> Model a -> Page -> Html msg
viewPagination appState cfg model page =
    let
        paginationQueryString =
            model.paginationQueryString

        currentPage =
            Maybe.withDefault 1 model.paginationQueryString.page

        viewPageLink pageNumber attributes content =
            li (class "page-item" :: attributes)
                [ button
                    [ onClick (cfg.wrapMsg (UpdatePaginationQueryString { paginationQueryString | page = Just pageNumber }))
                    , class "page-link"
                    ]
                    content
                ]

        viewNavLink number =
            viewPageLink number
                [ classList [ ( "active", number == currentPage ) ]
                , dataCy "listing_page-link"
                ]
                [ text (String.fromInt number) ]

        dots =
            li [ class "page-item disabled" ] [ a [ class "page-link" ] [ text "..." ] ]

        ( left, leftDots ) =
            if currentPage - 4 > 1 then
                ( currentPage - 4, dots )

            else
                ( 1, Html.nothing )

        ( right, rightDots ) =
            if currentPage + 4 < page.totalPages then
                ( currentPage + 4, dots )

            else
                ( page.totalPages, Html.nothing )
    in
    if page.totalPages > 1 then
        let
            lastLink =
                if currentPage < page.totalPages then
                    viewPageLink page.totalPages
                        [ class "icon-right" ]
                        [ text (gettext "Last" appState.locale)
                        , fa "fas fa-angle-double-right"
                        ]

                else
                    Html.nothing

            nextLink =
                viewPageLink (currentPage + 1)
                    [ class "icon-right"
                    , classList [ ( "disabled", currentPage == page.totalPages ) ]
                    , dataCy "listing_page-link_next"
                    ]
                    [ text (gettext "Next" appState.locale)
                    , fa "fas fa-angle-right"
                    ]

            pageLinks =
                List.map viewNavLink (List.range left right)

            prevLink =
                viewPageLink (currentPage - 1)
                    [ class "icon-left"
                    , classList [ ( "disabled", currentPage == 1 ) ]
                    , dataCy "listing_page-link_prev"
                    ]
                    [ fa "fas fa-angle-left"
                    , text (gettext "Prev" appState.locale)
                    ]

            firstLink =
                if currentPage > 1 then
                    viewPageLink 1
                        [ class "icon-left" ]
                        [ fa "fas fa-angle-double-left"
                        , text (gettext "First" appState.locale)
                        ]

                else
                    Html.nothing

            links =
                [ firstLink, prevLink, leftDots ] ++ pageLinks ++ [ rightDots, nextLink, lastLink ]
        in
        nav [] [ ul [ class "pagination" ] links ]

    else
        Html.nothing


viewEmpty : AppState -> ViewConfig a msg -> Model a -> Html msg
viewEmpty appState config model =
    let
        filtersActive =
            not (String.isEmpty model.qInput && Dict.isEmpty model.filters.values)

        emptyText =
            if filtersActive then
                gettext "There are no results matching your search and filters." appState.locale

            else
                config.emptyText
    in
    Page.illustratedMessage
        { image = Undraw.noData
        , heading = gettext "No data" appState.locale
        , lines = [ emptyText ]
        , cy = "listing-empty"
        }


viewItem : AppState -> ViewConfig a msg -> Int -> Item a -> Html msg
viewItem appState config index item =
    let
        actions =
            config.dropdownItems item.item

        dropdown =
            if List.length actions > 0 then
                ListingDropdown.dropdown
                    { dropdownState = item.dropdownState
                    , toggleMsg = config.wrapMsg << ItemDropdownMsg index
                    , items = actions
                    }

            else
                Html.nothing

        icon =
            config.iconView
                |> Maybe.andMap (Just item.item)
                |> Maybe.withDefault (ItemIcon.view { text = config.textTitle item.item, image = Nothing })

        additionalData =
            Maybe.unwrap Html.nothing (div [ class "additional-data" ]) (config.itemAdditionalData item.item)
    in
    div [ class "list-group-item", dataCy "listing_item" ]
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
        , additionalData
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
            span (tooltip readableTime)
                [ text <| String.format (gettext "Updated %s" appState.locale) [ inWordsWithConfig { withAffix = True } (locale appState) time updated.currentTime ] ]

        Nothing ->
            Html.nothing
