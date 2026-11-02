module Common.Components.Listing.Internal exposing
    ( Range
    , applyFilters
    , applyQuery
    , clearAll
    , hasQueryOrFilters
    , isCurrent
    , operator
    , pageAfterDelete
    , range
    , setOperator
    , setSortBy
    , setValues
    , toggleDirection
    , toggleValue
    , values
    )

{-| The state transitions of `Common.Components.Listing`, as pure functions.

Separate so they can be tested: the listing's `update` needs a
`Browser.Navigation.Key`, which a test cannot construct, and everything worth
checking about the listing — which page a filter change lands on, which response
is stale, where a delete leaves you — is decided here, before any command.

Not for pages. Use `Common.Components.Listing`.

-}

import ActionResult exposing (ActionResult)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Pagination.Page exposing (Page)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator(..))
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Dict
import List.Extra as List


{-| New filters, run through the page's validation, and the page reset: a
filter changes which items there are, so page 7 of the old set means nothing.
-}
applyFilters :
    Maybe (PaginationQueryFilters -> PaginationQueryFilters)
    -> PaginationQueryFilters
    -> PaginationQueryString
    -> ( PaginationQueryFilters, PaginationQueryString )
applyFilters validate newFilters pqs =
    ( Maybe.withDefault identity validate newFilters
    , PaginationQueryString.resetPage pqs
    )


{-| A search applied: the query set and the first page.
-}
applyQuery : String -> PaginationQueryString -> PaginationQueryString
applyQuery query pqs =
    { pqs | q = Just query, page = Just 1 }


{-| Ordered by another field, from the first page.
-}
setSortBy : String -> PaginationQueryString -> PaginationQueryString
setSortBy sortBy pqs =
    { pqs | sortBy = Just sortBy, page = Just 1 }


toggleDirection : PaginationQueryString -> PaginationQueryString
toggleDirection pqs =
    { pqs
        | sortDirection =
            if pqs.sortDirection == SortASC then
                SortDESC

            else
                SortASC
        , page = Just 1
    }


{-| No query, no filters (before the page's validation), first page. The sort
stays: it is how the user likes to read the list, not a narrowing of it.
-}
clearAll : Maybe (PaginationQueryFilters -> PaginationQueryFilters) -> PaginationQueryString -> ( PaginationQueryFilters, PaginationQueryString )
clearAll validate pqs =
    applyFilters validate PaginationQueryFilters.empty { pqs | q = Nothing }


hasQueryOrFilters : String -> PaginationQueryFilters -> Bool
hasQueryOrFilters query filters =
    not (String.isEmpty query) || List.any (not << String.isEmpty) (Dict.values filters.values)


{-| Whether a response still answers the current question. Anything asked for
before the latest change of query, sort, page or filters is dropped, so a slow
response cannot overwrite a newer one.
-}
isCurrent :
    { c | paginationQueryString : PaginationQueryString, filters : PaginationQueryFilters }
    -> PaginationQueryString
    -> PaginationQueryFilters
    -> Bool
isCurrent current pqs filters =
    current.paginationQueryString == pqs && current.filters == filters


{-| After a delete: if that was the only item on the last page (and it was not
the first page), step back one page, or the user is left on an empty page that
no longer exists.
-}
pageAfterDelete : ActionResult (Pagination a) -> PaginationQueryString -> PaginationQueryString
pageAfterDelete result pqs =
    case result of
        ActionResult.Success pagination ->
            let
                isLastPage =
                    pagination.page.number == (pagination.page.totalPages - 1)

                isFirstPage =
                    pagination.page.number == 0

                isLastElement =
                    pagination.page.size > 0 && modBy pagination.page.size pagination.page.totalElements == 1
            in
            if isLastPage && not isFirstPage && isLastElement then
                -- `number` is zero-based and the query string one-based, so
                -- the current number is the previous page.
                PaginationQueryString.setPage pqs pagination.page.number

            else
                pqs

        _ ->
            pqs


{-| The values of a multi-valued filter, which travel as one comma-separated
string.
-}
values : String -> PaginationQueryFilters -> List String
values filterId filters =
    PaginationQueryFilters.getValue filterId filters
        |> Maybe.map (String.split ",")
        |> Maybe.withDefault []
        |> List.filterNot String.isEmpty


{-| Sets the values of a multi-valued filter. No values removes the filter
altogether — its operator too, since there is nothing left to combine.
-}
setValues : String -> List String -> PaginationQueryFilters -> PaginationQueryFilters
setValues filterId newValues filters =
    case List.unique (List.filterNot String.isEmpty newValues) of
        [] ->
            PaginationQueryFilters.removeFilter filterId filters

        unique ->
            PaginationQueryFilters.insertValue filterId (String.join "," unique) filters


{-| Adds a value that is not there, removes one that is. Added values go at the
end, so the order on the button is the order they were chosen in.
-}
toggleValue : String -> String -> PaginationQueryFilters -> PaginationQueryFilters
toggleValue filterId value filters =
    let
        current =
            values filterId filters
    in
    if List.member value current then
        setValues filterId (List.remove value current) filters

    else
        setValues filterId (current ++ [ value ]) filters


{-| How the values of a filter combine. `OR` unless the query string says
otherwise, as in the wizard.
-}
operator : String -> PaginationQueryFilters -> FilterOperator
operator filterId filters =
    Maybe.withDefault OR (PaginationQueryFilters.getOp filterId filters)


setOperator : String -> FilterOperator -> PaginationQueryFilters -> PaginationQueryFilters
setOperator =
    PaginationQueryFilters.insertOp


type alias Range =
    { from : Int
    , to : Int
    , total : Int
    }


{-| Which items of how many a page shows, one-based, for the status line.
-}
range : Page -> Int -> Range
range page itemCount =
    let
        from =
            page.number * page.size + 1
    in
    { from = from
    , to = from + itemCount - 1
    , total = page.totalElements
    }
