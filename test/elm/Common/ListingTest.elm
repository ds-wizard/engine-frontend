module Common.ListingTest exposing
    ( filterChangeTests
    , multiValueTests
    , operatorTests
    , pageAfterDeleteTests
    , staleResponseTests
    , statusRangeTests
    )

import ActionResult
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.Listing.Internal as Internal
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryFilters.FilterOperator as FilterOperator exposing (FilterOperator(..))
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Expect
import Test exposing (Test, describe, test)


onPage : Int -> PaginationQueryString
onPage page =
    PaginationQueryString.setPage PaginationQueryString.empty page


paginationOf : { number : Int, size : Int, totalElements : Int, totalPages : Int } -> Pagination String
paginationOf page =
    { items = [ "item" ]
    , page = page
    }


filterChangeTests : Test
filterChangeTests =
    describe "applyFilters"
        [ test "resets the page" <|
            \_ ->
                Internal.applyFilters Nothing (PaginationQueryFilters.fromValues [ ( "role", Just "admin" ) ]) (onPage 4)
                    |> Tuple.second
                    |> .page
                    |> Expect.equal Nothing
        , test "keeps the query and the sort" <|
            \_ ->
                let
                    pqs =
                        { empty | q = Just "dmp", sortBy = Just "name" }

                    empty =
                        onPage 3
                in
                Internal.applyFilters Nothing PaginationQueryFilters.empty pqs
                    |> Tuple.second
                    |> (\result -> ( result.q, result.sortBy ))
                    |> Expect.equal ( Just "dmp", Just "name" )
        , test "runs the page's validation" <|
            \_ ->
                let
                    withDefault filters =
                        if PaginationQueryFilters.isFilterActive "state" filters then
                            filters

                        else
                            PaginationQueryFilters.insertValue "state" "latest" filters
                in
                Internal.applyFilters (Just withDefault) PaginationQueryFilters.empty (onPage 2)
                    |> Tuple.first
                    |> PaginationQueryFilters.getValue "state"
                    |> Expect.equal (Just "latest")
        , test "clearing drops the query and the filters but keeps the sort" <|
            \_ ->
                let
                    base =
                        onPage 5

                    pqs =
                        { base | q = Just "dmp", sortBy = Just "name" }
                in
                Internal.clearAll Nothing pqs
                    |> (\( filters, result ) -> ( filters, result.q, ( result.sortBy, result.page ) ))
                    |> Expect.equal ( PaginationQueryFilters.empty, Nothing, ( Just "name", Nothing ) )
        , test "a query goes to the first page" <|
            \_ ->
                Internal.applyQuery "dmp" (onPage 7)
                    |> (\result -> ( result.q, result.page ))
                    |> Expect.equal ( Just "dmp", Just 1 )
        , test "search or filters count as narrowing; an empty value does not" <|
            \_ ->
                [ Internal.hasQueryOrFilters "" PaginationQueryFilters.empty
                , Internal.hasQueryOrFilters "x" PaginationQueryFilters.empty
                , Internal.hasQueryOrFilters "" (PaginationQueryFilters.fromValues [ ( "role", Just "" ) ])
                , Internal.hasQueryOrFilters "" (PaginationQueryFilters.fromValues [ ( "role", Just "admin" ) ])
                ]
                    |> Expect.equal [ False, True, False, True ]
        ]


staleResponseTests : Test
staleResponseTests =
    let
        filters =
            PaginationQueryFilters.fromValues [ ( "role", Just "admin" ) ]

        current =
            { paginationQueryString = onPage 2, filters = filters }
    in
    describe "isCurrent"
        [ test "a response to the current request is kept" <|
            \_ ->
                Internal.isCurrent current (onPage 2) filters
                    |> Expect.equal True
        , test "a response to an older page is dropped" <|
            \_ ->
                Internal.isCurrent current (onPage 1) filters
                    |> Expect.equal False
        , test "a response to older filters is dropped" <|
            \_ ->
                Internal.isCurrent current (onPage 2) PaginationQueryFilters.empty
                    |> Expect.equal False
        ]


pageAfterDeleteTests : Test
pageAfterDeleteTests =
    describe "pageAfterDelete"
        [ test "steps back when the last item on the last page was deleted" <|
            \_ ->
                Internal.pageAfterDelete
                    (ActionResult.Success (paginationOf { number = 2, size = 20, totalElements = 41, totalPages = 3 }))
                    (onPage 3)
                    |> .page
                    |> Expect.equal (Just 2)
        , test "stays when other items remain on the last page" <|
            \_ ->
                Internal.pageAfterDelete
                    (ActionResult.Success (paginationOf { number = 2, size = 20, totalElements = 45, totalPages = 3 }))
                    (onPage 3)
                    |> .page
                    |> Expect.equal (Just 3)
        , test "stays on the first page" <|
            \_ ->
                Internal.pageAfterDelete
                    (ActionResult.Success (paginationOf { number = 0, size = 20, totalElements = 1, totalPages = 1 }))
                    (onPage 1)
                    |> .page
                    |> Expect.equal (Just 1)
        , test "stays when the page is not the last" <|
            \_ ->
                Internal.pageAfterDelete
                    (ActionResult.Success (paginationOf { number = 1, size = 20, totalElements = 41, totalPages = 3 }))
                    (onPage 2)
                    |> .page
                    |> Expect.equal (Just 2)
        , test "stays when nothing was loaded" <|
            \_ ->
                Internal.pageAfterDelete ActionResult.Loading (onPage 3)
                    |> .page
                    |> Expect.equal (Just 3)
        ]


multiValueTests : Test
multiValueTests =
    describe "multi-valued filters"
        [ test "values are split on commas" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "state", Just "draft,published" ) ]
                    |> Internal.values "state"
                    |> Expect.equal [ "draft", "published" ]
        , test "an absent or empty filter has no values" <|
            \_ ->
                [ Internal.values "state" PaginationQueryFilters.empty
                , Internal.values "state" (PaginationQueryFilters.fromValues [ ( "state", Just "" ) ])
                ]
                    |> Expect.equal [ [], [] ]
        , test "toggling adds at the end, joined with commas" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "state", Just "draft" ) ]
                    |> Internal.toggleValue "state" "published"
                    |> PaginationQueryFilters.getValue "state"
                    |> Expect.equal (Just "draft,published")
        , test "toggling removes a present value" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "state", Just "draft,published,outdated" ) ]
                    |> Internal.toggleValue "state" "published"
                    |> PaginationQueryFilters.getValue "state"
                    |> Expect.equal (Just "draft,outdated")
        , test "removing the last value removes the filter" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "state", Just "draft" ) ]
                    |> Internal.toggleValue "state" "draft"
                    |> PaginationQueryFilters.isFilterActive "state"
                    |> Expect.equal False
        , test "values are not duplicated" <|
            \_ ->
                PaginationQueryFilters.empty
                    |> Internal.setValues "state" [ "a", "b", "a", "" ]
                    |> PaginationQueryFilters.getValue "state"
                    |> Expect.equal (Just "a,b")
        ]


operatorTests : Test
operatorTests =
    describe "search filter operators"
        [ test "OR when the query string says nothing" <|
            \_ ->
                Internal.operator "users" PaginationQueryFilters.empty
                    |> Expect.equal OR
        , test "an operator is stored next to the values" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "users", Just "u1,u2" ) ]
                    |> Internal.setOperator "users" AND
                    |> (\filters -> ( Internal.operator "users" filters, PaginationQueryFilters.toList filters ))
                    |> Expect.equal ( AND, [ ( "users", "u1,u2" ), ( "users", "AND" ) ] )
        , test "the operator becomes the Op query parameter" <|
            \_ ->
                PaginationQueryFilters.empty
                    |> Internal.setOperator "users" AND
                    |> PaginationQueryFilters.getOp "users"
                    |> FilterOperator.toUrlParam "users"
                    |> Expect.equal ( "usersOp", Just "AND" )
        , test "removing the last value removes the operator too" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "users", Just "u1" ) ]
                    |> Internal.setOperator "users" AND
                    |> Internal.toggleValue "users" "u1"
                    |> PaginationQueryFilters.getOp "users"
                    |> Expect.equal Nothing
        , test "removing one of several values keeps the operator" <|
            \_ ->
                PaginationQueryFilters.fromValues [ ( "users", Just "u1,u2" ) ]
                    |> Internal.setOperator "users" AND
                    |> Internal.toggleValue "users" "u1"
                    |> PaginationQueryFilters.getOp "users"
                    |> Expect.equal (Just AND)
        ]


statusRangeTests : Test
statusRangeTests =
    describe "range"
        [ test "a middle page" <|
            \_ ->
                Internal.range { number = 1, size = 20, totalElements = 134, totalPages = 7 } 20
                    |> Expect.equal { from = 21, to = 40, total = 134 }
        , test "a short last page" <|
            \_ ->
                Internal.range { number = 6, size = 20, totalElements = 134, totalPages = 7 } 14
                    |> Expect.equal { from = 121, to = 134, total = 134 }
        ]
