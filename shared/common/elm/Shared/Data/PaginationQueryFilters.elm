module Shared.Data.PaginationQueryFilters exposing
    ( PaginationQueryFilters
    , create
    , empty
    , fromValues
    , getOp
    , getValue
    , insertOp
    , insertValue
    , isFilterActive
    , removeFilter
    )

import Dict exposing (Dict)
import Shared.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Shared.Utils exposing (dictFromMaybeList, flip)



-- filters -> values


type alias PaginationQueryFilters =
    { values : Dict String String
    , operators : Dict String FilterOperator
    }


empty : PaginationQueryFilters
empty =
    { values = Dict.empty
    , operators = Dict.empty
    }


create : List ( String, Maybe String ) -> List ( String, Maybe FilterOperator ) -> PaginationQueryFilters
create mbFilters mbOperators =
    { values = dictFromMaybeList mbFilters
    , operators = dictFromMaybeList mbOperators
    }


fromValues : List ( String, Maybe String ) -> PaginationQueryFilters
fromValues =
    flip create []


removeFilter : String -> PaginationQueryFilters -> PaginationQueryFilters
removeFilter filterId pqf =
    pqf
        |> removeValue filterId
        |> removeOp filterId


insertValue : String -> String -> PaginationQueryFilters -> PaginationQueryFilters
insertValue filterId value pqf =
    { pqf | values = Dict.insert filterId value pqf.values }


removeValue : String -> PaginationQueryFilters -> PaginationQueryFilters
removeValue filterId pqf =
    { pqf | values = Dict.remove filterId pqf.values }


getValue : String -> PaginationQueryFilters -> Maybe String
getValue filterId pqf =
    Dict.get filterId pqf.values


isFilterActive : String -> PaginationQueryFilters -> Bool
isFilterActive filterId pqf =
    case Dict.get filterId pqf.values of
        Just value ->
            not (String.isEmpty value)

        Nothing ->
            False


insertOp : String -> FilterOperator -> PaginationQueryFilters -> PaginationQueryFilters
insertOp filterId op pqf =
    { pqf | operators = Dict.insert filterId op pqf.operators }


removeOp : String -> PaginationQueryFilters -> PaginationQueryFilters
removeOp filterId pqf =
    { pqf | operators = Dict.remove filterId pqf.operators }


getOp : String -> PaginationQueryFilters -> Maybe FilterOperator
getOp filterId pqf =
    Dict.get filterId pqf.operators
