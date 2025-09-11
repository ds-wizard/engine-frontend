module Common.Data.PaginationQueryFilters exposing
    ( PaginationQueryFilters
    , create
    , decoder
    , empty
    , encode
    , fromValues
    , getOp
    , getValue
    , insertOp
    , insertValue
    , isFilterActive
    , removeFilter
    , removeValue
    , toList
    )

import Common.Data.PaginationQueryFilters.FilterOperator as FilterOperator exposing (FilterOperator)
import Dict exposing (Dict)
import Dict.Extensions as Dict
import Flip exposing (flip)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E



-- filters -> values


type alias PaginationQueryFilters =
    { values : Dict String String
    , operators : Dict String FilterOperator
    }


decoder : Decoder PaginationQueryFilters
decoder =
    D.succeed PaginationQueryFilters
        |> D.required "values" (D.dict D.string)
        |> D.required "operators" (D.dict FilterOperator.decoder)


encode : PaginationQueryFilters -> E.Value
encode pqf =
    E.object
        [ ( "values", E.dict identity E.string pqf.values )
        , ( "operators", E.dict identity FilterOperator.encode pqf.operators )
        ]


empty : PaginationQueryFilters
empty =
    { values = Dict.empty
    , operators = Dict.empty
    }


create : List ( String, Maybe String ) -> List ( String, Maybe FilterOperator ) -> PaginationQueryFilters
create mbFilters mbOperators =
    { values = Dict.fromMaybeList mbFilters
    , operators = Dict.fromMaybeList mbOperators
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


toList : PaginationQueryFilters -> List ( String, String )
toList pqf =
    Dict.toList pqf.values ++ Dict.toList (Dict.map (always FilterOperator.toString) pqf.operators)
