module Shared.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator(..), fromString, queryParser, toString, toUrlParam)

import Url.Parser.Query as Query


type FilterOperator
    = AND
    | OR


queryParser : String -> Query.Parser (Maybe FilterOperator)
queryParser key =
    Query.custom (toKey key) <|
        \stringList ->
            case stringList of
                [ str ] ->
                    fromString str

                _ ->
                    Nothing


toUrlParam : String -> Maybe FilterOperator -> ( String, Maybe String )
toUrlParam key mbValue =
    ( toKey key, Maybe.map toString mbValue )


fromString : String -> Maybe FilterOperator
fromString str =
    case str of
        "AND" ->
            Just AND

        "OR" ->
            Just OR

        _ ->
            Nothing


toString : FilterOperator -> String
toString op =
    case op of
        AND ->
            "AND"

        OR ->
            "OR"


toKey : String -> String
toKey key =
    key ++ "Op"
