module Common.Data.PaginationQueryFilters.FilterOperator exposing
    ( FilterOperator(..)
    , decoder
    , encode
    , queryParser
    , toString
    , toUrlParam
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url.Parser.Query as Query


type FilterOperator
    = AND
    | OR


decoder : Decoder FilterOperator
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just op ->
                        D.succeed op

                    Nothing ->
                        D.fail ("Invalid FilterOperator: " ++ str)
            )


encode : FilterOperator -> E.Value
encode =
    E.string << toString


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
