module Shared.Data.PaginationQueryString exposing
    ( PaginationQueryString
    , SortDirection(..)
    , empty
    , filterParams
    , fromQ
    , parser
    , parser1
    , parser2
    , parser3
    , parser5
    , toApiUrl
    , toApiUrlWith
    , toUrl
    , toUrlWith
    , withSize
    , withSort
    , wrapRoute
    , wrapRoute1
    , wrapRoute2
    , wrapRoute3
    , wrapRoute5
    )

import List.Extra as List
import Maybe.Extra as Maybe
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias PaginationQueryString =
    { page : Maybe Int
    , q : Maybe String
    , sortBy : Maybe String
    , sortDirection : SortDirection
    , size : Maybe Int
    }


type SortDirection
    = SortASC
    | SortDESC


empty : PaginationQueryString
empty =
    PaginationQueryString Nothing Nothing Nothing SortASC (Just defaultPageSize)


fromQ : String -> PaginationQueryString
fromQ q =
    { empty | q = Just q }


withSize : Maybe Int -> PaginationQueryString -> PaginationQueryString
withSize size qs =
    { qs | size = size }


withSort : Maybe String -> SortDirection -> PaginationQueryString -> PaginationQueryString
withSort sortBy sortDirection qs =
    { qs | sortBy = sortBy, sortDirection = sortDirection }


defaultPageSize : Int
defaultPageSize =
    20


wrapRoute : (PaginationQueryString -> a) -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> a
wrapRoute route defaultSortBy page q sort =
    let
        ( sortBy, sortDirection ) =
            parseSort defaultSortBy sort
    in
    route <| PaginationQueryString page q sortBy sortDirection (Just defaultPageSize)


wrapRoute1 : (PaginationQueryString -> b -> a) -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> (b -> a)
wrapRoute1 route defaultSortBy page q sort =
    let
        ( sortBy, sortDirection ) =
            parseSort defaultSortBy sort
    in
    route (PaginationQueryString page q sortBy sortDirection (Just defaultPageSize))


wrapRoute2 : (PaginationQueryString -> c -> b -> a) -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> (c -> b -> a)
wrapRoute2 route defaultSortBy page q sort =
    let
        ( sortBy, sortDirection ) =
            parseSort defaultSortBy sort
    in
    route (PaginationQueryString page q sortBy sortDirection (Just defaultPageSize))


wrapRoute3 : (PaginationQueryString -> d -> c -> b -> a) -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> (d -> c -> b -> a)
wrapRoute3 route defaultSortBy page q sort =
    let
        ( sortBy, sortDirection ) =
            parseSort defaultSortBy sort
    in
    route (PaginationQueryString page q sortBy sortDirection (Just defaultPageSize))


wrapRoute5 : (PaginationQueryString -> f -> e -> d -> c -> b -> a) -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> (f -> e -> d -> c -> b -> a)
wrapRoute5 route defaultSortBy page q sort =
    let
        ( sortBy, sortDirection ) =
            parseSort defaultSortBy sort
    in
    route (PaginationQueryString page q sortBy sortDirection (Just defaultPageSize))


parser : Parser a (Maybe Int -> Maybe String -> Maybe String -> b) -> Parser a b
parser p =
    p <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort"


parser1 : Parser a (Maybe Int -> Maybe String -> Maybe String -> c -> b) -> Query.Parser c -> Parser a b
parser1 p qs =
    p <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort" <?> qs


parser2 : Parser a (Maybe Int -> Maybe String -> Maybe String -> d -> c -> b) -> Query.Parser d -> Query.Parser c -> Parser a b
parser2 p qs1 qs2 =
    p <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort" <?> qs1 <?> qs2


parser3 : Parser a (Maybe Int -> Maybe String -> Maybe String -> e -> d -> c -> b) -> Query.Parser e -> Query.Parser d -> Query.Parser c -> Parser a b
parser3 p qs1 qs2 qs3 =
    p <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort" <?> qs1 <?> qs2 <?> qs3


parser5 : Parser a (Maybe Int -> Maybe String -> Maybe String -> g -> f -> e -> d -> c -> b) -> Query.Parser g -> Query.Parser f -> Query.Parser e -> Query.Parser d -> Query.Parser c -> Parser a b
parser5 p qs1 qs2 qs3 qs4 qs5 =
    p <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort" <?> qs1 <?> qs2 <?> qs3 <?> qs4 <?> qs5


toUrl : PaginationQueryString -> String
toUrl =
    toUrlWith []


toUrlWith : List ( String, String ) -> PaginationQueryString -> String
toUrlWith extraParams qs =
    let
        sortQuery sortBy =
            sortBy ++ "," ++ sortDirectionToString qs.sortDirection

        params =
            [ ( "page", Maybe.unwrap "" String.fromInt qs.page )
            , ( "q", Maybe.withDefault "" qs.q )
            , ( "sort", Maybe.unwrap "" sortQuery qs.sortBy )
            ]
                ++ extraParams
    in
    toQueryString params


toApiUrl : PaginationQueryString -> String
toApiUrl =
    toApiUrlWith []


toApiUrlWith : List ( String, String ) -> PaginationQueryString -> String
toApiUrlWith extraParams qs =
    let
        sortQuery sortBy =
            sortBy ++ "," ++ sortDirectionToString qs.sortDirection

        params =
            [ ( "page", Maybe.unwrap "0" (\p -> String.fromInt (p - 1)) qs.page )
            , ( "q", Maybe.withDefault "" qs.q )
            , ( "sort", Maybe.unwrap "" sortQuery qs.sortBy )
            , ( "size", Maybe.unwrap "" String.fromInt qs.size )
            ]
                ++ extraParams
    in
    toQueryString params


filterParams : List ( String, Maybe String ) -> List ( String, String )
filterParams params =
    let
        fold ( param, mbValue ) acc =
            case mbValue of
                Just value ->
                    acc ++ [ ( param, value ) ]

                Nothing ->
                    acc
    in
    List.foldl fold [] params


toQueryString : List ( String, String ) -> String
toQueryString params =
    let
        queryString =
            params
                |> List.filter (\( _, v ) -> not (String.isEmpty v))
                |> List.map (\( k, v ) -> k ++ "=" ++ v)
                |> String.join "&"
    in
    if String.length queryString > 0 then
        "?" ++ queryString

    else
        ""


parseSort : Maybe String -> Maybe String -> ( Maybe String, SortDirection )
parseSort defaultSortBy mbSort =
    let
        parts =
            Maybe.unwrap [] (String.split ",") mbSort

        defaultParts =
            Maybe.unwrap [] (String.split ",") defaultSortBy

        sortBy =
            case ( List.head parts, List.head defaultParts ) of
                ( Just s, _ ) ->
                    Just s

                ( Nothing, Just s ) ->
                    Just s

                _ ->
                    Nothing

        sortDirection =
            case
                ( Maybe.map parseSortDirection (List.last parts)
                , Maybe.map parseSortDirection (List.last defaultParts)
                )
            of
                ( Just d, _ ) ->
                    d

                ( Nothing, Just d ) ->
                    d

                _ ->
                    SortASC
    in
    ( sortBy, sortDirection )


parseSortDirection : String -> SortDirection
parseSortDirection direction =
    if direction == "desc" then
        SortDESC

    else
        SortASC


sortDirectionToString : SortDirection -> String
sortDirectionToString sortDirection =
    if sortDirection == SortASC then
        "asc"

    else
        "desc"
