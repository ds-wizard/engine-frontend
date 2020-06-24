module Shared.Data.PaginationQueryString exposing
    ( PaginationQueryString
    , SortDirection(..)
    , empty
    , toApiUrl
    , toApiUrlWith
    , toUrl
    , toUrlWith
    , withSize
    , withSort
    , wrapRoute
    )

import List.Extra as List
import Maybe.Extra as Maybe


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

        sortBy =
            case ( List.head parts, defaultSortBy ) of
                ( Just s, _ ) ->
                    Just s

                ( Nothing, Just s ) ->
                    Just s

                _ ->
                    Nothing

        sortDirection =
            List.last parts
                |> Maybe.unwrap SortASC parseSortDirection
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
