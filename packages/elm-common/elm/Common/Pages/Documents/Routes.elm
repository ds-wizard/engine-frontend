module Common.Pages.Documents.Routes exposing
    ( Route(..)
    , isAllowed
    , parsers
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Maybe.Extra as Maybe
import Url.Parser exposing ((<?>), Parser, map, s)
import Url.Parser.Query.Extensions as Query
import Uuid exposing (Uuid)


type Route
    = IndexRoute (Maybe Uuid) PaginationQueryString


moduleRoot : String
moduleRoot =
    "project-documents"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (indexRoute wrapRoute) (PaginationQueryString.parser (s moduleRoot <?> Query.uuid "projectUuid"))
    ]


indexRoute : (Route -> a) -> Maybe Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
indexRoute wrapRoute documentUuid =
    PaginationQueryString.wrapRoute (wrapRoute << IndexRoute documentUuid) (Just "createdAt,desc")


toUrl : Route -> List String
toUrl route =
    case route of
        IndexRoute projectUuid paginationQueryString ->
            let
                queryString =
                    PaginationQueryString.toUrlWith
                        [ ( "projectUuid", Maybe.unwrap "" Uuid.toString projectUuid )
                        ]
                        paginationQueryString
            in
            if String.isEmpty queryString then
                [ moduleRoot ]

            else
                [ moduleRoot, queryString ]


isAllowed : AppStateLike a -> Bool
isAllowed =
    Feature.documentsView
