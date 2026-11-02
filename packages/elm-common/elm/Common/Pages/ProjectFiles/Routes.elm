module Common.Pages.ProjectFiles.Routes exposing
    ( Route(..)
    , isAllowed
    , parsers
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Url.Parser exposing (Parser, map, s)


type Route
    = IndexRoute PaginationQueryString


moduleRoot : String
moduleRoot =
    "project-files"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "createdAt,desc")) (PaginationQueryString.parser (s moduleRoot))
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : AppStateLike a -> Bool
isAllowed appState =
    Feature.projectFiles appState
