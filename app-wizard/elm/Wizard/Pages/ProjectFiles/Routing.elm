module Wizard.Pages.ProjectFiles.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing (Parser, map, s)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.ProjectFiles.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


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


isAllowed : AppState -> Bool
isAllowed appState =
    Feature.projectImporters appState
