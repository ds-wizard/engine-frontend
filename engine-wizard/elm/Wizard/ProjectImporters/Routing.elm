module Wizard.ProjectImporters.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing (Parser, map, s)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.ProjectImporters.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "projectImporters" appState
    in
    [ map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    ]


toUrl : AppState -> Route -> List String
toUrl appState route =
    case route of
        IndexRoute paginationQueryString ->
            let
                moduleRoot =
                    lr "projectImporters" appState
            in
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : AppState -> Bool
isAllowed appState =
    Feature.projectImporters appState
