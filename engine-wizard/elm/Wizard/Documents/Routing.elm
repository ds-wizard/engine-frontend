module Wizard.Documents.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Maybe.Extra as Maybe
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing ((<?>), Parser, map, s)
import Url.Parser.Query.Extra as Query
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Documents.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "documents" appState
    in
    [ map (indexRoute wrapRoute) (PaginationQueryString.parser (s moduleRoot <?> Query.uuid (lr "documents.index.questionnaireUuid" appState)))
    ]


indexRoute : (Route -> a) -> Maybe Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
indexRoute wrapRoute documentUuid =
    PaginationQueryString.wrapRoute (wrapRoute << IndexRoute documentUuid) (Just "createdAt,desc")


toUrl : AppState -> Route -> List String
toUrl appState route =
    case route of
        IndexRoute questionnaireUuid paginationQueryString ->
            let
                moduleRoot =
                    lr "documents" appState

                queryString =
                    PaginationQueryString.toUrlWith
                        [ ( lr "documents.index.questionnaireUuid" appState, Maybe.unwrap "" Uuid.toString questionnaireUuid )
                        ]
                        paginationQueryString
            in
            if String.isEmpty queryString then
                [ moduleRoot ]

            else
                [ moduleRoot, queryString ]


isAllowed : Route -> AppState -> Bool
isAllowed _ appState =
    Feature.documentsView appState
