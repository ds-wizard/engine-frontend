module Wizard.Documents.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Maybe.Extra as Maybe
import Shared.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((<?>), Parser, map, s)
import Url.Parser.Query.Extra as Query
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Documents.Routes exposing (Route(..))


moduleRoot : String
moduleRoot =
    "project-documents"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (indexRoute wrapRoute) (PaginationQueryString.parser (s moduleRoot <?> Query.uuid "questionnaireUuid"))
    ]


indexRoute : (Route -> a) -> Maybe Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
indexRoute wrapRoute documentUuid =
    PaginationQueryString.wrapRoute (wrapRoute << IndexRoute documentUuid) (Just "createdAt,desc")


toUrl : Route -> List String
toUrl route =
    case route of
        IndexRoute questionnaireUuid paginationQueryString ->
            let
                queryString =
                    PaginationQueryString.toUrlWith
                        [ ( "questionnaireUuid", Maybe.unwrap "" Uuid.toString questionnaireUuid )
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
