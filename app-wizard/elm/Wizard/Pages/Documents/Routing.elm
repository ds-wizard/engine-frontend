module Wizard.Pages.Documents.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Maybe.Extra as Maybe
import Url.Parser exposing ((<?>), Parser, map, s)
import Url.Parser.Query.Extensions as Query
import Uuid exposing (Uuid)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Documents.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


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
