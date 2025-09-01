module Wizard.Pages.Dev.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dev.Routes exposing (Route(..), persistentCommandIndexRouteStateFilterId)
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "dev"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedPersistentCommandsIndexRoute pqs mbState =
            wrapRoute <| PersistentCommandsIndex pqs mbState

        persistentCommandsIndexRouteParser =
            PaginationQueryString.parser1 (s moduleRoot </> s "persistent-commands")
                (Query.string persistentCommandIndexRouteStateFilterId)
    in
    [ map (wrapRoute <| OperationsRoute) (s moduleRoot </> s "operations")
    , map (wrapRoute << PersistentCommandsDetail) (s moduleRoot </> s "persistent-commands" </> uuid)
    , map (PaginationQueryString.wrapRoute1 wrappedPersistentCommandsIndexRoute (Just "createdAt,desc")) persistentCommandsIndexRouteParser
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        OperationsRoute ->
            [ moduleRoot, "operations" ]

        PersistentCommandsDetail uuid ->
            [ moduleRoot, "persistent-commands", Uuid.toString uuid ]

        PersistentCommandsIndex paginationQueryString mbState ->
            let
                params =
                    PaginationQueryString.filterParams [ ( persistentCommandIndexRouteStateFilterId, mbState ) ]
            in
            [ moduleRoot, "persistent-commands" ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed _ appState =
    Feature.dev appState
