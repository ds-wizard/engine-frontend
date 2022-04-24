module Wizard.Dev.Routing exposing (isAllowed, parsers, toUrl)

import Dict
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils exposing (dictFromMaybeList)
import Url.Parser exposing ((</>), Parser, map, s)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Dev.Routes exposing (Route(..), persistentCommandIndexRouteStateFilterId)


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers _ wrapRoute =
    let
        moduleRoot =
            "dev"

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


toUrl : AppState -> Route -> List String
toUrl _ route =
    let
        moduleRoot =
            "dev"
    in
    case route of
        OperationsRoute ->
            [ moduleRoot, "operations" ]

        PersistentCommandsDetail uuid ->
            [ moduleRoot, "persistent-commands", Uuid.toString uuid ]

        PersistentCommandsIndex paginationQueryString mbState ->
            let
                params =
                    Dict.toList <|
                        dictFromMaybeList
                            [ ( persistentCommandIndexRouteStateFilterId, mbState ) ]
            in
            [ moduleRoot, "persistent-commands" ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed _ appState =
    Feature.dev appState
