module Wizard.Admin.Routing exposing (isAllowed, parsers, toUrl)

import Url.Parser exposing ((</>), Parser, map, s)
import Wizard.Admin.Routes exposing (Route(..))
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers _ wrapRoute =
    let
        moduleRoot =
            "admin"
    in
    [ map (wrapRoute <| OperationsRoute) (s moduleRoot </> s "operations") ]


toUrl : AppState -> Route -> List String
toUrl _ route =
    let
        moduleRoot =
            "admin"
    in
    case route of
        OperationsRoute ->
            [ moduleRoot, "operations" ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        OperationsRoute ->
            Feature.adminOperations appState
