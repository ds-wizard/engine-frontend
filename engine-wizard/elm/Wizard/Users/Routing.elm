module Wizard.Users.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Dict
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils exposing (dictFromMaybeList)
import Url.Parser exposing ((</>), Parser, map, s, string)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Users.Routes exposing (Route(..), indexRouteRoleFilterId)


moduleRoot : String
moduleRoot =
    "users"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrappedIndexRoute pqs q =
            wrapRoute <| IndexRoute pqs q
    in
    [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
    , map (wrapRoute << EditRoute) (s moduleRoot </> s "edit" </> string)
    , map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "lastName")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string indexRouteRoleFilterId))
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute ->
            [ moduleRoot, "create" ]

        EditRoute uuid ->
            [ moduleRoot, "edit", uuid ]

        IndexRoute paginationQueryString mbRole ->
            let
                params =
                    Dict.toList <| dictFromMaybeList [ ( indexRouteRoleFilterId, mbRole ) ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        CreateRoute ->
            Feature.usersCreate appState

        EditRoute uuid ->
            Feature.userEdit appState uuid

        IndexRoute _ _ ->
            Feature.usersView appState
