module Wizard.Users.Routing exposing
    ( isAllowed
    , moduleRoot
    , parses
    , toUrl
    )

import Dict
import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils exposing (dictFromMaybeList)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Users.Routes exposing (Route(..), indexRouteRoleFilterId)


moduleRoot : String
moduleRoot =
    "users"


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
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


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        EditRoute uuid ->
            if uuid == "current" then
                True

            else
                Perm.hasPerm session Perm.userManagement

        _ ->
            Perm.hasPerm session Perm.userManagement
