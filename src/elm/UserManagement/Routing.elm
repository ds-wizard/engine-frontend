module UserManagement.Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import UrlParser exposing (..)


type Route
    = Create
    | Edit String
    | Index


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute <| Create) (s "user-management" </> s "create")
    , map (wrapRoute << Edit) (s "user-management" </> s "edit" </> string)
    , map (wrapRoute <| Index) (s "user-management")
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create ->
            [ "user-management", "create" ]

        Edit uuid ->
            [ "user-management", "edit", uuid ]

        Index ->
            [ "user-management" ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Create ->
            hasPerm maybeJwt Perm.userManagement

        Edit uuid ->
            if uuid == "current" then
                True
            else
                hasPerm maybeJwt Perm.userManagement

        Index ->
            hasPerm maybeJwt Perm.userManagement
