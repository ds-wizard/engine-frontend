module Users.Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import UrlParser exposing (..)


type Route
    = Create
    | Edit String
    | Index


moduleRoot : String
moduleRoot =
    "users"


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute <| Create) (s moduleRoot </> s "create")
    , map (wrapRoute << Edit) (s moduleRoot </> s "edit" </> string)
    , map (wrapRoute <| Index) (s moduleRoot)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create ->
            [ moduleRoot, "create" ]

        Edit uuid ->
            [ moduleRoot, "edit", uuid ]

        Index ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Edit uuid ->
            if uuid == "current" then
                True
            else
                hasPerm maybeJwt Perm.userManagement

        _ ->
            hasPerm maybeJwt Perm.userManagement
