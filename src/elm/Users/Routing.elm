module Users.Routing exposing
    ( isAllowed
    , moduleRoot
    , parses
    , toUrl
    )

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)
import Users.Routes exposing (Route(..))


moduleRoot : String
moduleRoot =
    "users"


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute <| CreateRoute) (s moduleRoot </> s "create")
    , map (wrapRoute << EditRoute) (s moduleRoot </> s "edit" </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        CreateRoute ->
            [ moduleRoot, "create" ]

        EditRoute uuid ->
            [ moduleRoot, "edit", uuid ]

        IndexRoute ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        EditRoute uuid ->
            if uuid == "current" then
                True

            else
                hasPerm maybeJwt Perm.userManagement

        _ ->
            hasPerm maybeJwt Perm.userManagement
