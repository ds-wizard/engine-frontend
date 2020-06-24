module Wizard.Users.Routing exposing
    ( isAllowed
    , moduleRoot
    , parses
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Url.Parser exposing (..)
import Wizard.Users.Routes exposing (Route(..))


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
