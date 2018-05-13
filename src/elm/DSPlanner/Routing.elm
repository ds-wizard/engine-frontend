module DSPlanner.Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import UrlParser exposing (..)


type Route
    = Create
    | Detail String
    | Index


moduleRoot : String
moduleRoot =
    "ds-planner"


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute <| Create) (s moduleRoot </> s "create")
    , map (wrapRoute << Detail) (s moduleRoot </> s "detail" </> string)
    , map (wrapRoute <| Index) (s moduleRoot)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create ->
            [ moduleRoot, "create" ]

        Detail uuid ->
            [ moduleRoot, "detail", uuid ]

        Index ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    hasPerm maybeJwt Perm.questionnaire
