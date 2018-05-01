module Questionnaires.Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import UrlParser exposing (..)


type Route
    = Create
    | Detail String
    | Index


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute <| Create) (s "questionnaires" </> s "create")
    , map (wrapRoute << Detail) (s "questionnaires" </> s "detail" </> string)
    , map (wrapRoute <| Index) (s "questionnaires")
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create ->
            [ "questionnaires", "create" ]

        Detail uuid ->
            [ "questionnaires", "detail", uuid ]

        Index ->
            [ "questionnaires" ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Create ->
            hasPerm maybeJwt Perm.questionnaire

        Detail uuid ->
            hasPerm maybeJwt Perm.questionnaire

        Index ->
            hasPerm maybeJwt Perm.questionnaire
