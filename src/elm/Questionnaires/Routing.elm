module Questionnaires.Routing exposing (Route(..), isAllowed, moduleRoot, parses, toUrl)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = Create (Maybe String)
    | Detail String
    | Index


moduleRoot : String
moduleRoot =
    "questionnaires"


parses : (Route -> a) -> List (Parser (a -> c) c)
parses wrapRoute =
    [ map (wrapRoute << Create) (s moduleRoot </> s "create" <?> Query.string "selected")
    , map (wrapRoute << Detail) (s moduleRoot </> s "detail" </> string)
    , map (wrapRoute <| Index) (s moduleRoot)
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        Create selected ->
            case selected of
                Just id ->
                    [ moduleRoot, "create", "?selected=" ++ id ]

                Nothing ->
                    [ moduleRoot, "create" ]

        Detail uuid ->
            [ moduleRoot, "detail", uuid ]

        Index ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    hasPerm maybeJwt Perm.questionnaire
