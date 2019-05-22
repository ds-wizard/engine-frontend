module KnowledgeModels.Routing exposing (Route(..), detail, isAllowed, moduleRoot, parsers, toUrl)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)


type Route
    = Detail String
    | Import
    | Index


moduleRoot : String
moduleRoot =
    "knowledge-models"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute <| Import) (s moduleRoot </> s "import")
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (wrapRoute <| Index) (s moduleRoot)
    ]


detail : (Route -> a) -> String -> a
detail wrapRoute packageId =
    Detail packageId |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        Detail packageId ->
            [ moduleRoot, packageId ]

        Import ->
            [ moduleRoot, "import" ]

        Index ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Import ->
            hasPerm maybeJwt Perm.packageManagementWrite

        _ ->
            hasPerm maybeJwt Perm.packageManagementRead
