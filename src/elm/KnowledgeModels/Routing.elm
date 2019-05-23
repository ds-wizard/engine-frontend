module KnowledgeModels.Routing exposing (Route(..), detail, isAllowed, moduleRoot, parsers, toUrl)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


type Route
    = Detail String
    | Import (Maybe String)
    | Index


moduleRoot : String
moduleRoot =
    "knowledge-models"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << Import) (s moduleRoot </> s "import" <?> Query.string "packageId")
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

        Import packageId ->
            case packageId of
                Just id ->
                    [ moduleRoot, "import", "?packageId=" ++ id ]

                Nothing ->
                    [ moduleRoot, "import" ]

        Index ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Import _ ->
            hasPerm maybeJwt Perm.packageManagementWrite

        _ ->
            hasPerm maybeJwt Perm.packageManagementRead
