module Wizard.KnowledgeModels.Routing exposing
    ( detail
    , isAllowed
    , parsers
    , toUrl
    )

import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Common.Locale exposing (lr)
import Wizard.KnowledgeModels.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "knowledgeModels" appState
    in
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s (lr "knowledgeModels.import" appState) <?> Query.string (lr "knowledgeModels.import.packageId" appState))
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    ]


detail : (Route -> a) -> String -> a
detail wrapRoute packageId =
    DetailRoute packageId |> wrapRoute


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "knowledgeModels" appState
    in
    case route of
        DetailRoute packageId ->
            [ moduleRoot, packageId ]

        ImportRoute packageId ->
            case packageId of
                Just id ->
                    [ moduleRoot, lr "knowledgeModels.import" appState, "?" ++ lr "knowledgeModels.import.packageId" appState ++ "=" ++ id ]

                Nothing ->
                    [ moduleRoot, lr "knowledgeModels.import" appState ]

        IndexRoute ->
            [ moduleRoot ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        ImportRoute _ ->
            hasPerm maybeJwt Perm.packageManagementWrite

        _ ->
            hasPerm maybeJwt Perm.packageManagementRead
