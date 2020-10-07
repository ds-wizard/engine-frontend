module Wizard.KnowledgeModels.Routing exposing
    ( detail
    , isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "knowledgeModels" appState
    in
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s (lr "knowledgeModels.import" appState) <?> Query.string (lr "knowledgeModels.import.packageId" appState))
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "versions.name")) (PaginationQueryString.parser (s moduleRoot))
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

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        ImportRoute _ ->
            Perm.hasPerm session Perm.packageManagementWrite

        _ ->
            Perm.hasPerm session Perm.packageManagementRead
