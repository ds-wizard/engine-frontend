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
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    , map (project wrapRoute) (s moduleRoot </> string </> s (lr "knowledgeModels.preview" appState) <?> Query.string (lr "knowledgeModels.preview.questionUuid" appState))
    ]


detail : (Route -> a) -> String -> a
detail wrapRoute packageId =
    wrapRoute <| DetailRoute packageId


project : (Route -> a) -> String -> Maybe String -> a
project wrapRoute packageId mbQuestionUuid =
    wrapRoute <| PreviewRoute packageId mbQuestionUuid


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

        PreviewRoute packageId mbQuestionUuid ->
            case mbQuestionUuid of
                Just uuid ->
                    [ moduleRoot, packageId, lr "knowledgeModels.preview" appState, "?" ++ lr "knowledgeModels.preview.questionUuid" appState ++ "=" ++ uuid ]

                Nothing ->
                    [ moduleRoot, packageId, lr "knowledgeModels.preview" appState ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        DetailRoute _ ->
            True

        PreviewRoute _ _ ->
            True

        ImportRoute _ ->
            Perm.hasPerm session Perm.packageManagementWrite

        _ ->
            Perm.hasPerm session Perm.packageManagementRead
