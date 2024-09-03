module Wizard.KnowledgeModels.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.KnowledgeModels.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "knowledgeModels" appState

        wrapResourcePageRoute kmId resourcePageUuid =
            wrapRoute <| ResourcePageRoute kmId resourcePageUuid
    in
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s (lr "knowledgeModels.import" appState) <?> Query.string (lr "knowledgeModels.import.packageId" appState))
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    , map (project wrapRoute) (s moduleRoot </> string </> s (lr "knowledgeModels.preview" appState) <?> Query.string (lr "knowledgeModels.preview.questionUuid" appState))
    , map wrapResourcePageRoute (s moduleRoot </> string </> s "resource-pages" </> string)
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

        ResourcePageRoute kmId resourcePageUuid ->
            [ moduleRoot, kmId, "resource-pages", resourcePageUuid ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        DetailRoute _ ->
            True

        ImportRoute _ ->
            Feature.knowledgeModelsImport appState

        PreviewRoute _ _ ->
            True

        ResourcePageRoute _ _ ->
            True

        _ ->
            Feature.knowledgeModelsView appState
