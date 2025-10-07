module Wizard.Pages.KnowledgeModels.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Query as Query
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "knowledge-models"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    let
        wrapResourcePageRoute kmId resourcePageUuid =
            wrapRoute <| ResourcePageRoute kmId resourcePageUuid
    in
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s "import" <?> Query.string "packageId")
    , map (detail wrapRoute) (s moduleRoot </> string)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    , map (project wrapRoute) (s moduleRoot </> string </> s "preview" <?> Query.string "questionUuid")
    , map wrapResourcePageRoute (s moduleRoot </> string </> s "resource-pages" </> string)
    ]


detail : (Route -> a) -> String -> a
detail wrapRoute packageId =
    wrapRoute <| DetailRoute packageId


project : (Route -> a) -> String -> Maybe String -> a
project wrapRoute packageId mbQuestionUuid =
    wrapRoute <| PreviewRoute packageId mbQuestionUuid


toUrl : Route -> List String
toUrl route =
    case route of
        DetailRoute packageId ->
            [ moduleRoot, packageId ]

        ImportRoute packageId ->
            case packageId of
                Just id ->
                    [ moduleRoot, "import", "?" ++ "packageId" ++ "=" ++ id ]

                Nothing ->
                    [ moduleRoot, "import" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        PreviewRoute packageId mbQuestionUuid ->
            case mbQuestionUuid of
                Just uuid ->
                    [ moduleRoot, packageId, "preview", "?" ++ "questionUuid" ++ "=" ++ uuid ]

                Nothing ->
                    [ moduleRoot, packageId, "preview" ]

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
