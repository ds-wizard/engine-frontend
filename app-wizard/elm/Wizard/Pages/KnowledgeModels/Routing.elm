module Wizard.Pages.KnowledgeModels.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Extensions as Parser
import Url.Parser.Query as Query
import Uuid exposing (Uuid)
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
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s "import" <?> Query.string "knowledgeModelPackageId")
    , map (detail wrapRoute) (s moduleRoot </> Parser.uuid)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    , map (preview wrapRoute) (s moduleRoot </> Parser.uuid </> s "preview" <?> Query.string "questionUuid")
    , map wrapResourcePageRoute (s moduleRoot </> Parser.uuid </> s "resource-pages" </> string)
    ]


detail : (Route -> a) -> Uuid -> a
detail wrapRoute kmPackageUuid =
    wrapRoute <| DetailRoute kmPackageUuid


preview : (Route -> a) -> Uuid -> Maybe String -> a
preview wrapRoute kmPackageUuid mbQuestionUuid =
    wrapRoute <| PreviewRoute kmPackageUuid mbQuestionUuid


toUrl : Route -> List String
toUrl route =
    case route of
        DetailRoute kmPackageUuid ->
            [ moduleRoot, Uuid.toString kmPackageUuid ]

        ImportRoute kmPackageId ->
            case kmPackageId of
                Just id ->
                    [ moduleRoot, "import", "?knowledgeModelPackageId=" ++ id ]

                Nothing ->
                    [ moduleRoot, "import" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        PreviewRoute kmPackageUuid mbQuestionUuid ->
            case mbQuestionUuid of
                Just uuid ->
                    [ moduleRoot, Uuid.toString kmPackageUuid, "preview", "?questionUuid=" ++ uuid ]

                Nothing ->
                    [ moduleRoot, Uuid.toString kmPackageUuid, "preview" ]

        ResourcePageRoute kmUuid resourcePageUuid ->
            [ moduleRoot, Uuid.toString kmUuid, "resource-pages", resourcePageUuid ]


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
