module Wizard.Pages.DocumentTemplates.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extensions as Parser
import Url.Parser.Query as Query
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplates.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "document-templates"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s "import" <?> Query.string "documentTemplateId")
    , map (wrapRoute << DetailRoute) (s moduleRoot </> Parser.uuid)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    ]


toUrl : Route -> List String
toUrl route =
    case route of
        DetailRoute templateUuid ->
            [ moduleRoot, Uuid.toString templateUuid ]

        ImportRoute tempalteId ->
            case tempalteId of
                Just id ->
                    [ moduleRoot, "import", "?documentTemplateId=" ++ id ]

                Nothing ->
                    [ moduleRoot, "import" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        ImportRoute _ ->
            Feature.documentTemplatesImport appState

        _ ->
            Feature.documentTemplatesView appState
