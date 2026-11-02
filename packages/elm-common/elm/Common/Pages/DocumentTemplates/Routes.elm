module Common.Pages.DocumentTemplates.Routes exposing
    ( Route(..)
    , isAllowed
    , parsers
    , toUrl
    )

import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Pages.DocumentTemplates.Detail.Routes as DocumentTemplateDetailRoute exposing (DocumentTemplateDetailRoute)
import Url.Parser exposing ((</>), (<?>), Parser, map, s)
import Url.Parser.Extensions as Parser
import Url.Parser.Query as Query
import Uuid exposing (Uuid)


type Route
    = DetailRoute Uuid DocumentTemplateDetailRoute
    | ImportRoute (Maybe String)
    | IndexRoute PaginationQueryString


moduleRoot : String
moduleRoot =
    "document-templates"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute << ImportRoute) (s moduleRoot </> s "import" <?> Query.string "documentTemplateId")
    , map (detailLocales wrapRoute) (s moduleRoot </> Parser.uuid </> s "locales")
    , map (detail wrapRoute) (s moduleRoot </> Parser.uuid)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (PaginationQueryString.parser (s moduleRoot))
    ]


detail : (Route -> a) -> Uuid -> a
detail wrapRoute templateUuid =
    wrapRoute <| DetailRoute templateUuid DocumentTemplateDetailRoute.Readme


detailLocales : (Route -> a) -> Uuid -> a
detailLocales wrapRoute templateUuid =
    wrapRoute <| DetailRoute templateUuid DocumentTemplateDetailRoute.Locales


toUrl : Route -> List String
toUrl route =
    case route of
        DetailRoute templateUuid templateDetailRoute ->
            case templateDetailRoute of
                DocumentTemplateDetailRoute.Readme ->
                    [ moduleRoot, Uuid.toString templateUuid ]

                DocumentTemplateDetailRoute.Locales ->
                    [ moduleRoot, Uuid.toString templateUuid, "locales" ]

        ImportRoute tempalteId ->
            case tempalteId of
                Just id ->
                    [ moduleRoot, "import", "?documentTemplateId=" ++ id ]

                Nothing ->
                    [ moduleRoot, "import" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]


isAllowed : Route -> AppStateLike a -> Bool
isAllowed route appState =
    case route of
        ImportRoute _ ->
            Feature.documentTemplatesImport appState

        _ ->
            Feature.documentTemplatesView appState
