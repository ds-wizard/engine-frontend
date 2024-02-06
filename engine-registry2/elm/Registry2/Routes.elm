module Registry2.Routes exposing
    ( Route(..)
    , documentTemplateDetail
    , documentTemplates
    , home
    , isAllowed
    , knowledgeModelDetail
    , knowledgeModels
    , localeDetail
    , locales
    , parse
    , toUrl
    )

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = Home
    | KnowledgeModels
    | KnowledgeModelsDetail String
    | DocumentTemplates
    | DocumentTemplatesDetail String
    | Locales
    | LocalesDetail String
    | NotFound
    | NotAllowed


parse : Url -> Route
parse =
    Maybe.withDefault NotFound << Parser.parse parsers


parsers : Parser (Route -> a) a
parsers =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map KnowledgeModels (Parser.s "knowledge-models")
        , Parser.map KnowledgeModelsDetail (Parser.s "knowledge-models" </> Parser.string)
        , Parser.map DocumentTemplates (Parser.s "document-templates")
        , Parser.map DocumentTemplatesDetail (Parser.s "document-templates" </> Parser.string)
        , Parser.map Locales (Parser.s "locales")
        , Parser.map LocalesDetail (Parser.s "locales" </> Parser.string)
        ]


toUrl : Route -> String
toUrl route =
    case route of
        KnowledgeModels ->
            "/knowledge-models"

        KnowledgeModelsDetail kmId ->
            "/knowledge-models/" ++ kmId

        DocumentTemplates ->
            "/document-templates"

        DocumentTemplatesDetail dtId ->
            "/document-templates/" ++ dtId

        Locales ->
            "/locales"

        LocalesDetail lId ->
            "/locales/" ++ lId

        _ ->
            "/"


isAllowed : Route -> Bool
isAllowed _ =
    True



-- Route Helpers


home : Route
home =
    Home


knowledgeModels : Route
knowledgeModels =
    KnowledgeModels


knowledgeModelDetail : String -> Route
knowledgeModelDetail kmId =
    KnowledgeModelsDetail kmId


documentTemplates : Route
documentTemplates =
    DocumentTemplates


documentTemplateDetail : String -> Route
documentTemplateDetail dtId =
    DocumentTemplatesDetail dtId


locales : Route
locales =
    Locales


localeDetail : String -> Route
localeDetail lId =
    LocalesDetail lId
