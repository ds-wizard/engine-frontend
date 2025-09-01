module Registry.Routes exposing
    ( Route(..)
    , documentTemplateDetail
    , documentTemplates
    , forgottenToken
    , home
    , isAllowed
    , knowledgeModelDetail
    , knowledgeModels
    , localeDetail
    , locales
    , login
    , navigate
    , organizationDetail
    , parse
    , redirect
    , signup
    , toUrl
    )

import Browser.Navigation as Navigation
import Maybe.Extra as Maybe
import Registry.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Registry.Data.Session exposing (Session)
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
    | Login
    | Signup
    | SignupConfirmation String String
    | ForgottenToken
    | ForgottenTokenConfirmation String String
    | OrganizationDetail
    | NotFound
    | NotAllowed


parse : BootstrapConfig -> Url -> Route
parse config =
    Maybe.withDefault NotFound << Parser.parse (parsers config)


parsers : BootstrapConfig -> Parser (Route -> a) a
parsers config =
    let
        authRoutes =
            if config.authentication.publicRegistrationEnabled then
                [ Parser.map Login (Parser.s "login")
                , Parser.map Signup (Parser.s "signup")
                , Parser.map SignupConfirmation (Parser.s "signup" </> Parser.string </> Parser.string)
                , Parser.map ForgottenToken (Parser.s "forgotten-token")
                , Parser.map ForgottenTokenConfirmation (Parser.s "forgotten-token" </> Parser.string </> Parser.string)
                , Parser.map OrganizationDetail (Parser.s "organization")
                ]

            else
                []

        localeRoutes =
            if config.locale.enabled then
                [ Parser.map Locales (Parser.s "locales")
                , Parser.map LocalesDetail (Parser.s "locales" </> Parser.string)
                ]

            else
                []

        publicRoutes =
            [ Parser.map Home Parser.top
            , Parser.map KnowledgeModels (Parser.s "knowledge-models")
            , Parser.map KnowledgeModelsDetail (Parser.s "knowledge-models" </> Parser.string)
            , Parser.map DocumentTemplates (Parser.s "document-templates")
            , Parser.map DocumentTemplatesDetail (Parser.s "document-templates" </> Parser.string)
            ]
    in
    Parser.oneOf (publicRoutes ++ localeRoutes ++ authRoutes)


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

        Login ->
            "/login"

        Signup ->
            "/signup"

        SignupConfirmation orgId hash ->
            "/signup/" ++ orgId ++ "/" ++ hash

        ForgottenToken ->
            "/forgotten-token"

        ForgottenTokenConfirmation orgId hash ->
            "/forgotten-token/" ++ orgId ++ "/" ++ hash

        OrganizationDetail ->
            "/organization"

        _ ->
            "/"


isAllowed : { a | session : Maybe Session } -> Route -> Bool
isAllowed appState route =
    case route of
        OrganizationDetail ->
            Maybe.isJust appState.session

        _ ->
            True


navigate : Navigation.Key -> Route -> Cmd msg
navigate key =
    Navigation.pushUrl key << toUrl


redirect : Url -> Maybe Url
redirect url =
    if url.path == "/templates" then
        Just { url | path = "/document-templates" }

    else if String.startsWith "/templates/" url.path then
        Just { url | path = "/document-templates/" ++ String.dropLeft 11 url.path }

    else
        Nothing



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


login : Route
login =
    Login


signup : Route
signup =
    Signup


forgottenToken : Route
forgottenToken =
    ForgottenToken


organizationDetail : Route
organizationDetail =
    OrganizationDetail
