module Wizard.Settings.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Settings.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "settings" appState
    in
    [ map (wrapRoute <| OrganizationRoute) (s moduleRoot </> s (lr "settings.organization" appState))
    , map (wrapRoute <| AuthenticationRoute) (s moduleRoot </> s (lr "settings.authentication" appState))
    , map (wrapRoute <| PrivacyAndSupportRoute) (s moduleRoot </> s (lr "settings.privacyAndSupport" appState))
    , map (wrapRoute <| DashboardRoute) (s moduleRoot </> s (lr "settings.dashboard" appState))
    , map (wrapRoute <| LookAndFeelRoute) (s moduleRoot </> s (lr "settings.lookAndFeel" appState))
    , map (wrapRoute <| RegistryRoute) (s moduleRoot </> s (lr "settings.registry" appState))
    , map (wrapRoute <| ProjectsRoute) (s moduleRoot </> s (lr "settings.projects" appState))
    , map (wrapRoute <| SubmissionRoute) (s moduleRoot </> s (lr "settings.submission" appState))
    , map (wrapRoute <| TemplateRoute) (s moduleRoot </> s (lr "settings.template" appState))
    , map (wrapRoute <| KnowledgeModelsRoute) (s moduleRoot </> s (lr "settings.knowledgeModel" appState))
    ]


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "settings" appState
    in
    case route of
        OrganizationRoute ->
            [ moduleRoot, lr "settings.organization" appState ]

        AuthenticationRoute ->
            [ moduleRoot, lr "settings.authentication" appState ]

        PrivacyAndSupportRoute ->
            [ moduleRoot, lr "settings.privacyAndSupport" appState ]

        DashboardRoute ->
            [ moduleRoot, lr "settings.dashboard" appState ]

        LookAndFeelRoute ->
            [ moduleRoot, lr "settings.lookAndFeel" appState ]

        RegistryRoute ->
            [ moduleRoot, lr "settings.registry" appState ]

        ProjectsRoute ->
            [ moduleRoot, lr "settings.projects" appState ]

        SubmissionRoute ->
            [ moduleRoot, lr "settings.submission" appState ]

        TemplateRoute ->
            [ moduleRoot, lr "settings.template" appState ]

        KnowledgeModelsRoute ->
            [ moduleRoot, lr "settings.knowledgeModel" appState ]


isAllowed : Route -> Session -> Bool
isAllowed _ session =
    Perm.hasPerm session Perm.settings
