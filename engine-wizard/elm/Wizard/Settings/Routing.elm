module Wizard.Settings.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
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
    , map (wrapRoute <| KnowledgeModelRegistryRoute) (s moduleRoot </> s (lr "settings.knowledgeModelRegistry" appState))
    , map (wrapRoute <| QuestionnairesRoute) (s moduleRoot </> s (lr "settings.questionnaires" appState))
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

        KnowledgeModelRegistryRoute ->
            [ moduleRoot, lr "settings.knowledgeModelRegistry" appState ]

        QuestionnairesRoute ->
            [ moduleRoot, lr "settings.questionnaires" appState ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed _ maybeJwt =
    hasPerm maybeJwt Perm.settings
