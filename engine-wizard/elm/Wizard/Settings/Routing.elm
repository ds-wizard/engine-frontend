module Wizard.Settings.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Locale exposing (lr)
import Shared.Utils exposing (listInsertIf)
import Url.Parser exposing ((</>), Parser, map, s)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Settings.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "settings" appState

        adminDisabled =
            not (Admin.isEnabled appState.config.admin)
    in
    []
        |> listInsertIf (map (wrapRoute <| OrganizationRoute) (s moduleRoot </> s (lr "settings.organization" appState))) True
        |> listInsertIf (map (wrapRoute <| AuthenticationRoute) (s moduleRoot </> s (lr "settings.authentication" appState))) adminDisabled
        |> listInsertIf (map (wrapRoute <| PrivacyAndSupportRoute) (s moduleRoot </> s (lr "settings.privacyAndSupport" appState))) adminDisabled
        |> listInsertIf (map (wrapRoute <| DashboardAndLoginScreenRoute) (s moduleRoot </> s (lr "settings.dashboard" appState))) True
        |> listInsertIf (map (wrapRoute <| LookAndFeelRoute) (s moduleRoot </> s (lr "settings.lookAndFeel" appState))) True
        |> listInsertIf (map (wrapRoute <| RegistryRoute) (s moduleRoot </> s (lr "settings.registry" appState))) (Feature.registry appState)
        |> listInsertIf (map (wrapRoute <| ProjectsRoute) (s moduleRoot </> s (lr "settings.projects" appState))) True
        |> listInsertIf (map (wrapRoute <| SubmissionRoute) (s moduleRoot </> s (lr "settings.submission" appState))) True
        |> listInsertIf (map (wrapRoute <| KnowledgeModelsRoute) (s moduleRoot </> s (lr "settings.knowledgeModel" appState))) True
        |> listInsertIf (map (wrapRoute <| UsageRoute) (s moduleRoot </> s (lr "settings.usage" appState))) True
        |> listInsertIf (map (wrapRoute <| PlansRoute) (s moduleRoot </> s (lr "settings.plans" appState))) (Feature.plans appState)


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

        DashboardAndLoginScreenRoute ->
            [ moduleRoot, lr "settings.dashboard" appState ]

        LookAndFeelRoute ->
            [ moduleRoot, lr "settings.lookAndFeel" appState ]

        RegistryRoute ->
            [ moduleRoot, lr "settings.registry" appState ]

        ProjectsRoute ->
            [ moduleRoot, lr "settings.projects" appState ]

        SubmissionRoute ->
            [ moduleRoot, lr "settings.submission" appState ]

        KnowledgeModelsRoute ->
            [ moduleRoot, lr "settings.knowledgeModel" appState ]

        UsageRoute ->
            [ moduleRoot, lr "settings.usage" appState ]

        PlansRoute ->
            [ moduleRoot, lr "settings.plans" appState ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        RegistryRoute ->
            Feature.settings appState && Feature.registry appState

        PlansRoute ->
            Feature.settings appState && Feature.plans appState

        _ ->
            Feature.settings appState
