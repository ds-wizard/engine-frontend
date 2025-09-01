module Wizard.Pages.Settings.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import List.Utils as List
import Url.Parser exposing ((</>), Parser, map, s)
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "settings"


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        adminDisabled =
            not (Admin.isEnabled appState.config.admin)
    in
    []
        |> List.insertIf (map (wrapRoute <| OrganizationRoute) (s moduleRoot </> s "organization")) adminDisabled
        |> List.insertIf (map (wrapRoute <| AuthenticationRoute) (s moduleRoot </> s "authentication")) adminDisabled
        |> List.insertIf (map (wrapRoute <| PrivacyAndSupportRoute) (s moduleRoot </> s "privacy-and-support")) adminDisabled
        |> List.insertIf (map (wrapRoute <| FeaturesRoute) (s moduleRoot </> s "features")) adminDisabled
        |> List.insertIf (map (wrapRoute <| DashboardAndLoginScreenRoute) (s moduleRoot </> s "dashboard")) True
        |> List.insertIf (map (wrapRoute <| LookAndFeelRoute) (s moduleRoot </> s "look-and-feel")) True
        |> List.insertIf (map (wrapRoute <| RegistryRoute) (s moduleRoot </> s "registry")) (Feature.registry appState)
        |> List.insertIf (map (wrapRoute <| ProjectsRoute) (s moduleRoot </> s "projects")) True
        |> List.insertIf (map (wrapRoute <| SubmissionRoute) (s moduleRoot </> s "submission")) True
        |> List.insertIf (map (wrapRoute <| KnowledgeModelsRoute) (s moduleRoot </> s "knowledge-models")) True
        |> List.insertIf (map (wrapRoute <| UsageRoute) (s moduleRoot </> s "usage")) True


toUrl : Route -> List String
toUrl route =
    case route of
        OrganizationRoute ->
            [ moduleRoot, "organization" ]

        AuthenticationRoute ->
            [ moduleRoot, "authentication" ]

        PrivacyAndSupportRoute ->
            [ moduleRoot, "privacy-and-support" ]

        FeaturesRoute ->
            [ moduleRoot, "features" ]

        DashboardAndLoginScreenRoute ->
            [ moduleRoot, "dashboard" ]

        LookAndFeelRoute ->
            [ moduleRoot, "look-and-feel" ]

        RegistryRoute ->
            [ moduleRoot, "registry" ]

        ProjectsRoute ->
            [ moduleRoot, "projects" ]

        SubmissionRoute ->
            [ moduleRoot, "submission" ]

        KnowledgeModelsRoute ->
            [ moduleRoot, "knowledge-models" ]

        UsageRoute ->
            [ moduleRoot, "usage" ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        RegistryRoute ->
            Feature.settings appState && Feature.registry appState

        _ ->
            Feature.settings appState
