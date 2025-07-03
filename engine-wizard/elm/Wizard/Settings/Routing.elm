module Wizard.Settings.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Utils exposing (listInsertIf)
import Url.Parser exposing ((</>), Parser, map, s)
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Settings.Routes exposing (Route(..))


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
        |> listInsertIf (map (wrapRoute <| OrganizationRoute) (s moduleRoot </> s "organization")) adminDisabled
        |> listInsertIf (map (wrapRoute <| AuthenticationRoute) (s moduleRoot </> s "authentication")) adminDisabled
        |> listInsertIf (map (wrapRoute <| PrivacyAndSupportRoute) (s moduleRoot </> s "privacy-and-support")) adminDisabled
        |> listInsertIf (map (wrapRoute <| DashboardAndLoginScreenRoute) (s moduleRoot </> s "dashboard")) True
        |> listInsertIf (map (wrapRoute <| LookAndFeelRoute) (s moduleRoot </> s "look-and-feel")) True
        |> listInsertIf (map (wrapRoute <| RegistryRoute) (s moduleRoot </> s "registry")) (Feature.registry appState)
        |> listInsertIf (map (wrapRoute <| ProjectsRoute) (s moduleRoot </> s "projects")) True
        |> listInsertIf (map (wrapRoute <| SubmissionRoute) (s moduleRoot </> s "submission")) True
        |> listInsertIf (map (wrapRoute <| KnowledgeModelsRoute) (s moduleRoot </> s "knowledge-models")) True
        |> listInsertIf (map (wrapRoute <| UsageRoute) (s moduleRoot </> s "usage")) True


toUrl : Route -> List String
toUrl route =
    case route of
        OrganizationRoute ->
            [ moduleRoot, "organization" ]

        AuthenticationRoute ->
            [ moduleRoot, "authentication" ]

        PrivacyAndSupportRoute ->
            [ moduleRoot, "privacy-and-support" ]

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
