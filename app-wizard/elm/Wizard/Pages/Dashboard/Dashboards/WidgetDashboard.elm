module Wizard.Pages.Dashboard.Dashboards.WidgetDashboard exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import Gettext
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List.Extensions as List
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.AddOpenIDWidget as AddOpenIDWidget
import Wizard.Pages.Dashboard.Widgets.AssignedCommentsWidget as AssignedCommentsWidget
import Wizard.Pages.Dashboard.Widgets.ConfigureLookAndFeelWidget as ConfigureLookAndFeelWidget
import Wizard.Pages.Dashboard.Widgets.ConfigureOrganizationWidget as ConfigureOrganizationWidget
import Wizard.Pages.Dashboard.Widgets.ConnectRegistryWidget as ConnectRegistryWidget
import Wizard.Pages.Dashboard.Widgets.CreateKnowledgeModelWidget as CreateKnowledgeModelWidget
import Wizard.Pages.Dashboard.Widgets.CreateProjectTemplateWidget as CreateProjectTemplateWidget
import Wizard.Pages.Dashboard.Widgets.CreateProjectWidget as CreateProjectWidget
import Wizard.Pages.Dashboard.Widgets.ImportDocumentTemplateWidget as ImportDocumentTemplateWidget
import Wizard.Pages.Dashboard.Widgets.ImportKnowledgeModelWidget as ImportKnowledgeModelWidget
import Wizard.Pages.Dashboard.Widgets.OutdatedPackagesWidget as OutdatedPackagesWidget
import Wizard.Pages.Dashboard.Widgets.OutdatedTemplatesWidget as OutdatedTemplatesWidget
import Wizard.Pages.Dashboard.Widgets.RecentProjectsWidget as RecentProjectsWidget
import Wizard.Pages.Dashboard.Widgets.UsageWidget as UsageWidget
import Wizard.Pages.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { assignedCommentsWidgetModel : AssignedCommentsWidget.Model
    , outdatedPackagesWidgetModel : OutdatedPackagesWidget.Model
    , outdatedTemplatesWidgetModel : OutdatedTemplatesWidget.Model
    , recentProjectsWidgetModel : RecentProjectsWidget.Model
    , usageWidgetModel : UsageWidget.Model
    }


initialModel : Model
initialModel =
    { assignedCommentsWidgetModel = AssignedCommentsWidget.initialModel
    , outdatedPackagesWidgetModel = OutdatedPackagesWidget.initialModel
    , outdatedTemplatesWidgetModel = OutdatedTemplatesWidget.initialModel
    , recentProjectsWidgetModel = RecentProjectsWidget.initialModel
    , usageWidgetModel = UsageWidget.initialModel
    }


type Msg
    = AssignedCommentsWidgetMsg AssignedCommentsWidget.Msg
    | OutdatedPackagesWidgetMsg OutdatedPackagesWidget.Msg
    | OutdatedTemplatesWidgetMsg OutdatedTemplatesWidget.Msg
    | RecentProjectsWidgetMsg RecentProjectsWidget.Msg
    | UsageWidgetMsg UsageWidget.Msg


fetchData : AppState -> Cmd Msg
fetchData appState =
    []
        |> List.insertIf (Cmd.map AssignedCommentsWidgetMsg (AssignedCommentsWidget.fetchData appState)) True
        |> List.insertIf (Cmd.map OutdatedPackagesWidgetMsg (OutdatedPackagesWidget.fetchData appState)) (OutdatedPackagesWidget.enabled appState)
        |> List.insertIf (Cmd.map OutdatedTemplatesWidgetMsg (OutdatedTemplatesWidget.fetchData appState)) (OutdatedTemplatesWidget.enabled appState)
        |> List.insertIf (Cmd.map RecentProjectsWidgetMsg (RecentProjectsWidget.fetchData appState)) True
        |> List.insertIf (Cmd.map UsageWidgetMsg (UsageWidget.fetchData appState)) (UsageWidget.enabled appState)
        |> Cmd.batch


type alias UpdateConfig msg =
    { locale : Gettext.Locale
    , logoutMsg : msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        AssignedCommentsWidgetMsg assignedCommentsWidgetMsg ->
            let
                updateConfig =
                    { locale = cfg.locale, logoutMsg = cfg.logoutMsg }

                ( updatedAssignedCommentsWidgetModel, assignedCommentsWidgetCmd ) =
                    AssignedCommentsWidget.update updateConfig assignedCommentsWidgetMsg model.assignedCommentsWidgetModel
            in
            ( { model | assignedCommentsWidgetModel = updatedAssignedCommentsWidgetModel }, assignedCommentsWidgetCmd )

        OutdatedPackagesWidgetMsg outdatedPackagesWidgetMsg ->
            let
                updateConfig =
                    { locale = cfg.locale, logoutMsg = cfg.logoutMsg }

                ( updatedOutdatedPackagesWidgetModel, outdatedPackagesWidgetCmd ) =
                    OutdatedPackagesWidget.update updateConfig outdatedPackagesWidgetMsg model.outdatedPackagesWidgetModel
            in
            ( { model | outdatedPackagesWidgetModel = updatedOutdatedPackagesWidgetModel }, outdatedPackagesWidgetCmd )

        OutdatedTemplatesWidgetMsg outdatedTemplatesWidgetMsg ->
            let
                updateConfig =
                    { locale = cfg.locale, logoutMsg = cfg.logoutMsg }

                ( updatedOutdatedTemplatesWidgetModel, outdatedTemplatesWidgetCmd ) =
                    OutdatedTemplatesWidget.update updateConfig outdatedTemplatesWidgetMsg model.outdatedTemplatesWidgetModel
            in
            ( { model | outdatedTemplatesWidgetModel = updatedOutdatedTemplatesWidgetModel }, outdatedTemplatesWidgetCmd )

        RecentProjectsWidgetMsg recentProjectsWidgetMsg ->
            let
                updateConfig =
                    { locale = cfg.locale, logoutMsg = cfg.logoutMsg }

                ( updatedRecentProjectsWidgetModel, recentProjectsWidgetCmd ) =
                    RecentProjectsWidget.update updateConfig recentProjectsWidgetMsg model.recentProjectsWidgetModel
            in
            ( { model | recentProjectsWidgetModel = updatedRecentProjectsWidgetModel }, recentProjectsWidgetCmd )

        UsageWidgetMsg usageWidgetMsg ->
            let
                updateConfig =
                    { locale = cfg.locale, logoutMsg = cfg.logoutMsg }

                ( updatedUsageWidgetModel, usageWidgetCmd ) =
                    UsageWidget.update updateConfig usageWidgetMsg model.usageWidgetModel
            in
            ( { model | usageWidgetModel = updatedUsageWidgetModel }, usageWidgetCmd )


view : AppState -> Model -> Html Msg
view appState model =
    let
        widgets =
            []
                |> List.insertIf (Html.map AssignedCommentsWidgetMsg (AssignedCommentsWidget.view appState model.assignedCommentsWidgetModel)) True
                |> List.insertIf (Html.map RecentProjectsWidgetMsg (RecentProjectsWidget.view appState model.recentProjectsWidgetModel)) True
                |> List.insertIf (CreateProjectWidget.view appState) True
                |> List.insertIf (CreateProjectTemplateWidget.view appState) (CreateProjectTemplateWidget.enabled appState)
                |> List.insertIf (CreateKnowledgeModelWidget.view appState) (CreateKnowledgeModelWidget.enabled appState)
                |> List.insertIf (Html.map OutdatedPackagesWidgetMsg (OutdatedPackagesWidget.view appState model.outdatedPackagesWidgetModel)) (OutdatedPackagesWidget.enabled appState)
                |> List.insertIf (Html.map OutdatedTemplatesWidgetMsg (OutdatedTemplatesWidget.view appState model.outdatedTemplatesWidgetModel)) (OutdatedTemplatesWidget.enabled appState)
                |> List.insertIf (ImportKnowledgeModelWidget.view appState) (ImportKnowledgeModelWidget.enabled appState)
                |> List.insertIf (ImportDocumentTemplateWidget.view appState) (ImportDocumentTemplateWidget.enabled appState)
                |> List.insertIf (Html.map UsageWidgetMsg (UsageWidget.view appState model.usageWidgetModel)) (UsageWidget.enabled appState)
                |> List.insertIf (ConfigureLookAndFeelWidget.view appState) (ConfigureLookAndFeelWidget.enabled appState)
                |> List.insertIf (ConfigureOrganizationWidget.view appState) (ConfigureOrganizationWidget.enabled appState)
                |> List.insertIf (ConnectRegistryWidget.view appState) (ConnectRegistryWidget.enabled appState)
                |> List.insertIf (AddOpenIDWidget.view appState) (AddOpenIDWidget.enabled appState)
    in
    div []
        [ div [ class "row gx-3" ]
            (WelcomeWidget.view appState :: widgets)
        ]
