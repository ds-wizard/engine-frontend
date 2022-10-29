module Wizard.Settings.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Routes as Routes
import Wizard.Settings.Authentication.View
import Wizard.Settings.Dashboard.View
import Wizard.Settings.KnowledgeModels.View
import Wizard.Settings.LookAndFeel.View
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))
import Wizard.Settings.Organization.View
import Wizard.Settings.Plans.View
import Wizard.Settings.PrivacyAndSupport.View
import Wizard.Settings.Projects.View
import Wizard.Settings.Registry.View
import Wizard.Settings.Routes exposing (Route(..))
import Wizard.Settings.Submission.View
import Wizard.Settings.Template.View
import Wizard.Settings.Usage.View


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    let
        content =
            case route of
                OrganizationRoute ->
                    Html.map OrganizationMsg <|
                        Wizard.Settings.Organization.View.view appState model.organizationModel

                AuthenticationRoute ->
                    Html.map AuthenticationMsg <|
                        Wizard.Settings.Authentication.View.view appState model.authenticationModel

                PrivacyAndSupportRoute ->
                    Html.map PrivacyAndSupportMsg <|
                        Wizard.Settings.PrivacyAndSupport.View.view appState model.privacyAndSupportModel

                DashboardRoute ->
                    Html.map DashboardMsg <|
                        Wizard.Settings.Dashboard.View.view appState model.dashboardModel

                LookAndFeelRoute ->
                    Html.map LookAndFeelMsg <|
                        Wizard.Settings.LookAndFeel.View.view appState model.lookAndFeelModel

                RegistryRoute ->
                    Html.map RegistryMsg <|
                        Wizard.Settings.Registry.View.view appState model.registryModel

                ProjectsRoute ->
                    Html.map QuestionnairesMsg <|
                        Wizard.Settings.Projects.View.view appState model.questionnairesModel

                SubmissionRoute ->
                    Html.map SubmissionMsg <|
                        Wizard.Settings.Submission.View.view appState model.documentSubmissionModel

                TemplateRoute ->
                    Html.map TemplateMsg <|
                        Wizard.Settings.Template.View.view appState model.templateModel

                KnowledgeModelsRoute ->
                    Html.map KnowledgeModelsMsg <|
                        Wizard.Settings.KnowledgeModels.View.view appState model.knowledgeModelsModel

                UsageRoute ->
                    Html.map UsageMsg <|
                        Wizard.Settings.Usage.View.view appState model.usageModel

                PlansRoute ->
                    Html.map PlansMsg <|
                        Wizard.Settings.Plans.View.view appState model.plansModel
    in
    div [ class "Settings col-full" ]
        [ div [ class "Settings__navigation" ] [ navigation appState route ]
        , div [ class "Settings__content" ] [ content ]
        ]


navigation : AppState -> Route -> Html Msg
navigation appState currentRoute =
    let
        systemTitle =
            [ strong [] [ text (gettext "System Settings" appState.locale) ] ]

        userInterfaceTitle =
            [ strong [] [ text (gettext "User Interface Settings" appState.locale) ] ]

        contentTitle =
            [ strong [] [ text (gettext "Content Settings" appState.locale) ] ]

        statisticsTitle =
            [ strong [] [ text (gettext "Info" appState.locale) ] ]
    in
    div [ class "nav nav-pills flex-column" ]
        (systemTitle
            ++ List.map (navigationLink appState currentRoute) (navigationSystemLinks appState)
            ++ userInterfaceTitle
            ++ List.map (navigationLink appState currentRoute) (navigationUserInterfaceLinks appState)
            ++ contentTitle
            ++ List.map (navigationLink appState currentRoute) (navigationContentLinks appState)
            ++ statisticsTitle
            ++ List.map (navigationLink appState currentRoute) (navigationStatisticsLinks appState)
        )


navigationSystemLinks : AppState -> List ( Route, String )
navigationSystemLinks appState =
    [ ( OrganizationRoute, gettext "Organization" appState.locale )
    , ( AuthenticationRoute, gettext "Authentication" appState.locale )
    , ( PrivacyAndSupportRoute, gettext "Privacy & Support" appState.locale )
    ]


navigationUserInterfaceLinks : AppState -> List ( Route, String )
navigationUserInterfaceLinks appState =
    [ ( DashboardRoute, gettext "Dashboard" appState.locale )
    , ( LookAndFeelRoute, gettext "Look & Feel" appState.locale )
    ]


navigationContentLinks : AppState -> List ( Route, String )
navigationContentLinks appState =
    [ ( RegistryRoute, gettext "DSW Registry" appState.locale )
    , ( KnowledgeModelsRoute, gettext "Knowledge Models" appState.locale )
    , ( ProjectsRoute, gettext "Projects" appState.locale )
    , ( SubmissionRoute, gettext "Document Submission" appState.locale )
    , ( TemplateRoute, gettext "Document Templates" appState.locale )
    ]


navigationStatisticsLinks : AppState -> List ( Route, String )
navigationStatisticsLinks appState =
    let
        items =
            [ ( UsageRoute, gettext "Usage" appState.locale )
            ]
    in
    if appState.config.cloud.enabled then
        ( PlansRoute, gettext "Plans" appState.locale ) :: items

    else
        items


navigationLink : AppState -> Route -> ( Route, String ) -> Html Msg
navigationLink appState currentRoute ( route, title ) =
    linkTo appState
        (Routes.SettingsRoute route)
        [ class "nav-link", classList [ ( "active", currentRoute == route ) ] ]
        [ text title ]
