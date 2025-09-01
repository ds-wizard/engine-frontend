module Wizard.Pages.Settings.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Authentication.View
import Wizard.Pages.Settings.DashboardAndLoginScreen.View
import Wizard.Pages.Settings.Features.View
import Wizard.Pages.Settings.KnowledgeModels.View
import Wizard.Pages.Settings.LookAndFeel.View
import Wizard.Pages.Settings.Models exposing (Model)
import Wizard.Pages.Settings.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Organization.View
import Wizard.Pages.Settings.PrivacyAndSupport.View
import Wizard.Pages.Settings.Projects.View
import Wizard.Pages.Settings.Registry.View
import Wizard.Pages.Settings.Routes exposing (Route(..))
import Wizard.Pages.Settings.Submission.View
import Wizard.Pages.Settings.Usage.View
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (settingsClass)


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    let
        content =
            case route of
                OrganizationRoute ->
                    Html.map OrganizationMsg <|
                        Wizard.Pages.Settings.Organization.View.view appState model.organizationModel

                AuthenticationRoute ->
                    Html.map AuthenticationMsg <|
                        Wizard.Pages.Settings.Authentication.View.view appState model.authenticationModel

                PrivacyAndSupportRoute ->
                    Html.map PrivacyAndSupportMsg <|
                        Wizard.Pages.Settings.PrivacyAndSupport.View.view appState model.privacyAndSupportModel

                FeaturesRoute ->
                    Html.map FeaturesMsg <|
                        Wizard.Pages.Settings.Features.View.view appState model.featuresModel

                DashboardAndLoginScreenRoute ->
                    Html.map DashboardMsg <|
                        Wizard.Pages.Settings.DashboardAndLoginScreen.View.view appState model.dashboardModel

                LookAndFeelRoute ->
                    Html.map LookAndFeelMsg <|
                        Wizard.Pages.Settings.LookAndFeel.View.view appState model.lookAndFeelModel

                RegistryRoute ->
                    Html.map RegistryMsg <|
                        Wizard.Pages.Settings.Registry.View.view appState model.registryModel

                ProjectsRoute ->
                    Html.map QuestionnairesMsg <|
                        Wizard.Pages.Settings.Projects.View.view appState model.questionnairesModel

                SubmissionRoute ->
                    Html.map SubmissionMsg <|
                        Wizard.Pages.Settings.Submission.View.view appState model.documentSubmissionModel

                KnowledgeModelsRoute ->
                    Html.map KnowledgeModelsMsg <|
                        Wizard.Pages.Settings.KnowledgeModels.View.view appState model.knowledgeModelsModel

                UsageRoute ->
                    Html.map UsageMsg <|
                        Wizard.Pages.Settings.Usage.View.view appState model.usageModel
    in
    div [ settingsClass "Settings" ]
        [ div [ class "Settings__navigation" ] [ navigation appState route ]
        , div [ class "Settings__content" ] [ content ]
        ]


navigation : AppState -> Route -> Html Msg
navigation appState currentRoute =
    let
        systemSettings =
            if Admin.isEnabled appState.config.admin then
                []

            else
                strong [] [ text (gettext "System Settings" appState.locale) ]
                    :: List.map (navigationLink currentRoute) (navigationSystemLinks appState)

        userInterfaceTitle =
            [ strong [] [ text (gettext "User Interface Settings" appState.locale) ] ]

        contentTitle =
            [ strong [] [ text (gettext "Content Settings" appState.locale) ] ]

        statisticsTitle =
            [ strong [] [ text (gettext "Info" appState.locale) ] ]
    in
    div [ class "nav nav-pills flex-column" ]
        (systemSettings
            ++ userInterfaceTitle
            ++ List.map (navigationLink currentRoute) (navigationUserInterfaceLinks appState)
            ++ contentTitle
            ++ List.map (navigationLink currentRoute) (navigationContentLinks appState)
            ++ statisticsTitle
            ++ List.map (navigationLink currentRoute) (navigationStatisticsLinks appState)
        )


navigationSystemLinks : AppState -> List ( Route, String )
navigationSystemLinks appState =
    [ ( OrganizationRoute, gettext "Organization" appState.locale )
    , ( AuthenticationRoute, gettext "Authentication" appState.locale )
    , ( PrivacyAndSupportRoute, gettext "Privacy & Support" appState.locale )
    , ( FeaturesRoute, gettext "Features" appState.locale )
    ]


navigationUserInterfaceLinks : AppState -> List ( Route, String )
navigationUserInterfaceLinks appState =
    let
        dashboardAndLoginScreenTitle =
            if Admin.isEnabled appState.config.admin then
                gettext "Dashboard" appState.locale

            else
                gettext "Dashboard & Login Screen" appState.locale

        lookAndFeelTitle =
            if Admin.isEnabled appState.config.admin then
                gettext "Menu" appState.locale

            else
                gettext "Look & Feel" appState.locale
    in
    [ ( DashboardAndLoginScreenRoute, dashboardAndLoginScreenTitle )
    , ( LookAndFeelRoute, lookAndFeelTitle )
    ]


navigationContentLinks : AppState -> List ( Route, String )
navigationContentLinks appState =
    let
        items =
            [ ( KnowledgeModelsRoute, gettext "Knowledge Models" appState.locale )
            , ( ProjectsRoute, gettext "Projects" appState.locale )
            , ( SubmissionRoute, gettext "Document Submission" appState.locale )
            ]
    in
    if Feature.registry appState then
        ( RegistryRoute, gettext "DSW Registry" appState.locale ) :: items

    else
        items


navigationStatisticsLinks : AppState -> List ( Route, String )
navigationStatisticsLinks appState =
    [ ( UsageRoute, gettext "Usage" appState.locale )
    ]


navigationLink : Route -> ( Route, String ) -> Html Msg
navigationLink currentRoute ( route, title ) =
    linkTo (Routes.SettingsRoute route)
        [ class "nav-link", classList [ ( "active", currentRoute == route ) ] ]
        [ text title ]
