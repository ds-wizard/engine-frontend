module Wizard.Settings.View exposing (view)

import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Routes
import Wizard.Settings.Authentication.View
import Wizard.Settings.Dashboard.View
import Wizard.Settings.LookAndFeel.View
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))
import Wizard.Settings.Organization.View
import Wizard.Settings.PrivacyAndSupport.View
import Wizard.Settings.Questionnaires.View
import Wizard.Settings.Registry.View
import Wizard.Settings.Routes exposing (Route(..))
import Wizard.Settings.Submission.View
import Wizard.Settings.Template.View


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.View"


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

                QuestionnairesRoute ->
                    Html.map QuestionnairesMsg <|
                        Wizard.Settings.Questionnaires.View.view appState model.questionnairesModel

                SubmissionRoute ->
                    Html.map SubmissionMsg <|
                        Wizard.Settings.Submission.View.view appState model.documentSubmissionModel

                TemplateRoute ->
                    Html.map TemplateMsg <|
                        Wizard.Settings.Template.View.view appState model.templateModel
    in
    div [ class "Settings" ]
        [ div [ class "Settings__navigation" ] [ navigation appState route ]
        , div [ class "Settings__content" ] [ content ]
        ]


navigation : AppState -> Route -> Html Msg
navigation appState currentRoute =
    div [ class "nav nav-pills flex-column" ]
        ([ strong [] [ lx_ "navigation.title.system" appState ] ]
            ++ List.map (navigationLink appState currentRoute) (navigationSystemLinks appState)
            ++ [ strong [] [ lx_ "navigation.title.userInterface" appState ] ]
            ++ List.map (navigationLink appState currentRoute) (navigationUserInterfaceLinks appState)
            ++ [ strong [] [ lx_ "navigation.title.content" appState ] ]
            ++ List.map (navigationLink appState currentRoute) (navigationContentLinks appState)
        )


navigationSystemLinks : AppState -> List ( Route, String )
navigationSystemLinks appState =
    [ ( OrganizationRoute, l_ "navigation.organization" appState )
    , ( AuthenticationRoute, l_ "navigation.authentication" appState )
    , ( PrivacyAndSupportRoute, l_ "navigation.privacyAndSupport" appState )
    ]


navigationUserInterfaceLinks : AppState -> List ( Route, String )
navigationUserInterfaceLinks appState =
    [ ( DashboardRoute, l_ "navigation.dashboard" appState )
    , ( LookAndFeelRoute, l_ "navigation.lookAndFeel" appState )
    ]


navigationContentLinks : AppState -> List ( Route, String )
navigationContentLinks appState =
    [ ( RegistryRoute, l_ "navigation.registry" appState )
    , ( QuestionnairesRoute, l_ "navigation.questionnaires" appState )
    , ( SubmissionRoute, l_ "navigation.submission" appState )
    , ( TemplateRoute, l_ "navigation.template" appState )
    ]


navigationLink : AppState -> Route -> ( Route, String ) -> Html Msg
navigationLink appState currentRoute ( route, title ) =
    linkTo appState
        (Wizard.Routes.SettingsRoute route)
        [ class "nav-link", classList [ ( "active", currentRoute == route ) ] ]
        [ text title ]
