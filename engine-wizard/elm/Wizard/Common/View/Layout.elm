module Wizard.Common.View.Layout exposing
    ( app
    , misconfigured
    , public
    )

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.CustomMenuLink exposing (CustomMenuLink)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Events exposing (onLinkClick)
import Wizard.Common.Menu.View exposing (viewAboutModal, viewHelpMenu, viewProfileMenu, viewReportIssueModal, viewSettingsMenu)
import Wizard.Common.View.Page as Page
import Wizard.Documents.Routes
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Models exposing (Model, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Questionnaires.Routes
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (appRoute, homeRoute, loginRoute, questionnaireDemoRoute, signupRoute)
import Wizard.Users.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.View.Layout"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.View.Layout"


misconfigured : AppState -> Document Msg
misconfigured appState =
    let
        html =
            Page.illustratedMessage
                { image = "bug_fixing"
                , heading = l_ "misconfigured.configurationError" appState
                , lines =
                    [ l_ "misconfigured.appNotConfigured" appState
                    , l_ "misconfigured.contactAdmin" appState
                    ]
                }
    in
    { title = l_ "misconfigured.configurationError" appState
    , body = [ html ]
    }


public : Model -> Html Msg -> Document Msg
public model content =
    let
        html =
            div [ class "public" ]
                [ publicHeader model
                , div [ class "container" ]
                    [ content ]
                ]
    in
    { title = model.appState.config.client.appTitle
    , body = [ html ]
    }


publicHeader : Model -> Html Msg
publicHeader model =
    let
        questionnaireDemoLink =
            if model.appState.config.features.publicQuestionnaire.enabled then
                li [ class "nav-item" ]
                    [ linkTo model.appState
                        questionnaireDemoRoute
                        [ class "nav-link" ]
                        [ lx_ "header.questionnaireDemo" model.appState ]
                    ]

            else
                emptyNode

        signUpLink =
            if model.appState.config.auth.internal.registration.enabled then
                li [ class "nav-item" ]
                    [ linkTo model.appState
                        signupRoute
                        [ class "nav-link" ]
                        [ lx_ "header.signUp" model.appState ]
                    ]

            else
                emptyNode

        links =
            if userLoggedIn model then
                [ questionnaireDemoLink
                , li [ class "nav-item" ]
                    [ linkTo model.appState
                        appRoute
                        [ class "nav-link" ]
                        [ lx_ "header.goToApp" model.appState ]
                    ]
                ]

            else
                [ questionnaireDemoLink
                , li [ class "nav-item" ]
                    [ linkTo model.appState
                        (loginRoute Nothing)
                        [ class "nav-link" ]
                        [ lx_ "header.logIn" model.appState ]
                    ]
                , signUpLink
                ]
    in
    nav [ class "navbar navbar-expand-sm fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ linkTo model.appState
                    homeRoute
                    [ class "navbar-brand" ]
                    [ text model.appState.config.client.appTitle
                    ]
                ]
            , ul [ class "nav navbar-nav ml-auto" ] links
            ]
        ]


app : Model -> Html Msg -> Document Msg
app model content =
    let
        html =
            div [ class "app-view", classList [ ( "side-navigation-collapsed", model.appState.session.sidebarCollapsed ) ] ]
                [ menu model
                , div [ class "page row justify-content-center" ]
                    [ content ]
                , viewReportIssueModal model.appState model.menuModel.reportIssueOpen
                , viewAboutModal model.appState model.menuModel.aboutOpen model.menuModel.apiBuildInfo
                ]
    in
    { title = model.appState.config.client.appTitle
    , body = [ html ]
    }


menu : Model -> Html Msg
menu model =
    div [ class "side-navigation", classList [ ( "side-navigation-collapsed", model.appState.session.sidebarCollapsed ) ] ]
        [ logo model
        , ul [ class "menu" ]
            (createMenu model)
        , profileInfo model
        ]


logo : Model -> Html Msg
logo model =
    let
        logoImg =
            span [ class "logo-full" ]
                [ span [] [ text model.appState.config.client.appTitleShort ] ]
    in
    linkTo model.appState Routes.DashboardRoute [ class "logo" ] [ logoImg ]


type MenuItem msg
    = MenuItem String (Html msg) Routes.Route String


createMenu : Model -> List (Html Msg)
createMenu model =
    let
        defaultMenuItems =
            menuItems model.appState
                |> List.filter (\(MenuItem _ _ _ perm) -> hasPerm model.appState.jwt perm)
                |> List.map (menuItem model)

        customMenuItems =
            List.map customMenuItem model.appState.config.client.customMenuLinks
    in
    defaultMenuItems ++ customMenuItems


menuItems : AppState -> List (MenuItem msg)
menuItems appState =
    [ MenuItem
        (l_ "menu.users" appState)
        (faSet "menu.users" appState)
        (Routes.UsersRoute Wizard.Users.Routes.IndexRoute)
        Perm.userManagement
    , MenuItem
        (l_ "menu.kmEditor" appState)
        (faSet "menu.kmEditor" appState)
        (Routes.KMEditorRoute Wizard.KMEditor.Routes.IndexRoute)
        Perm.knowledgeModel
    , MenuItem
        (l_ "menu.knowledgeModels" appState)
        (faSet "menu.knowledgeModels" appState)
        (Routes.KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.IndexRoute)
        Perm.packageManagementRead
    , MenuItem
        (l_ "menu.questionnaires" appState)
        (faSet "menu.questionnaires" appState)
        (Routes.QuestionnairesRoute Wizard.Questionnaires.Routes.IndexRoute)
        Perm.questionnaire
    , MenuItem
        (l_ "menu.documents" appState)
        (faSet "menu.documents" appState)
        (Routes.DocumentsRoute <| Wizard.Documents.Routes.IndexRoute Nothing)
        Perm.questionnaire
    ]


menuItem : Model -> MenuItem msg -> Html msg
menuItem model (MenuItem label icon route _) =
    let
        activeClass =
            if model.appState.route == route then
                "active"

            else
                ""
    in
    li []
        [ linkTo model.appState
            route
            [ class activeClass ]
            [ icon
            , span [ class "sidebar-link" ] [ text label ]
            ]
        ]


customMenuItem : CustomMenuLink -> Html msg
customMenuItem link =
    let
        targetArg =
            if link.newWindow then
                [ target "_blank" ]

            else
                []
    in
    li []
        [ a ([ href link.url ] ++ targetArg)
            [ fa link.icon
            , span [ class "sidebar-link" ] [ text link.title ]
            ]
        ]


profileInfo : Model -> Html Msg
profileInfo model =
    let
        collapseLink =
            if model.appState.session.sidebarCollapsed then
                a [ onLinkClick (Wizard.Msgs.SetSidebarCollapsed False), class "collapse" ]
                    [ faSet "menu.open" model.appState ]

            else
                a [ onLinkClick (Wizard.Msgs.SetSidebarCollapsed True), class "collapse" ]
                    [ faSet "menu.collapse" model.appState
                    , lx_ "sidebar.collapse" model.appState
                    ]
    in
    div [ class "profile-info" ]
        [ viewSettingsMenu model.appState
        , viewHelpMenu model.appState model.menuModel.helpMenuDropdownState
        , viewProfileMenu model.appState model.menuModel.profileMenuDropdownState
        , collapseLink
        ]
