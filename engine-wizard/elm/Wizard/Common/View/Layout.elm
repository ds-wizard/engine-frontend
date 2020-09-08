module Wizard.Common.View.Layout exposing
    ( app
    , misconfigured
    , mixedApp
    , public
    , publicApp
    )

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Auth.Permission as Perm
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink exposing (CustomMenuLink)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Events exposing (onLinkClick)
import Wizard.Common.Menu.View exposing (viewAboutModal, viewHelpMenu, viewProfileMenu, viewReportIssueModal, viewSettingsMenu)
import Wizard.Common.View.Page as Page
import Wizard.Documents.Routes
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Models exposing (Model, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Projects.Routes
import Wizard.Routes as Routes
import Wizard.Routing exposing (appRoute, homeRoute, loginRoute, signupRoute)
import Wizard.Templates.Routes
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


mixedApp : Model -> Html Msg -> Document Msg
mixedApp model =
    if model.appState.session.user == Nothing then
        publicApp model

    else
        app model


public : Model -> Html Msg -> Document Msg
public model content =
    let
        html =
            div [ class "public" ]
                [ publicHeader False model
                , div [ class "container" ] [ content ]
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html ]
    }


publicApp : Model -> Html Msg -> Document Msg
publicApp model content =
    let
        html =
            div [ class "public public--app" ]
                [ publicHeader True model
                , div [ class "container-fluid" ] [ content ]
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html ]
    }


publicHeader : Bool -> Model -> Html Msg
publicHeader fluid model =
    let
        signUpLink =
            if model.appState.config.authentication.internal.registration.enabled then
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
                [ li [ class "nav-item" ]
                    [ linkTo model.appState
                        appRoute
                        [ class "nav-link" ]
                        [ lx_ "header.goToApp" model.appState ]
                    ]
                ]

            else
                [ li [ class "nav-item" ]
                    [ linkTo model.appState
                        (loginRoute Nothing)
                        [ class "nav-link" ]
                        [ lx_ "header.logIn" model.appState ]
                    ]
                , signUpLink
                ]
    in
    nav [ class "navbar navbar-expand-sm fixed-top" ]
        [ div [ classList [ ( "container-fluid", fluid ), ( "container", not fluid ) ] ]
            [ div [ class "navbar-header" ]
                [ linkTo model.appState
                    homeRoute
                    [ class "navbar-brand" ]
                    [ text <| LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
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
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
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
                [ span [] [ text <| LookAndFeelConfig.getAppTitleShort model.appState.config.lookAndFeel ] ]
    in
    linkTo model.appState Routes.DashboardRoute [ class "logo" ] [ logoImg ]


type MenuItem msg
    = MenuItem String (Html msg) Routes.Route (Routes.Route -> Bool) String


createMenu : Model -> List (Html Msg)
createMenu model =
    let
        defaultMenuItems =
            menuItems model.appState
                |> List.filter (\(MenuItem _ _ _ _ perm) -> Perm.hasPerm model.appState.session perm)
                |> List.map (menuItem model)

        customMenuItems =
            List.map customMenuItem model.appState.config.lookAndFeel.customMenuLinks
    in
    defaultMenuItems ++ customMenuItems


menuItems : AppState -> List (MenuItem msg)
menuItems appState =
    let
        isQuestionnaireIndex route =
            case route of
                Routes.PlansRoute (Wizard.Projects.Routes.IndexRoute _) ->
                    True

                _ ->
                    False

        isDocumentsIndex route =
            case route of
                Routes.DocumentsRoute (Wizard.Documents.Routes.IndexRoute _ _) ->
                    True

                _ ->
                    False
    in
    [ MenuItem
        (l_ "menu.users" appState)
        (faSet "menu.users" appState)
        (Routes.UsersRoute Wizard.Users.Routes.IndexRoute)
        ((==) (Routes.UsersRoute Wizard.Users.Routes.IndexRoute))
        Perm.userManagement
    , MenuItem
        (l_ "menu.kmEditor" appState)
        (faSet "menu.kmEditor" appState)
        (Routes.KMEditorRoute Wizard.KMEditor.Routes.IndexRoute)
        ((==) (Routes.KMEditorRoute Wizard.KMEditor.Routes.IndexRoute))
        Perm.knowledgeModel
    , MenuItem
        (l_ "menu.knowledgeModels" appState)
        (faSet "menu.knowledgeModels" appState)
        (Routes.KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.IndexRoute)
        ((==) (Routes.KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.IndexRoute))
        Perm.packageManagementRead
    , MenuItem
        (l_ "menu.projects" appState)
        (faSet "menu.projects" appState)
        (Routes.PlansRoute <| Wizard.Projects.Routes.IndexRoute PaginationQueryString.empty)
        isQuestionnaireIndex
        Perm.questionnaire
    , MenuItem
        (l_ "menu.documents" appState)
        (faSet "menu.documents" appState)
        (Routes.DocumentsRoute <| Wizard.Documents.Routes.IndexRoute Nothing PaginationQueryString.empty)
        isDocumentsIndex
        Perm.documents
    , MenuItem
        (l_ "menu.templates" appState)
        (faSet "menu.templates" appState)
        (Routes.TemplatesRoute Wizard.Templates.Routes.IndexRoute)
        ((==) (Routes.TemplatesRoute Wizard.Templates.Routes.IndexRoute))
        Perm.templates
    ]


menuItem : Model -> MenuItem msg -> Html msg
menuItem model (MenuItem label icon route isActive _) =
    let
        activeClass =
            if isActive model.appState.route then
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
