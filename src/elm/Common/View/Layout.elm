module Common.View.Layout exposing (appView, publicView)

import Auth.Permission as Perm exposing (hasPerm)
import Browser exposing (Document)
import Common.Html exposing (fa, linkTo)
import Common.Html.Events exposing (onLinkClick)
import Common.Menu.View exposing (viewAboutModal, viewProfileMenu, viewReportIssueModal)
import DSPlanner.Routing
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Routing
import KMPackages.Routing
import Models exposing (Model, userLoggedIn)
import Msgs exposing (Msg)
import Routing exposing (Route(..), appRoute, homeRoute, loginRoute, questionnaireDemoRoute, signupRoute)
import Users.Routing


publicView : Model -> Html Msg -> Document Msg
publicView model content =
    let
        html =
            div [ class "public" ]
                [ publicHeader model
                , div [ class "container" ]
                    [ content ]
                ]
    in
    { title = "Data Stewardship Wizard"
    , body = [ html ]
    }


publicHeader : Model -> Html Msg
publicHeader model =
    let
        questionnaireDemoLink =
            li [ class "nav-item" ] [ linkTo questionnaireDemoRoute [ class "nav-link" ] [ text "Questionnaire Demo" ] ]

        links =
            if userLoggedIn model then
                [ questionnaireDemoLink
                , li [ class "nav-item" ] [ linkTo appRoute [ class "nav-link" ] [ text "Go to App" ] ]
                ]

            else
                [ questionnaireDemoLink
                , li [ class "nav-item" ] [ linkTo loginRoute [ class "nav-link" ] [ text "Log In" ] ]
                , li [ class "nav-item" ] [ linkTo signupRoute [ class "nav-link" ] [ text "Sign Up" ] ]
                ]
    in
    nav [ class "navbar navbar-expand-sm bg-primary fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ linkTo homeRoute [ class "navbar-brand" ] [ text "Data Stewardship Wizard" ] ]
            , ul [ class "nav navbar-nav ml-auto" ] links
            ]
        ]


appView : Model -> Html Msg -> Document Msg
appView model content =
    let
        html =
            div [ class "app-view", classList [ ( "side-navigation-collapsed", model.state.session.sidebarCollapsed ) ] ]
                [ menu model
                , div [ class "page row justify-content-center" ]
                    [ content ]
                , viewReportIssueModal model.menuModel.reportIssueOpen
                , viewAboutModal model.menuModel.aboutOpen model.menuModel.apiBuildInfo
                ]
    in
    { title = "Data Stewardship Wizard"
    , body = [ html ]
    }


menu : Model -> Html Msg
menu model =
    div [ class "side-navigation", classList [ ( "side-navigation-collapsed", model.state.session.sidebarCollapsed ) ] ]
        [ logo model
        , ul [ class "menu" ]
            (createMenu model)
        , profileInfo model
        ]


logo : Model -> Html Msg
logo model =
    let
        logoImg =
            if model.state.session.sidebarCollapsed then
                img [ src "/img/dsw-logo.svg" ] []

            else
                span [ class "logo-full" ]
                    [ img [ src "/img/dsw-logo.svg" ] []
                    , span [] [ text "DS Wizard" ]
                    ]
    in
    linkTo Welcome [ class "logo" ] [ logoImg ]


type MenuItem
    = MenuItem String String Route String


createMenu : Model -> List (Html Msg)
createMenu model =
    menuItems
        |> List.filter (\(MenuItem _ _ _ perm) -> hasPerm model.state.jwt perm)
        |> List.map (menuItem model)


menuItems : List MenuItem
menuItems =
    [ MenuItem "Organization" "fa-building" Organization Perm.organization
    , MenuItem "Users" "fa-users" (Users Users.Routing.Index) Perm.userManagement
    , MenuItem "KM Editor" "fa-edit" (KMEditor KMEditor.Routing.Index) Perm.knowledgeModel
    , MenuItem "KM Packages" "fa-cubes" (KMPackages KMPackages.Routing.Index) Perm.packageManagementRead
    , MenuItem "DS Planner" "fa-list-alt" (DSPlanner DSPlanner.Routing.Index) Perm.questionnaire
    ]


menuItem : Model -> MenuItem -> Html Msg
menuItem model (MenuItem label icon route perm) =
    let
        activeClass =
            if model.state.route == route then
                "active"

            else
                ""
    in
    li []
        [ linkTo route
            [ class activeClass ]
            [ i [ class ("fa " ++ icon) ] []
            , span [ class "sidebar-link" ] [ text label ]
            ]
        ]


profileInfo : Model -> Html Msg
profileInfo model =
    let
        name =
            case model.state.session.user of
                Just user ->
                    user.name ++ " " ++ user.surname

                Nothing ->
                    ""

        collapseLink =
            if model.state.session.sidebarCollapsed then
                a [ onLinkClick (Msgs.SetSidebarCollapsed False), class "collapse" ]
                    [ fa "angle-double-right" ]

            else
                a [ onLinkClick (Msgs.SetSidebarCollapsed True), class "collapse" ]
                    [ fa "angle-double-left" ]
    in
    div [ class "profile-info" ]
        [ viewProfileMenu model.state.session.user model.menuModel.profileMenuDropdownState
        , collapseLink
        ]
