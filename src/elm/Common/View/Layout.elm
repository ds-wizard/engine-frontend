module Common.View.Layout exposing (appView, publicView)

import Auth.Msgs
import Auth.Permission as Perm exposing (hasPerm)
import Common.Html exposing (linkTo)
import Common.Html.Events exposing (onLinkClick)
import DSPlanner.Routing
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Routing
import KMPackages.Routing
import Models exposing (Model)
import Msgs exposing (Msg)
import Routing exposing (Route(..), homeRoute, loginRoute, signupRoute)
import Users.Routing


publicView : Html Msg -> Html Msg
publicView content =
    div [ class "public" ]
        [ publicHeader
        , div [ class "container" ]
            [ content ]
        ]


publicHeader : Html Msg
publicHeader =
    nav [ class "navbar navbar-expand-sm bg-primary fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ linkTo homeRoute [ class "navbar-brand" ] [ text "Data Stewardship Wizard" ] ]
            , ul [ class "nav navbar-nav ml-auto" ]
                [ li [ class "nav-item" ] [ linkTo loginRoute [ class "nav-link" ] [ text "Log In" ] ]
                , li [ class "nav-item" ] [ linkTo signupRoute [ class "nav-link" ] [ text "Sign Up" ] ]
                ]
            ]
        ]


appView : Model -> Html Msg -> Html Msg
appView model content =
    div [ class "app-view", classList [ ( "side-navigation-collapsed", model.session.sidebarCollapsed ) ] ]
        [ menu model
        , div [ class "page row justify-content-center" ]
            [ content ]
        ]


menu : Model -> Html Msg
menu model =
    div [ class "side-navigation", classList [ ( "side-navigation-collapsed", model.session.sidebarCollapsed ) ] ]
        [ logo model
        , ul [ class "menu" ]
            (createMenu model)
        , profileInfo model
        ]


logo : Model -> Html Msg
logo model =
    let
        heading =
            if model.session.sidebarCollapsed then
                "DSW"
            else
                "Data Stewardship Wizard"
    in
    linkTo Welcome
        [ class "logo" ]
        [ text heading ]


createMenu : Model -> List (Html Msg)
createMenu model =
    menuItems
        |> List.filter (\( _, _, _, perm ) -> hasPerm model.jwt perm)
        |> List.map (menuItem model)


menuItems : List ( String, String, Route, String )
menuItems =
    [ ( "Organization", "fa-building", Organization, Perm.organization )
    , ( "Users", "fa-users", Users Users.Routing.Index, Perm.userManagement )
    , ( "KM Editor", "fa-edit", KMEditor KMEditor.Routing.Index, Perm.knowledgeModel )
    , ( "KM Packages", "fa-cubes", KMPackages KMPackages.Routing.Index, Perm.packageManagement )
    , ( "DS Planner", "fa-list-alt", DSPlanner DSPlanner.Routing.Index, Perm.questionnaire )
    ]


menuItem : Model -> ( String, String, Route, String ) -> Html Msg
menuItem model ( label, icon, route, perm ) =
    let
        activeClass =
            if model.route == route then
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
            case model.session.user of
                Just user ->
                    user.name ++ " " ++ user.surname

                Nothing ->
                    ""

        collapseLink =
            if model.session.sidebarCollapsed then
                a [ onLinkClick (Msgs.SetSidebarCollapsed False), class "collapse" ]
                    [ i [ class "fa fa-angle-double-right" ] []
                    ]
            else
                a [ onLinkClick (Msgs.SetSidebarCollapsed True), class "collapse" ]
                    [ i [ class "fa fa-angle-double-left" ] []
                    ]
    in
    div [ class "profile-info" ]
        [ ul [ class "menu" ]
            [ li []
                [ linkTo (Users <| Users.Routing.Edit "current")
                    []
                    [ i [ class "fa fa-user-circle-o" ] []
                    , span [ class "sidebar-link" ] [ text name ]
                    ]
                ]
            , li []
                [ a [ onLinkClick (Msgs.AuthMsg Auth.Msgs.Logout) ]
                    [ i [ class "fa fa-sign-out" ] []
                    , span [ class "sidebar-link" ] [ text "Logout" ]
                    ]
                ]
            ]
        , collapseLink
        ]
