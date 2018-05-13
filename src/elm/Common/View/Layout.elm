module Common.View.Layout exposing (appView, publicView)

import Auth.Msgs
import Auth.Permission as Perm exposing (hasPerm)
import Common.Html exposing (linkTo)
import Common.Html.Events exposing (onLinkClick)
import DSPlanner.Routing
import Html exposing (..)
import Html.Attributes exposing (..)
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
    div [ class "navbar navbar-default navbar-fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ linkTo homeRoute [ class "navbar-brand" ] [ text "Data Stewardship Wizard" ] ]
            , ul [ class "nav navbar-nav" ]
                [ li [] [ linkTo loginRoute [] [ text "Log In" ] ]
                , li [] [ linkTo signupRoute [] [ text "Sign Up" ] ]
                ]
            ]
        ]


appView : Model -> Html Msg -> Html Msg
appView model content =
    div [ class "app-view" ]
        [ menu model
        , div [ class "page" ]
            [ content ]
        ]


menu : Model -> Html Msg
menu model =
    div [ class "side-navigation" ]
        [ logo
        , ul [ class "menu" ]
            (createMenu model)
        , profileInfo model
        ]


logo : Html Msg
logo =
    linkTo Welcome
        [ class "logo" ]
        [ text "Data Stewardship Wizard" ]


createMenu : Model -> List (Html Msg)
createMenu model =
    menuItems
        |> List.filter (\( _, _, _, perm ) -> hasPerm model.jwt perm)
        |> List.map (menuItem model)


menuItems : List ( String, String, Route, String )
menuItems =
    [ ( "Organization", "fa-building", Organization, Perm.organization )
    , ( "Users", "fa-users", Users Users.Routing.Index, Perm.userManagement )
    , ( "KM Editor", "fa-edit", KMEditor, Perm.knowledgeModel )
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
            , text label
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
    in
    div [ class "profile-info" ]
        [ linkTo (Users <| Users.Routing.Edit "current") [ class "name" ] [ text name ]
        , a [ onLinkClick (Msgs.AuthMsg Auth.Msgs.Logout) ]
            [ i [ class "fa fa-sign-out" ] []
            , text "Logout"
            ]
        ]
