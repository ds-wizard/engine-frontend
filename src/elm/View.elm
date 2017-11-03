module View exposing (..)

import Auth.Msgs
import Auth.View
import Common.Html exposing (linkTo, onLinkClick)
import Html exposing (..)
import Html.Attributes exposing (class, href)
import KnowledgeModels.Create.View
import KnowledgeModels.Editor.View
import KnowledgeModels.Index.View
import Models exposing (Model)
import Msgs exposing (Msg)
import Routing exposing (Route(..))


view : Model -> Html Msg
view model =
    case model.route of
        Login ->
            Auth.View.view model.authModel

        Index ->
            appView model indexView

        Organization ->
            appView model organizationView

        UserManagement ->
            appView model userManagementView

        KnowledgeModelsCreate ->
            appView model KnowledgeModels.Create.View.view

        KnowledgeModelsEditor ->
            appView model KnowledgeModels.Editor.View.view

        KnowledgeModels ->
            appView model KnowledgeModels.Index.View.view

        Wizzards ->
            appView model wizzardsView

        DataManagementPlans ->
            appView model dataManagementPlansView

        NotFound ->
            appView model notFoundView


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
            [ menuItem model "Organization" "fa-building" Organization
            , menuItem model "User Management" "fa-users" UserManagement
            , menuItem model "Knowledge Models" "fa-database" KnowledgeModels
            , menuItem model "Wizzards" "fa-list-alt" Wizzards
            , menuItem model "Data Management Plans" "fa-file-text" DataManagementPlans
            ]
        , profileInfo model
        ]


logo : Html Msg
logo =
    linkTo Index
        [ class "logo" ]
        [ text "Elixir DSP" ]


menuItem : Model -> String -> String -> Route -> Html Msg
menuItem model label icon route =
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
        [ span [] [ text name ]
        , a [ onLinkClick (Msgs.AuthMsg Auth.Msgs.Logout) ]
            [ i [ class "fa fa-sign-out" ] []
            , text "Logout"
            ]
        ]


indexView : Html Msg
indexView =
    text "Welcome to DSP!"


organizationView : Html Msg
organizationView =
    text "Organization"


userManagementView : Html Msg
userManagementView =
    text "User Management"


wizzardsView : Html Msg
wizzardsView =
    text "Wizzards"


dataManagementPlansView : Html Msg
dataManagementPlansView =
    text "Data Management Plans"


notFoundView : Html msg
notFoundView =
    text "Not Found"
