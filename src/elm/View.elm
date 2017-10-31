module View exposing (..)

import Common.Html exposing (onLinkClick)
import Html exposing (Attribute, Html, a, div, i, li, text, ul)
import Html.Attributes exposing (class, href)
import KnowledgeModels.Create.View
import KnowledgeModels.Editor.View
import KnowledgeModels.Index.View
import Login.View
import Models exposing (Model)
import Msgs exposing (Msg)
import Routing


view : Model -> Html Msg
view model =
    case model.route of
        Models.LoginRoute ->
            Login.View.view

        Models.IndexRoute ->
            appView model indexView

        Models.OrganizationRoute ->
            appView model organizationView

        Models.UserManagementRoute ->
            appView model userManagementView

        Models.KnowledgeModelsCreateRoute ->
            appView model KnowledgeModels.Create.View.view

        Models.KnowledgeModelsEditorRoute ->
            appView model KnowledgeModels.Editor.View.view

        Models.KnowledgeModelsRoute ->
            appView model KnowledgeModels.Index.View.view

        Models.WizzardsRoute ->
            appView model wizzardsView

        Models.DataManagementPlansRoute ->
            appView model dataManagementPlansView

        Models.NotFoundRouteRoute ->
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
            [ menuItem "Organization" "fa-building" Routing.organizationPath (model.route == Models.OrganizationRoute)
            , menuItem "User Management" "fa-users" Routing.userManagementPath (model.route == Models.UserManagementRoute)
            , menuItem "Knowledge Models" "fa-database" Routing.knowledgeModelsPath (model.route == Models.KnowledgeModelsRoute)
            , menuItem "Wizzards" "fa-list-alt" Routing.wizzardsPath (model.route == Models.WizzardsRoute)
            , menuItem "Data Management Plans" "fa-file-text" Routing.dataManagementPlansPath (model.route == Models.DataManagementPlansRoute)
            ]
        ]


logo : Html Msg
logo =
    a [ class "logo", href Routing.indexPath, onLinkClick (Msgs.ChangeLocation Routing.indexPath) ]
        [ text "Elixir DSP" ]


menuItem : String -> String -> String -> Bool -> Html Msg
menuItem label icon url active =
    li []
        [ a
            [ href url
            , onLinkClick (Msgs.ChangeLocation url)
            , class
                (if active then
                    "active"
                 else
                    ""
                )
            ]
            [ i [ class ("fa " ++ icon) ] []
            , text label
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
