module View exposing (..)

import Html exposing (Attribute, Html, a, div, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Models exposing (Model)
import Msgs exposing (Msg)
import Routing


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
    onWithOptions "click" options (Decode.succeed message)


view : Model -> Html Msg
view model =
    div []
        [ menu
        , content model
        ]


menu : Html Msg
menu =
    ul []
        [ menuItem "Index" Routing.indexPath
        , menuItem "Organization" Routing.organizationPath
        , menuItem "User Management" Routing.userManagementPath
        , menuItem "Knowledge Models" Routing.knowledgeModelsPath
        , menuItem "Wizzards" Routing.wizzardsPath
        , menuItem "Data Management Plans" Routing.dataManagementPlansPath
        ]


menuItem : String -> String -> Html Msg
menuItem label url =
    li []
        [ a [ href url, onLinkClick (Msgs.ChangeLocation url) ] [ text label ] ]


content : Model -> Html Msg
content model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Models.IndexRoute ->
            indexView

        Models.OrganizationRoute ->
            organizationView

        Models.UserManagementRoute ->
            userManagementView

        Models.KnowledgeModelsRoute ->
            knowledgeModelsView

        Models.WizzardsRoute ->
            wizzardsView

        Models.DataManagementPlansRoute ->
            dataManagementPlansView

        Models.NotFoundRouteRoute ->
            notFoundView


indexView : Html Msg
indexView =
    text "Welcome to DSP!"


organizationView : Html Msg
organizationView =
    text "Organization"


userManagementView : Html Msg
userManagementView =
    text "User Management"


knowledgeModelsView : Html Msg
knowledgeModelsView =
    text "Knowledge Models"


wizzardsView : Html Msg
wizzardsView =
    text "Wizzards"


dataManagementPlansView : Html Msg
dataManagementPlansView =
    text "Data Management Plans"


notFoundView : Html msg
notFoundView =
    text "Not Found"
