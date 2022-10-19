module Wizard.Dashboard.Widgets.OutdatedTemplatesWidget exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, code, div, h2, strong, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Data.Template exposing (Template)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Dashboard.Widgets.OutdatedTemplatesWidget"


view : AppState -> ActionResult (List Template) -> Html msg
view appState templates =
    case templates of
        ActionResult.Success templateList ->
            if not (List.isEmpty templateList) then
                viewWidget appState templateList

            else
                emptyNode

        _ ->
            emptyNode


viewWidget : AppState -> List Template -> Html msg
viewWidget appState templates =
    WidgetHelpers.widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ lx_ "title" appState ]
            , div [ class "mb-4" ] [ lx_ "description" appState ]
            , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewTemplate appState) templates)
            ]
        ]


viewTemplate : AppState -> Template -> Html msg
viewTemplate appState template =
    linkTo appState
        (Routes.templatesDetail template.id)
        [ class "p-2 py-2 d-flex rounded-3" ]
        [ ItemIcon.view { text = template.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text template.name ]
            , div [ class "d-flex align-items-center mt-1" ]
                [ code [] [ text template.id ]
                , Badge.warning [ class "ms-2" ] [ lx_ "updateBadge" appState ]
                ]
            ]
        ]
