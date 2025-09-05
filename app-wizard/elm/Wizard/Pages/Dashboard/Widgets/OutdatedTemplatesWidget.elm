module Wizard.Pages.Dashboard.Widgets.OutdatedTemplatesWidget exposing (view)

import ActionResult exposing (ActionResult)
import Common.Components.Badge as Badge
import Gettext exposing (gettext)
import Html exposing (Html, code, div, h2, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> ActionResult (List DocumentTemplate) -> Html msg
view appState templates =
    case templates of
        ActionResult.Success templateList ->
            if not (List.isEmpty templateList) then
                viewWidget appState templateList

            else
                Html.nothing

        _ ->
            Html.nothing


viewWidget : AppState -> List DocumentTemplate -> Html msg
viewWidget appState templates =
    WidgetHelpers.widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Update Document Templates" appState.locale) ]
            , div [ class "mb-4" ] [ text (gettext "There are updates available for some document templates." appState.locale) ]
            , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewTemplate appState) templates)
            ]
        ]


viewTemplate : AppState -> DocumentTemplate -> Html msg
viewTemplate appState template =
    linkTo (Routes.documentTemplatesDetail template.id)
        [ class "p-2 py-2 d-flex rounded-3" ]
        [ ItemIcon.view { text = template.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text template.name ]
            , div [ class "d-flex align-items-center mt-1" ]
                [ code [] [ text template.id ]
                , Badge.warning [ class "ms-2" ] [ text (gettext "update available" appState.locale) ]
                ]
            ]
        ]
