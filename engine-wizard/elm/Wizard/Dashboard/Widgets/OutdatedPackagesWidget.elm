module Wizard.Dashboard.Widgets.OutdatedPackagesWidget exposing (view)

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, code, div, h2, strong, text)
import Html.Attributes exposing (class)
import Shared.Components.Badge as Badge
import Shared.Html exposing (emptyNode)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> ActionResult (List Package) -> Html msg
view appState packages =
    case packages of
        ActionResult.Success packageList ->
            if not (List.isEmpty packageList) then
                viewWidget appState packageList

            else
                emptyNode

        _ ->
            emptyNode


viewWidget : AppState -> List Package -> Html msg
viewWidget appState packages =
    WidgetHelpers.widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Update Knowledge Models" appState.locale) ]
            , div [ class "mb-4" ] [ text (gettext "There are updates available for some knowledge models." appState.locale) ]
            , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewPackage appState) packages)
            ]
        ]


viewPackage : AppState -> Package -> Html msg
viewPackage appState package =
    linkTo appState
        (Routes.knowledgeModelsDetail package.id)
        [ class "p-2 py-2 d-flex rounded-3" ]
        [ ItemIcon.view { text = package.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text package.name ]
            , div [ class "d-flex align-items-center mt-1" ]
                [ code [] [ text package.id ]
                , Badge.warning [ class "ms-2" ] [ text (gettext "update available" appState.locale) ]
                ]
            ]
        ]
