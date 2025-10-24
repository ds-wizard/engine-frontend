module Wizard.Pages.Dashboard.Widgets.OutdatedPackagesWidget exposing (view)

import ActionResult exposing (ActionResult)
import Common.Components.Badge as Badge
import Gettext exposing (gettext)
import Html exposing (Html, code, div, h2, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> ActionResult (List KnowledgeModelPackage) -> Html msg
view appState packages =
    case packages of
        ActionResult.Success packageList ->
            if not (List.isEmpty packageList) then
                viewWidget appState packageList

            else
                Html.nothing

        _ ->
            Html.nothing


viewWidget : AppState -> List KnowledgeModelPackage -> Html msg
viewWidget appState kmPackages =
    WidgetHelpers.widget
        [ div [ class "d-flex flex-column h-100" ]
            [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Update Knowledge Models" appState.locale) ]
            , div [ class "mb-4" ] [ text (gettext "There are updates available for some knowledge models." appState.locale) ]
            , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewKnowledgeModelPackage appState) kmPackages)
            ]
        ]


viewKnowledgeModelPackage : AppState -> KnowledgeModelPackage -> Html msg
viewKnowledgeModelPackage appState kmPackage =
    linkTo (Routes.knowledgeModelsDetail kmPackage.id)
        [ class "p-2 py-2 d-flex rounded-3" ]
        [ ItemIcon.view { text = kmPackage.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text kmPackage.name ]
            , div [ class "d-flex align-items-center mt-1" ]
                [ code [] [ text kmPackage.id ]
                , Badge.warning [ class "ms-2" ] [ text (gettext "update available" appState.locale) ]
                ]
            ]
        ]
