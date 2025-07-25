module Wizard.Dashboard.Widgets.UsageWidget exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, h2, text)
import Html.Attributes exposing (class)
import Wizard.Api.Models.Usage exposing (Usage)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.UsageTable as UsageTable
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers


view : AppState -> ActionResult Usage -> Html msg
view appState usage =
    WidgetHelpers.widget <|
        case usage of
            Unset ->
                []

            Loading ->
                [ WidgetHelpers.widgetLoader ]

            Error error ->
                [ WidgetHelpers.widgetError error ]

            Success usageData ->
                [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Usage" appState.locale) ]
                , UsageTable.view appState False usageData
                ]
