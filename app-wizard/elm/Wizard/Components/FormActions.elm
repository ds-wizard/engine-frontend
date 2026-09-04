module Wizard.Components.FormActions exposing
    ( view
    , viewCustomButton
    )

import ActionResult
import Common.Components.ActionButton as ActionButton
import Common.Utils.ShortcutUtils as Shortcut
import Gettext exposing (gettext)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shortcut
import Wizard.Data.AppState exposing (AppState)


view : AppState -> msg -> ActionButton.ButtonConfig a msg -> Html msg
view appState cancelMsg actionButtonConfig =
    let
        shortcuts =
            if ActionResult.isLoading actionButtonConfig.result then
                []

            else
                [ Shortcut.submitShortcut appState.navigator.isMac actionButtonConfig.msg ]
    in
    Shortcut.shortcutElement shortcuts
        [ class "form-actions" ]
        [ button [ class "btn btn-secondary", onClick cancelMsg ] [ text (gettext "Cancel" appState.locale) ]
        , ActionButton.button actionButtonConfig
        ]


viewCustomButton : AppState -> msg -> Html msg -> Html msg
viewCustomButton appState cancelMsg actionButton =
    div [ class "form-actions" ]
        [ button [ class "btn btn-secondary", onClick cancelMsg ] [ text (gettext "Cancel" appState.locale) ]
        , actionButton
        ]
