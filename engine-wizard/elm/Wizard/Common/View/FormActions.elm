module Wizard.Common.View.FormActions exposing (view, viewActionOnly, viewSubmit)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Locale exposing (lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Routes as Routes


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.View.FormActions"


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
view : AppState -> Routes.Route -> ActionButton.ButtonConfig a msg -> Html msg
view appState cancelRoute actionButtonConfig =
    div [ class "form-actions" ]
        [ linkTo appState cancelRoute [ class "btn btn-secondary" ] [ lx_ "cancelButton.cancel" appState ]
        , ActionButton.button appState actionButtonConfig
        ]


viewSubmit : AppState -> Routes.Route -> ActionButton.SubmitConfig a -> Html msg
viewSubmit appState cancelRoute submitConfig =
    div [ class "form-actions" ]
        [ linkTo appState cancelRoute [ class "btn btn-secondary" ] [ lx_ "cancelButton.cancel" appState ]
        , ActionButton.submit appState submitConfig
        ]


{-| Similar to previous, but it contains only the action button.
-}
viewActionOnly : AppState -> ActionButton.ButtonConfig a msg -> Html msg
viewActionOnly appState actionButtonConfig =
    div [ class "text-right" ]
        [ ActionButton.button appState actionButtonConfig ]
