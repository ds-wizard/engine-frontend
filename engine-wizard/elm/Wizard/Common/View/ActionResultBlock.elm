module Wizard.Common.View.ActionResultBlock exposing
    ( dropdownView
    , inlineView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown exposing (DropdownItem)
import Gettext exposing (gettext)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Page as Page


view : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
view appState viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Page.loader appState

        Error err ->
            div [ class "alert alert-danger" ] [ text err ]

        Success result ->
            viewContent result


inlineView : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
inlineView appState viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Flash.loader appState

        Error err ->
            Flash.error appState err

        Success result ->
            viewContent result


dropdownView : AppState -> (a -> DropdownItem msg) -> ActionResult (List a) -> List (DropdownItem msg)
dropdownView appState viewContent actionResult =
    case actionResult of
        Unset ->
            []

        Loading ->
            [ Dropdown.customItem
                (div [ class "dropdown-item dropdown-item-no-hover" ]
                    [ span [ class "alert-inline-loader" ]
                        [ faSet "_global.spinner" appState
                        , text (gettext "Loading..." appState.locale)
                        ]
                    ]
                )
            ]

        Error err ->
            [ Dropdown.customItem
                (div [ class "dropdown-item dropdown-item-no-hover" ]
                    [ span [ class "text-danger" ]
                        [ faSet "_global.error" appState
                        , span [ class "ms-2" ] [ text err ]
                        ]
                    ]
                )
            ]

        Success results ->
            List.map viewContent results
