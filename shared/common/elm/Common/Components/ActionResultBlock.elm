module Common.Components.ActionResultBlock exposing
    ( DropdownViewConfig
    , ViewConfig
    , dropdownView
    , inlineView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown exposing (DropdownItem)
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faError)
import Common.Components.Page as Page
import Gettext
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Extra as Html


type alias ViewConfig a msg =
    { viewContent : a -> Html msg
    , actionResult : ActionResult a
    , locale : Gettext.Locale
    }


view : ViewConfig a msg -> Html msg
view cfg =
    case cfg.actionResult of
        Unset ->
            Html.nothing

        Loading ->
            Page.loader cfg

        Error err ->
            Flash.error err

        Success result ->
            cfg.viewContent result


inlineView : ViewConfig a msg -> Html msg
inlineView cfg =
    case cfg.actionResult of
        Unset ->
            Html.nothing

        Loading ->
            Flash.loader cfg.locale

        Error err ->
            Flash.error err

        Success result ->
            cfg.viewContent result


type alias DropdownViewConfig a msg =
    { viewContent : a -> DropdownItem msg
    , actionResult : ActionResult (List a)
    , locale : Gettext.Locale
    }


dropdownView : DropdownViewConfig a msg -> List (DropdownItem msg)
dropdownView cfg =
    case cfg.actionResult of
        Unset ->
            []

        Loading ->
            [ Dropdown.customItem
                (div [ class "dropdown-item dropdown-item-no-hover" ]
                    [ Flash.loader cfg.locale ]
                )
            ]

        Error err ->
            [ Dropdown.customItem
                (div [ class "dropdown-item dropdown-item-no-hover" ]
                    [ span [ class "text-danger" ]
                        [ faError
                        , span [ class "ms-2" ] [ text err ]
                        ]
                    ]
                )
            ]

        Success results ->
            List.map cfg.viewContent results
