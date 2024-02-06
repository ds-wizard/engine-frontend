module Registry2.Components.Page exposing (view)

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, i, p, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Registry2.Data.AppState exposing (AppState)
import Shared.Undraw as Undraw


view : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
view appState viewPage actionResult =
    case actionResult of
        ActionResult.Unset ->
            Html.nothing

        ActionResult.Loading ->
            Html.div [ class "page-loader" ]
                [ i [ class "fas fa-spinner fa-spin" ] []
                , div [ class "mt-2" ] [ text (gettext "Loading..." appState.locale) ]
                ]

        ActionResult.Success a ->
            viewPage a

        ActionResult.Error e ->
            div [ class "page-error d-flex flex-lg-row flex-column-reverse justify-content-center align-items-center" ]
                [ Undraw.cancel
                , div [ class "ms-5 mb-5" ]
                    [ h1 [] [ text (gettext "Error" appState.locale) ]
                    , p [ class "fs-4" ] [ text e ]
                    ]
                ]
