module Registry.Common.View.ActionButton exposing (submit)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import String


submit : ( String, ActionResult a ) -> Html msg
submit ( label, result ) =
    actionButtonView [ type_ "submit" ] label result


actionButtonView : List (Attribute msg) -> String -> ActionResult a -> Html msg
actionButtonView attributes label result =
    let
        buttonContent =
            case result of
                Loading ->
                    span [ class "spinner-border spinner-border-sm" ] []

                _ ->
                    text label

        buttonAttributes =
            [ class "btn btn-primary btn-with-loader", disabled (result == Loading) ] ++ attributes
    in
    Html.button buttonAttributes [ buttonContent ]
