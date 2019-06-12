module Common.View.ActionButton exposing
    ( ButtonConfig
    , SubmitConfig
    , button
    , submit
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (fa)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String


type alias ButtonConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


button : ButtonConfig a msg -> Html msg
button cfg =
    let
        cssClass =
            if cfg.dangerous then
                "btn-danger"

            else
                "btn-primary"
    in
    actionButtonView [ onClick cfg.msg, class <| "btn btn-with-loader " ++ cssClass ] cfg.label cfg.result


type alias SubmitConfig a =
    { label : String
    , result : ActionResult a
    }


submit : SubmitConfig a -> Html msg
submit { label, result } =
    actionButtonView [ type_ "submit", class "btn btn-primary btn-with-loader" ] label result


actionButtonView : List (Attribute msg) -> String -> ActionResult a -> Html msg
actionButtonView attributes label result =
    let
        buttonContent =
            case result of
                Loading ->
                    fa "spinner fa-spin"

                _ ->
                    text label

        buttonAttributes =
            [ disabled (result == Loading) ] ++ attributes
    in
    Html.button buttonAttributes [ buttonContent ]
