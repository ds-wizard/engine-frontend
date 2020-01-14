module Wizard.Common.View.ActionButton exposing
    ( ButtonConfig
    , SubmitConfig
    , button
    , submit
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (faSet)


type alias ButtonConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


button : AppState -> ButtonConfig a msg -> Html msg
button appState cfg =
    let
        cssClass =
            if cfg.dangerous then
                "btn-danger"

            else
                "btn-primary"
    in
    actionButtonView appState [ onClick cfg.msg, class <| "btn btn-with-loader " ++ cssClass ] cfg.label cfg.result


type alias SubmitConfig a =
    { label : String
    , result : ActionResult a
    }


submit : AppState -> SubmitConfig a -> Html msg
submit appState { label, result } =
    actionButtonView appState [ type_ "submit", class "btn btn-primary btn-with-loader" ] label result


actionButtonView : AppState -> List (Attribute msg) -> String -> ActionResult a -> Html msg
actionButtonView appState attributes label result =
    let
        buttonContent =
            case result of
                Loading ->
                    faSet "_global.spinner" appState

                _ ->
                    text label

        buttonAttributes =
            [ disabled (result == Loading) ] ++ attributes
    in
    Html.button buttonAttributes [ buttonContent ]
