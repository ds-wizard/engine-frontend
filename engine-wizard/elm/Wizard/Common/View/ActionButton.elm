module Wizard.Common.View.ActionButton exposing
    ( ButtonConfig
    , ButtonExtraConfig
    , SubmitConfig
    , button
    , buttonExtra
    , submit
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Html exposing (faSet)
import String
import Wizard.Common.AppState exposing (AppState)


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
    actionButtonView appState [ onClick cfg.msg, class <| "btn btn-with-loader " ++ cssClass ] [ text cfg.label ] cfg.result


type alias ButtonExtraConfig a msg =
    { content : List (Html msg)
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


buttonExtra : AppState -> ButtonExtraConfig a msg -> Html msg
buttonExtra appState cfg =
    let
        cssClass =
            if cfg.dangerous then
                "btn-danger"

            else
                "btn-primary"
    in
    actionButtonView appState [ onClick cfg.msg, class <| "btn btn-with-loader link-with-icon " ++ cssClass ] cfg.content cfg.result


type alias SubmitConfig a =
    { label : String
    , result : ActionResult a
    }


submit : AppState -> SubmitConfig a -> Html msg
submit appState { label, result } =
    actionButtonView appState [ type_ "submit", class "btn btn-primary btn-with-loader" ] [ text label ] result


actionButtonView : AppState -> List (Attribute msg) -> List (Html msg) -> ActionResult a -> Html msg
actionButtonView appState attributes content result =
    let
        buttonContent =
            case result of
                Loading ->
                    [ faSet "_global.spinner" appState ]

                _ ->
                    content

        buttonAttributes =
            [ disabled (result == Loading) ] ++ attributes
    in
    Html.button buttonAttributes buttonContent
