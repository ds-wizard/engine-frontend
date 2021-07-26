module Wizard.Common.View.ActionButton exposing
    ( ButtonConfig
    , ButtonExtraConfig
    , ButtonWithAttrsConfig
    , SubmitConfig
    , button
    , buttonExtra
    , buttonWithAttrs
    , submit
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Html exposing (faSet)
import String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


type alias ButtonConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


button : AppState -> ButtonConfig a msg -> Html msg
button appState cfg =
    actionButtonView appState
        [ onClick cfg.msg, class <| "btn btn-with-loader " ++ buttonClass cfg.dangerous ]
        [ text cfg.label ]
        cfg.result


type alias ButtonWithAttrsConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    , attrs : List (Attribute msg)
    }


buttonWithAttrs : AppState -> ButtonWithAttrsConfig a msg -> Html msg
buttonWithAttrs appState cfg =
    actionButtonView appState
        ([ onClick cfg.msg, class <| "btn btn-with-loader " ++ buttonClass cfg.dangerous ] ++ cfg.attrs)
        [ text cfg.label ]
        cfg.result


type alias ButtonExtraConfig a msg =
    { content : List (Html msg)
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


buttonExtra : AppState -> ButtonExtraConfig a msg -> Html msg
buttonExtra appState cfg =
    actionButtonView appState
        [ onClick cfg.msg, class <| "btn btn-with-loader link-with-icon " ++ buttonClass cfg.dangerous ]
        cfg.content
        cfg.result


type alias SubmitConfig a =
    { label : String
    , result : ActionResult a
    }


submit : AppState -> SubmitConfig a -> Html msg
submit appState { label, result } =
    actionButtonView appState
        [ type_ "submit"
        , class "btn btn-primary btn-with-loader"
        , dataCy "form_submit"
        ]
        [ text label ]
        result


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


buttonClass : Bool -> String
buttonClass dangerous =
    if dangerous then
        "btn-danger"

    else
        "btn-primary"
