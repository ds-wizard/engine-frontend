module Shared.Components.ActionButton exposing
    ( ButtonConfig
    , ButtonCustomConfig
    , ButtonWithAttrsConfig
    , SubmitConfig
    , SubmitWithAttrsConfig
    , button
    , buttonCustom
    , buttonWithAttrs
    , submit
    , submitWithAttrs
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class, disabled, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Shared.Components.FontAwesome exposing (faSpinner)


type alias ButtonConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    }


button : ButtonConfig a msg -> Html msg
button cfg =
    actionButtonView
        [ onClick cfg.msg, class <| "btn btn-wide " ++ buttonClass cfg.dangerous ]
        [ text cfg.label ]
        cfg.result


type alias ButtonWithAttrsConfig a msg =
    { label : String
    , result : ActionResult a
    , msg : msg
    , dangerous : Bool
    , attrs : List (Attribute msg)
    }


buttonWithAttrs : ButtonWithAttrsConfig a msg -> Html msg
buttonWithAttrs cfg =
    actionButtonView
        ([ onClick cfg.msg, class <| "btn btn-wide " ++ buttonClass cfg.dangerous ] ++ cfg.attrs)
        [ text cfg.label ]
        cfg.result


type alias ButtonCustomConfig a msg =
    { content : List (Html msg)
    , result : ActionResult a
    , msg : msg
    , btnClass : String
    }


buttonCustom : ButtonCustomConfig a msg -> Html msg
buttonCustom cfg =
    actionButtonView
        [ onClick cfg.msg, class <| "btn btn-wide " ++ cfg.btnClass ]
        cfg.content
        cfg.result


type alias SubmitConfig a =
    { label : String
    , result : ActionResult a
    }


submit : SubmitConfig a -> Html msg
submit { label, result } =
    submitWithAttrs
        { label = label
        , result = result
        , attrs = []
        }


type alias SubmitWithAttrsConfig a msg =
    { label : String
    , result : ActionResult a
    , attrs : List (Attribute msg)
    }


submitWithAttrs : SubmitWithAttrsConfig a msg -> Html msg
submitWithAttrs { label, result, attrs } =
    actionButtonView
        ([ type_ "submit"
         , class "btn btn-primary btn-wide"
         , dataCy "form_submit"
         ]
            ++ attrs
        )
        [ text label ]
        result


actionButtonView : List (Attribute msg) -> List (Html msg) -> ActionResult a -> Html msg
actionButtonView attributes content result =
    let
        buttonContent =
            case result of
                Loading ->
                    [ faSpinner ]

                _ ->
                    content

        buttonAttributes =
            disabled (result == Loading) :: attributes
    in
    Html.button buttonAttributes buttonContent


buttonClass : Bool -> String
buttonClass dangerous =
    if dangerous then
        "btn-danger"

    else
        "btn-primary"
