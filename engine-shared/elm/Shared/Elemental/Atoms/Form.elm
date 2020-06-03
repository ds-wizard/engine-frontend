module Shared.Elemental.Atoms.Form exposing (group, groupSimple, helpText, label, labelBigger)

import Css exposing (..)
import Form exposing (Form)
import Html.Styled as Html exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (css)
import Maybe.Extra as Maybe
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Form.FormError exposing (FormError)
import Shared.Html.Styled exposing (emptyNode)


label : Theme -> String -> Html msg
label theme =
    label_ [ Typography.heading3 theme ]


labelBigger : Theme -> String -> Html msg
labelBigger theme =
    label_ [ Typography.heading2 theme ]


label_ : List Style -> String -> Html msg
label_ styles labelText =
    let
        commonStyles =
            [ Spacing.stackSM
            , display block
            , width (pct 100)
            ]
    in
    Html.label [ css (styles ++ commonStyles) ] [ text labelText ]


helpText : Theme -> String -> Html msg
helpText theme content =
    let
        styles =
            [ Typography.copy1lighter theme
            , Spacing.stackSM
            ]
    in
    p [ css styles ] [ text content ]


type alias GroupProps msg o =
    { label : Theme -> String -> Html msg
    , input : Form FormError o -> String -> Theme -> Html Form.Msg
    , textBefore : Theme -> String -> Html msg
    , textAfter : Theme -> String -> Html msg
    , toMsg : Form.Msg -> msg
    }


type alias SimpleGroupProps msg o =
    { input : Form FormError o -> String -> Theme -> Html Form.Msg
    , toMsg : Form.Msg -> msg
    }


type alias GroupData o =
    { form : Form FormError o
    , fieldName : String
    , mbFieldLabel : Maybe String
    , mbTextBefore : Maybe String
    , mbTextAfter : Maybe String
    }


fromSimpleGroupProps : SimpleGroupProps msg o -> GroupProps msg o
fromSimpleGroupProps { input, toMsg } =
    { label = label
    , input = input
    , textBefore = helpText
    , textAfter = helpText
    , toMsg = toMsg
    }


groupSimple : SimpleGroupProps msg o -> GroupData o -> Theme -> Html msg
groupSimple simpleGroupProps =
    group (fromSimpleGroupProps simpleGroupProps)


group : GroupProps msg o -> GroupData o -> Theme -> Html msg
group props { form, fieldName, mbFieldLabel, mbTextBefore, mbTextAfter } theme =
    let
        labelNode =
            Maybe.unwrap emptyNode (props.label theme) mbFieldLabel

        inputNode =
            Html.map props.toMsg <|
                props.input form fieldName theme

        textBeforeNode =
            mbTextBefore
                |> Maybe.map (props.textBefore theme)
                |> Maybe.withDefault emptyNode

        textAfterNode =
            mbTextAfter
                |> Maybe.map (props.textAfter theme)
                |> Maybe.withDefault emptyNode

        styles =
            [ Spacing.stackMD
            , width (pct 100)
            ]
    in
    div [ css styles ]
        [ labelNode
        , textBeforeNode
        , inputNode
        , textAfterNode
        ]
