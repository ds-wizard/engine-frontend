module Shared.Elemental.Atoms.Form exposing (group, groupSimple, helpText, label, labelBigger)

import Css exposing (..)
import Form exposing (Form)
import Html.Styled as Html exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (class, css)
import Maybe.Extra as Maybe
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Html.Styled exposing (emptyNode)
import Shared.Provisioning exposing (Provisioning)


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


errorText : Theme -> String -> Html msg
errorText theme content =
    let
        styles =
            [ Typography.copy1danger theme
            , Spacing.stackSM
            , marginTop (px2rem -Spacing.sm)
            , textAlign left
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
    , fieldReadableName : String
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


groupSimple : SimpleGroupProps msg o -> GroupData o -> { a | provisioning : Provisioning, theme : Theme } -> Html msg
groupSimple simpleGroupProps =
    group (fromSimpleGroupProps simpleGroupProps)


group : GroupProps msg o -> GroupData o -> { a | provisioning : Provisioning, theme : Theme } -> Html msg
group props { form, fieldName, fieldReadableName, mbFieldLabel, mbTextBefore, mbTextAfter } appState =
    let
        field =
            Form.getFieldAsString fieldName form

        errorNode =
            case field.liveError of
                Just error ->
                    errorText appState.theme (Form.errorToString appState fieldReadableName error)

                Nothing ->
                    emptyNode

        labelNode =
            Maybe.unwrap emptyNode (props.label appState.theme) mbFieldLabel

        inputNode =
            Html.map props.toMsg <|
                props.input form fieldName appState.theme

        textBeforeNode =
            mbTextBefore
                |> Maybe.map (props.textBefore appState.theme)
                |> Maybe.withDefault emptyNode

        textAfterNode =
            mbTextAfter
                |> Maybe.map (props.textAfter appState.theme)
                |> Maybe.withDefault emptyNode

        styles =
            [ Spacing.stackMD
            , width (pct 100)
            ]
    in
    div [ css styles, class "form-group" ]
        [ labelNode
        , textBeforeNode
        , inputNode
        , errorNode
        , textAfterNode
        ]
