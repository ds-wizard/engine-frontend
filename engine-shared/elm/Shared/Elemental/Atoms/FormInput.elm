module Shared.Elemental.Atoms.FormInput exposing (richRadioGroup, tagsGroup, text, textWithAttrs)

import Css exposing (..)
import Css.Global exposing (descendants, typeSelector)
import Css.Transitions exposing (transition)
import Form exposing (Form, Msg(..))
import Form.Field as Field
import Html.Styled as Html exposing (Attribute, Html, div, input, label, p, span, strong)
import Html.Styled.Attributes as Attributes exposing (autocomplete, class, css, for, id, type_, value)
import Html.Styled.Events exposing (onBlur, onCheck, onFocus, onInput)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Shadow as Shadow
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Transition as Transition
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (colorD10, colorL40, px2rem)
import Shared.Form.FormError exposing (FormError)


text : Form FormError o -> String -> Theme -> Html Form.Msg
text =
    textWithAttrs []


textWithAttrs : List (Attribute Form.Msg) -> Form FormError o -> String -> Theme -> Html Form.Msg
textWithAttrs attributes form fieldName theme =
    let
        field =
            Form.getFieldAsString fieldName form
    in
    text_ theme field (id fieldName :: attributes)


richRadioGroup : List ( String, String, String ) -> Form FormError o -> String -> Theme -> Html Form.Msg
richRadioGroup options form fieldName theme =
    let
        field =
            Form.getFieldAsString fieldName form

        optionStyle =
            [ Spacing.insetMD
            , Spacing.stackMD
            , Border.default theme
            , displayFlex
            , alignItems center
            , cursor pointer
            , descendants
                [ typeSelector "input"
                    [ Spacing.inlineMD
                    ]
                , typeSelector "span"
                    [ display block
                    , width (pct 100)
                    , descendants
                        [ typeSelector "strong" [ Typography.heading3 theme ]
                        , typeSelector "p" [ Typography.copy1 theme ]
                        ]
                    ]
                ]
            ]

        selectedStyles k =
            if field.value == Just k then
                [ important (borderColor (colorL40 theme.colors.primary))
                , backgroundColor (colorL40 theme.colors.primary)
                , descendants
                    [ typeSelector "span"
                        [ descendants
                            [ typeSelector "strong" [ important (color (colorD10 theme.colors.primary)) ]
                            , typeSelector "p" [ important (color (colorD10 theme.colors.primary)) ]
                            ]
                        ]
                    ]
                ]

            else
                []

        buildOption ( k, v, d ) =
            label [ css optionStyle, css (selectedStyles k) ]
                [ input
                    [ value k
                    , Attributes.checked (field.value == Just k)
                    , class "form-check-input"
                    , type_ "radio"
                    , id k
                    , onCheck (\_ -> Input field.path Form.Text <| Field.String k)
                    ]
                    []
                , span [ class "form-check-label", for k ]
                    [ strong [] [ Html.text v ]
                    , p [ class "form-text text-muted" ] [ Html.text d ]
                    ]
                ]
    in
    div [ class "form-radio-group" ] (List.map buildOption options)


tagsGroup : List Tag -> Form FormError o -> String -> Theme -> Html Form.Msg
tagsGroup tags form fieldName theme =
    let
        containerStyles =
            [ marginTop (px2rem Spacing.lg) ]

        labelStyles tagColor =
            [ Typography.copy2contrast theme tagColor
            , Border.roundedFull
            , Spacing.insetSquishSM
            , Spacing.stackXS
            , fontWeight bold
            , backgroundColor tagColor
            , display inlineBlock
            , cursor pointer
            , descendants
                [ typeSelector "input"
                    [ Spacing.inlineSM
                    ]
                ]
            ]

        descriptionStyles =
            [ Typography.copy1 theme
            , Spacing.stackMD
            , marginLeft (px2rem Spacing.sm)
            ]

        viewTag tag =
            let
                field =
                    Form.getFieldAsBool (fieldName ++ "." ++ tag.uuid) form
            in
            div []
                [ label [ css (labelStyles (hex tag.color)) ]
                    [ input
                        [ type_ "checkbox"
                        , Attributes.checked (Maybe.withDefault False field.value)
                        , onCheck (Form.Input field.path Form.Checkbox << Field.Bool)
                        , onFocus (Form.Focus field.path)
                        , onBlur (Form.Blur field.path)
                        ]
                        []
                    , Html.text tag.name
                    ]
                , p [ css descriptionStyles ] [ Html.text (Maybe.withDefault "" tag.description) ]
                ]
    in
    div [ css containerStyles ] (List.map viewTag tags)


text_ : Theme -> Form.FieldState e String -> List (Attribute Form.Msg) -> Html Form.Msg
text_ =
    baseInput "text" Field.String Form.Text


inputStyle : Theme -> Style
inputStyle theme =
    Css.batch
        [ Typography.copy1 theme
        , Spacing.insetSM
        , Spacing.stackSM
        , Border.default theme
        , width (pct 100)
        , outline none
        , transition
            [ Transition.default Css.Transitions.boxShadow3
            , Transition.default Css.Transitions.borderColor3
            ]
        , focus
            [ borderColor (colorL40 theme.colors.primary)
            , Shadow.outlinePrimary theme
            ]
        ]


baseInput : String -> (String -> Field.FieldValue) -> Form.InputType -> Theme -> (Form.FieldState e String -> List (Attribute Form.Msg) -> Html Form.Msg)
baseInput t toFieldValue inputType theme state attrs =
    let
        formAttrs =
            [ type_ t
            , value (state.value |> Maybe.withDefault "")
            , onInput (toFieldValue >> Form.Input state.path inputType)
            , onFocus (Form.Focus state.path)
            , onBlur (Form.Blur state.path)
            ]
    in
    input (css [ inputStyle theme ] :: formAttrs ++ attrs) []
