module Common.View.FormGroup exposing
    ( codeView
    , color
    , formGroup
    , getErrors
    , input
    , list
    , password
    , richRadioGroup
    , select
    , textView
    , textarea
    , toggle
    )

import Common.Form exposing (CustomFormError(..))
import Common.Html exposing (emptyNode, fa)
import Form exposing (Form, InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Input as Input
import Html exposing (Html, a, button, code, div, label, p, span, text)
import Html.Attributes exposing (checked, class, classList, for, id, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import String exposing (fromFloat)
import Utils exposing (getContrastColorHex)


{-| Helper for creating form group with text input field.
-}
input : Form CustomFormError o -> String -> String -> Html Form.Msg
input =
    formGroup Input.textInput []


{-| Helper for creating form group with password input field.
-}
password : Form CustomFormError o -> String -> String -> Html Form.Msg
password =
    formGroup Input.passwordInput []


{-| Helper for creating form group with select field.
-}
select : List ( String, String ) -> Form CustomFormError o -> String -> String -> Html Form.Msg
select options =
    formGroup (Input.selectInput options) []


richRadioGroup : List ( String, String, String ) -> Form CustomFormError o -> String -> String -> Html Form.Msg
richRadioGroup options =
    let
        radioInput state attrs =
            let
                buildOption ( k, v, d ) =
                    div [ class "form-check", classList [ ( "form-check-selected", state.value == Just k ) ] ]
                        [ Html.input
                            [ value k
                            , checked (state.value == Just k)
                            , class "form-check-input"
                            , type_ "radio"
                            , id k
                            , onInput (Field.String >> Input state.path Form.Text)
                            ]
                            []
                        , label [ class "form-check-label", for k ]
                            [ text v
                            , p [ class "form-text text-muted" ] [ text d ]
                            ]
                        ]
            in
            div [ class "form-radio-group" ] (List.map buildOption options)
    in
    formGroup radioInput []


{-| Helper for creating form group with textarea.
-}
textarea : Form CustomFormError o -> String -> String -> Html Form.Msg
textarea =
    formGroup Input.textArea []


{-| Helper for creating form group with toggle
-}
toggle : Form CustomFormError o -> String -> String -> Html Form.Msg
toggle form fieldName labelText =
    let
        field =
            Form.getFieldAsBool fieldName form
    in
    div [ class "form-check" ]
        [ label [ class "form-check-label form-check-toggle" ]
            [ Input.checkboxInput field [ class "form-check-input" ]
            , span [] [ text labelText ]
            ]
        ]


{-| Helper for creating form group with color input field
-}
color : Form CustomFormError o -> String -> String -> Html Form.Msg
color form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        colorButtons =
            List.map (colorButton field.value fieldName) colorOptions
    in
    div [ class "form-group form-group-color-picker" ]
        [ label [] [ text labelText ]
        , Input.textInput field []
        , div [ class "color-buttons" ] colorButtons
        ]


colorOptions : List String
colorOptions =
    [ "#1ABC9C"
    , "#2ECC71"
    , "#3498DB"
    , "#9B59B6"
    , "#34495E"
    , "#16A085"
    , "#27AE60"
    , "#2980B9"
    , "#8E44AD"
    , "#2C3E50"
    , "#F1C40F"
    , "#E67E22"
    , "#E74C3C"
    , "#ECF0F1"
    , "#95A5A6"
    , "#F39C12"
    , "#D35400"
    , "#C0392B"
    , "#BDC3C7"
    , "#7F8C8D"
    ]


colorButton : Maybe String -> String -> String -> Html Form.Msg
colorButton maybeValue fieldName colorHex =
    let
        isSelected =
            maybeValue == Just colorHex

        check =
            if isSelected then
                fa "check"

            else
                emptyNode
    in
    a
        [ onClick (Input fieldName Text (Field.String colorHex))
        , style "background" colorHex
        , style "color" <| getContrastColorHex colorHex
        , style "border-color" <| getContrastColorHex colorHex
        , classList [ ( "selected", isSelected ) ]
        ]
        [ check ]


list : (Form CustomFormError o -> Int -> Html Form.Msg) -> Form CustomFormError o -> String -> String -> Html Form.Msg
list itemView form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors field labelText
    in
    div [ class "form-group" ]
        [ label [] [ text labelText ]
        , div [] (List.map (itemView form) (Form.getListIndexes fieldName form))
        , div [ class "form-list-error" ] [ error ]
        , button [ class "btn btn-secondary", onClick (Form.Append fieldName) ]
            [ text "Add" ]
        ]


{-| Create Html for a form field using the given input field.
-}
formGroup : Input.Input CustomFormError String -> List (Html.Attribute Form.Msg) -> Form CustomFormError o -> String -> String -> Html.Html Form.Msg
formGroup inputFn attrs form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors field labelText
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , inputFn field (attrs ++ [ class <| "form-control " ++ errorClass, id fieldName, name fieldName ])
        , error
        ]


{-| Helper for creating plain group with text value.
-}
textView : String -> String -> Html.Html msg
textView value =
    plainGroup <|
        p [ class "form-value" ] [ text value ]


{-| Helper for creating plain group with code block.
-}
codeView : String -> String -> Html.Html msg
codeView value =
    plainGroup <|
        code [] [ text value ]


{-| Plain group is same Html as formGroup but without any input fields. It only
shows label with read only Html value.
-}
plainGroup : Html.Html msg -> String -> Html.Html msg
plainGroup valueHtml labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , valueHtml
        ]


{-| Get Html and form group error class for a given field. If the field
contains no errors, the returned Html and error class are empty.
-}
getErrors : Form.FieldState CustomFormError String -> String -> ( Html msg, String )
getErrors field labelText =
    case field.liveError of
        Just error ->
            ( p [ class "invalid-feedback" ] [ text (toReadable error labelText) ], "is-invalid" )

        Nothing ->
            ( text "", "" )


toReadable : ErrorValue CustomFormError -> String -> String
toReadable error labelText =
    case error of
        Empty ->
            labelText ++ " cannot be empty"

        InvalidString ->
            labelText ++ " cannot be empty"

        InvalidEmail ->
            "This is not a valid email"

        InvalidFloat ->
            "This is not a valid number"

        SmallerFloatThan n ->
            "This should not be less than " ++ fromFloat n

        GreaterFloatThan n ->
            "This should not be more than " ++ fromFloat n

        CustomError err ->
            case err of
                ConfirmationError ->
                    "Passwords don't match"

                InvalidUuid ->
                    "This is not a valid UUID"

                ServerValidationError msg ->
                    msg

                Error msg ->
                    msg

        _ ->
            "Invalid value"
