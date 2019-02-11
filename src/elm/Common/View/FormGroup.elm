module Common.View.FormGroup exposing
    ( codeView
    , color
    , formGroup
    , input
    , password
    , select
    , textView
    , textarea
    , toggle
    )

import Common.Form exposing (CustomFormError(..))
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Input as Input
import Html exposing (Html, code, div, label, p, span, text)
import Html.Attributes exposing (class, for, id, name)
import String exposing (fromFloat)


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
color =
    formGroup (Input.baseInput "color" Field.String Form.Text) []


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

        _ ->
            "Invalid value"
