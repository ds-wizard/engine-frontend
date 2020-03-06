module Registry.Common.View.FormGroup exposing
    ( codeView
    , formGroup
    , getErrors
    , input
    , password
    , select
    , textView
    , textarea
    , toggle
    )

import Form exposing (Form, InputType(..), Msg(..))
import Form.Error exposing (ErrorValue(..))
import Form.Input as Input
import Html exposing (Html, code, div, label, p, span, text)
import Html.Attributes exposing (class, for, id, name)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.FormExtra exposing (CustomFormError(..))
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lf)


l_ : String -> AppState -> String
l_ =
    l "Registry.Common.View.FormGroup"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Registry.Common.View.FormGroup"


{-| Helper for creating form group with text input field.
-}
input : AppState -> Form CustomFormError o -> String -> String -> Html Form.Msg
input =
    formGroup Input.textInput []


{-| Helper for creating form group with password input field.
-}
password : AppState -> Form CustomFormError o -> String -> String -> Html Form.Msg
password =
    formGroup Input.passwordInput []


{-| Helper for creating form group with select field.
-}
select : AppState -> List ( String, String ) -> Form CustomFormError o -> String -> String -> Html Form.Msg
select appState options =
    formGroup (Input.selectInput options) [] appState


{-| Helper for creating form group with textarea.
-}
textarea : AppState -> Form CustomFormError o -> String -> String -> Html Form.Msg
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


{-| Create Html for a form field using the given input field.
-}
formGroup : Input.Input CustomFormError String -> List (Html.Attribute Form.Msg) -> AppState -> Form CustomFormError o -> String -> String -> Html.Html Form.Msg
formGroup inputFn attrs appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors appState field labelText
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
getErrors : AppState -> Form.FieldState CustomFormError String -> String -> ( Html msg, String )
getErrors appState field labelText =
    case field.liveError of
        Just error ->
            ( p [ class "invalid-feedback" ] [ text (toReadable appState error labelText) ], "is-invalid" )

        Nothing ->
            ( emptyNode, "" )


toReadable : AppState -> ErrorValue CustomFormError -> String -> String
toReadable appState error labelText =
    case error of
        Empty ->
            lf_ "error.empty" [ labelText ] appState

        InvalidString ->
            lf_ "error.invalidString" [ labelText ] appState

        InvalidEmail ->
            l_ "error.invalidEmail" appState

        InvalidFloat ->
            l_ "error.invalidFloat" appState

        CustomError err ->
            case err of
                ServerValidationError msg ->
                    msg

                Error msg ->
                    msg

        _ ->
            l_ "error.general" appState
