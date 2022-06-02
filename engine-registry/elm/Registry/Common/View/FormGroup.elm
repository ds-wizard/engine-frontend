module Registry.Common.View.FormGroup exposing
    ( input
    , password
    , textarea
    )

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, div, label, p, text)
import Html.Attributes exposing (class, for, id, name)
import Registry.Common.AppState exposing (AppState)
import Shared.Form exposing (errorToString)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)


{-| Helper for creating form group with text input field.
-}
input : AppState -> Form FormError o -> String -> String -> Html Form.Msg
input =
    formGroup Input.textInput []


{-| Helper for creating form group with password input field.
-}
password : AppState -> Form FormError o -> String -> String -> Html Form.Msg
password =
    formGroup Input.passwordInput []


{-| Helper for creating form group with textarea.
-}
textarea : AppState -> Form FormError o -> String -> String -> Html Form.Msg
textarea =
    formGroup Input.textArea []


{-| Create Html for a form field using the given input field.
-}
formGroup : Input.Input FormError String -> List (Html.Attribute Form.Msg) -> AppState -> Form FormError o -> String -> String -> Html.Html Form.Msg
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


{-| Get Html and form group error class for a given field. If the field
contains no errors, the returned Html and error class are empty.
-}
getErrors : AppState -> Form.FieldState FormError String -> String -> ( Html msg, String )
getErrors appState field labelText =
    case field.liveError of
        Just error ->
            ( p [ class "invalid-feedback" ] [ text (errorToString appState labelText error) ], "is-invalid" )

        Nothing ->
            ( emptyNode, "" )
