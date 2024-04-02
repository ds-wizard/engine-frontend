module Registry.Components.FormGroup exposing
    ( input
    , password
    , textarea
    )

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, div, label, p, text)
import Html.Attributes exposing (class, for, id, name)
import Registry.Data.AppState exposing (AppState)
import Shared.Form exposing (errorToString)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)


input : AppState -> Form FormError o -> String -> String -> Html Form.Msg
input =
    formGroup Input.textInput []


password : AppState -> Form FormError o -> String -> String -> Html Form.Msg
password =
    formGroup Input.passwordInput []


textarea : AppState -> Form FormError o -> String -> String -> Html Form.Msg
textarea =
    formGroup Input.textArea []


formGroup : Input.Input FormError String -> List (Html.Attribute Form.Msg) -> AppState -> Form FormError o -> String -> String -> Html.Html Form.Msg
formGroup inputFn attrs appState form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors appState field labelText
    in
    div [ class "form-group my-4" ]
        [ label [ for fieldName ] [ text labelText ]
        , inputFn field (attrs ++ [ class <| "form-control " ++ errorClass, id fieldName, name fieldName ])
        , error
        ]


getErrors : AppState -> Form.FieldState FormError String -> String -> ( Html msg, String )
getErrors appState field labelText =
    case field.liveError of
        Just error ->
            ( p [ class "invalid-feedback" ] [ text (errorToString appState labelText error) ], "is-invalid" )

        Nothing ->
            ( emptyNode, "" )
