module Common.View.Forms exposing (..)

import Common.Form exposing (CustomFormError(..))
import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Form.Error exposing (ErrorValue(..))
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs exposing (Msg)
import Routing exposing (Route)


-- Form fields


{-| Create Html for a form field using the given input field.
-}
formGroup : Input.Input CustomFormError String -> Form CustomFormError o -> String -> String -> Html.Html Form.Msg
formGroup input form fieldName labelText =
    let
        field =
            Form.getFieldAsString fieldName form

        ( error, errorClass ) =
            getErrors field labelText
    in
    div [ class "form-group" ]
        [ label [ for fieldName ] [ text labelText ]
        , input field [ class <| "form-control " ++ errorClass, id fieldName, name fieldName ]
        , error
        ]


{-| Helper for creating form group with text input field.
-}
inputGroup : Form CustomFormError o -> String -> String -> Html.Html Form.Msg
inputGroup =
    formGroup Input.textInput


{-| Helper for creating form group with password input field.
-}
passwordGroup : Form CustomFormError o -> String -> String -> Html.Html Form.Msg
passwordGroup =
    formGroup Input.passwordInput


{-| Helper for creating form group with select field.
-}
selectGroup : List ( String, String ) -> Form CustomFormError o -> String -> String -> Html.Html Form.Msg
selectGroup options =
    formGroup (Input.selectInput options)


{-| Helper for creating form group with textarea.
-}
textAreaGroup : Form CustomFormError o -> String -> String -> Html.Html Form.Msg
textAreaGroup =
    formGroup Input.textArea


{-| Helper for creating form group with toggle
-}
toggleGroup : Form CustomFormError o -> String -> String -> Html.Html Form.Msg
toggleGroup form fieldName labelText =
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


{-| Plain group is same Html as formGroup but without any input fields. It only
shows label with read only Html value.
-}
plainGroup : Html.Html msg -> String -> Html.Html msg
plainGroup valueHtml labelText =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text labelText ]
        , valueHtml
        ]


{-| Helper for creating plain group with text value.
-}
textGroup : String -> String -> Html.Html msg
textGroup value =
    plainGroup <|
        p [ class "form-value" ] [ text value ]


{-| Helper for creating plain group with code block.
-}
codeGroup : String -> String -> Html.Html msg
codeGroup value =
    plainGroup <|
        code [] [ text value ]


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

        CustomError err ->
            case err of
                ConfirmationError ->
                    "Passwords don't match"

                ServerValidationError msg ->
                    msg

        _ ->
            toString error



-- Form Actions


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
formActions : Route -> ( String, ActionResult a, Msg ) -> Html Msg
formActions cancelRoute actionButtonSettings =
    div [ class "form-actions" ]
        [ linkTo cancelRoute [ class "btn btn-secondary" ] [ text "Cancel" ]
        , actionButton actionButtonSettings
        ]


{-| Similar to formActions, but it contains only the action button.
-}
formActionOnly : ( String, ActionResult a, Msg ) -> Html Msg
formActionOnly actionButtonSettings =
    div [ class "text-right" ]
        [ actionButton actionButtonSettings ]


{-| Action button invokes a message when clicked. It's state is defined by
the ActionResult. If the state is Loading action button is disabled and
a loader is shown instead of action name.
-}
actionButton : ( String, ActionResult a, Msg ) -> Html Msg
actionButton ( label, result, msg ) =
    actionButtonView [ onClick msg ] label result


submitButton : ( String, ActionResult a ) -> Html Msg
submitButton ( label, result ) =
    actionButtonView [ type_ "submit" ] label result


actionButtonView : List (Attribute msg) -> String -> ActionResult a -> Html msg
actionButtonView attributes label result =
    let
        buttonContent =
            case result of
                Loading ->
                    i [ class "fa fa-spinner fa-spin" ] []

                _ ->
                    text label

        buttonAttributes =
            [ class "btn btn-primary btn-with-loader", disabled (result == Loading) ] ++ attributes
    in
    button buttonAttributes [ buttonContent ]



-- Status Views


formResultView : ActionResult String -> Html Msgs.Msg
formResultView result =
    case result of
        Success msg ->
            successView msg

        Error msg ->
            errorView msg

        _ ->
            emptyNode


formSuccessResultView : ActionResult String -> Html Msgs.Msg
formSuccessResultView result =
    case result of
        Success msg ->
            successView msg

        _ ->
            emptyNode


errorView : String -> Html Msgs.Msg
errorView =
    statusView "alert-danger" "fa-exclamation-triangle"


successView : String -> Html Msgs.Msg
successView =
    statusView "alert-success" "fa-check"


statusView : String -> String -> String -> Html Msgs.Msg
statusView className icon msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ i [ class ("fa " ++ icon) ] []
            , text msg
            ]
    else
        text ""
