module UserManagement.Edit.View exposing (..)

import Common.Html exposing (defaultFullPageError, fullPageLoader, pageHeader)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Msgs
import UserManagement.Edit.Models exposing (Model)
import UserManagement.Edit.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Edit user profile" []
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    if model.loading then
        fullPageLoader
    else if model.loadingError /= "" then
        defaultFullPageError model.loadingError
    else
        div []
            [ editFormView model
            , passwordFormView model
            ]


editFormView : Model -> Html Msgs.Msg
editFormView model =
    div [ class "well" ]
        [ editForm model.editForm model.editError |> Html.map (EditFormMsg >> Msgs.UserManagementEditMsg)
        , div [ class "text-right" ]
            [ saveButton model.editSaving (Msgs.UserManagementEditMsg <| EditFormMsg Form.Submit)
            ]
        ]


editForm : Form () UserEditForm -> String -> Html Form.Msg
editForm form error =
    let
        email =
            Form.getFieldAsString "email" form

        name =
            Form.getFieldAsString "name" form

        surname =
            Form.getFieldAsString "surname" form

        role =
            Form.getFieldAsString "role" form

        roleOptions =
            ( "", "--" ) :: List.map (\o -> ( o, o )) roles
    in
    div []
        [ legend [] [ text "Profile" ]
        , errorView error
        , inputGroup Input.textInput email "Email"
        , inputGroup Input.textInput name "Name"
        , inputGroup Input.textInput surname "Surname"
        , selectGroup roleOptions role "Role"
        ]


passwordFormView : Model -> Html Msgs.Msg
passwordFormView model =
    div [ class "well" ]
        [ passwordForm model.passwordForm model.passwordError |> Html.map (PasswordFormMsg >> Msgs.UserManagementEditMsg)
        , div [ class "text-right" ]
            [ saveButton model.passwordSaving (Msgs.UserManagementEditMsg <| PasswordFormMsg Form.Submit)
            ]
        ]


passwordForm : Form UserPasswordFormError UserPasswordForm -> String -> Html Form.Msg
passwordForm form error =
    let
        password =
            Form.getFieldAsString "password" form

        passwordConfirmation =
            Form.getFieldAsString "passwordConfirmation" form
    in
    div []
        [ legend [] [ text "Password" ]
        , errorView error
        , inputGroup Input.passwordInput password "New password"
        , inputGroup Input.passwordInput passwordConfirmation "New password again"
        ]


errorView : String -> Html msg
errorView error =
    if error /= "" then
        div [ class "alert alert-danger" ]
            [ i [ class "fa fa-exclamation-triangle" ] []
            , text error
            ]
    else
        text ""


inputGroup : Input.Input e String -> Form.FieldState e String -> String -> Html Form.Msg
inputGroup input field labelText =
    let
        ( error, errorClass ) =
            getErrors field
    in
    div [ class ("form-group " ++ errorClass) ]
        [ label [ class "control-label" ] [ text labelText ]
        , input field [ class "form-control" ]
        , error
        ]


selectGroup : List ( String, String ) -> Form.FieldState e String -> String -> Html Form.Msg
selectGroup options field labelText =
    let
        ( error, errorClass ) =
            getErrors field
    in
    div [ class ("form-group " ++ errorClass) ]
        [ label [ class "control-label" ] [ text labelText ]
        , Input.selectInput options field [ class "form-control" ]
        , error
        ]


getErrors : Form.FieldState e String -> ( Html Form.Msg, String )
getErrors field =
    case field.liveError of
        Just error ->
            ( p [ class "help-block" ] [ text (toString error) ], "has-error" )

        Nothing ->
            ( text "", "" )


saveButton : Bool -> Msgs.Msg -> Html Msgs.Msg
saveButton saving msg =
    let
        buttonContent =
            if saving then
                i [ class "fa fa-spinner fa-spin" ] []
            else
                text "Save"
    in
    button
        [ class "btn btn-primary btn-with-loader"
        , disabled saving
        , onClick msg
        ]
        [ buttonContent ]
