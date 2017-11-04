module UserManagement.Create.View exposing (..)

import Common.Html exposing (linkTo, pageHeader)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msgs
import Routing exposing (Route(..))
import UserManagement.Create.Models exposing (Model)
import UserManagement.Create.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)


view : Model -> Html Msgs.Msg
view model =
    div [ class "col-xs-12 col-lg-10 col-lg-offset-1" ]
        [ pageHeader "Create user" []
        , errorView model
        , formView model.form |> Html.map FormMsg |> Html.map Msgs.UserManagementCreateMsg
        , formActions model
        ]


errorView : Model -> Html Msgs.Msg
errorView model =
    if model.error /= "" then
        div [ class "alert alert-danger" ]
            [ i [ class "fa fa-exclamation-triangle" ] []
            , text model.error
            ]
    else
        text ""


formView : Form () UserCreateForm -> Html Form.Msg
formView form =
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

        password =
            Form.getFieldAsString "password" form
    in
    div []
        [ inputGroup Input.textInput email "Email"
        , inputGroup Input.textInput name "Name"
        , inputGroup Input.textInput surname "Surname"
        , selectGroup roleOptions role "Role"
        , inputGroup Input.passwordInput password "Password"
        ]


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


formActions : Model -> Html Msgs.Msg
formActions model =
    div [ class "form-actions" ]
        [ linkTo UserManagement [ class "btn btn-default" ] [ text "Cancel" ]
        , saveButton model
        ]


saveButton : Model -> Html Msgs.Msg
saveButton model =
    let
        buttonContent =
            if model.savingUser then
                i [ class "fa fa-spinner fa-spin" ] []
            else
                text "Save"
    in
    button
        [ class "btn btn-primary btn-with-loader"
        , disabled model.savingUser
        , onClick (Msgs.UserManagementCreateMsg <| UserManagement.Create.Msgs.FormMsg Form.Submit)
        ]
        [ buttonContent ]
