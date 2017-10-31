module Auth.View exposing (..)

import Auth.Models exposing (Model)
import Auth.Msgs
import Html exposing (Html, button, div, fieldset, form, h2, input, label, legend, span, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Msgs exposing (Msg)


view : Model -> Html Msg
view model =
    div [ class "login" ]
        [ header
        , loginForm model
        ]


header : Html Msg
header =
    div [ class "navbar navbar-default navbar-fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ span [ class "navbar-brand" ] [ text "Elixir DSP" ] ]
            ]
        ]


loginForm : Model -> Html Msg
loginForm model =
    form [ onSubmit (Msgs.AuthMsg Auth.Msgs.Login), class "well col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-6 col-md-offset-3 col-lg-4 col-lg-offset-4" ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , loginError model
            , div [ class "form-group" ]
                [ label [ class "control-label" ]
                    [ text "Email" ]
                , input [ onInput (Msgs.AuthMsg << Auth.Msgs.Email), type_ "text", class "form-control", placeholder "Email" ] []
                ]
            , div [ class "form-group" ]
                [ label [ class "control-label" ]
                    [ text "Password" ]
                , input [ onInput (Msgs.AuthMsg << Auth.Msgs.Password), type_ "password", class "form-control", placeholder "Password" ] []
                ]
            , div [ class "form-group text-right" ]
                [ button [ type_ "submit", class "btn btn-primary" ]
                    [ text "Login" ]
                ]
            ]
        ]


loginError : Model -> Html Msg
loginError model =
    if model.error == "" then
        text ""
    else
        div [ class "alert alert-danger" ]
            [ text model.error ]
