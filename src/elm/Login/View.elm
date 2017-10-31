module Login.View exposing (..)

import Html exposing (Html, button, div, fieldset, form, h2, input, label, legend, span, text)
import Html.Attributes exposing (class, placeholder, type_)
import Msgs exposing (Msg)


view : Html Msg
view =
    div [ class "login" ]
        [ header
        , loginForm
        ]


header : Html Msg
header =
    div [ class "navbar navbar-default navbar-fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ span [ class "navbar-brand" ] [ text "Elixir DSP" ] ]
            ]
        ]


loginForm : Html Msg
loginForm =
    form [ class "well col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-6 col-md-offset-3 col-lg-4 col-lg-offset-4" ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , div [ class "form-group" ]
                [ label [ class "control-label" ]
                    [ text "Email" ]
                , input [ type_ "text", class "form-control", placeholder "Email" ] []
                ]
            , div [ class "form-group" ]
                [ label [ class "control-label" ]
                    [ text "Password" ]
                , input [ type_ "text", class "form-control", placeholder "Password" ] []
                ]
            , div [ class "form-group text-right" ]
                [ button [ type_ "submit", class "btn btn-primary" ]
                    [ text "Login" ]
                ]
            ]
        ]
