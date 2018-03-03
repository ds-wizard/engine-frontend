module Public.Login.View exposing (view)

import Common.Html exposing (linkTo)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, type_)
import Html.Events exposing (..)
import Msgs
import Public.Login.Models exposing (Model)
import Public.Login.Msgs exposing (Msg(..))
import Public.Routing exposing (Route(ForgottenPassword))
import Routing exposing (Route(Public))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "public__login" ]
        [ loginForm wrapMsg model ]


loginForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
loginForm wrapMsg model =
    form [ onSubmit (wrapMsg Login), class "well col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-6 col-md-offset-3 col-lg-4 col-lg-offset-4" ]
        [ fieldset []
            [ legend [] [ text "Log in" ]
            , loginError model
            , div [ class "form-group" ]
                [ label [ class "control-label" ]
                    [ text "Email" ]
                , input [ onInput (wrapMsg << Email), type_ "text", class "form-control", placeholder "Email" ] []
                ]
            , div [ class "form-group" ]
                [ label [ class "control-label" ]
                    [ text "Password" ]
                , input [ onInput (wrapMsg << Password), type_ "password", class "form-control", placeholder "Password" ] []
                ]
            , div [ class "form-group row public__login__formButtons" ]
                [ div [ class "col-xs-6" ]
                    [ linkTo (Public ForgottenPassword) [] [ text "Forgot your password?" ] ]
                , div [ class "col-xs-6 text-right" ]
                    [ loginButton model ]
                ]
            ]
        ]


loginButton : Model -> Html Msgs.Msg
loginButton model =
    let
        buttonContent =
            if model.loading then
                i [ class "fa fa-spinner fa-spin" ] []
            else
                text "Log in"
    in
    button [ type_ "submit", class "btn btn-primary", disabled model.loading ]
        [ buttonContent ]


loginError : Model -> Html Msgs.Msg
loginError model =
    if model.error == "" then
        text ""
    else
        div [ class "alert alert-danger" ]
            [ text model.error ]
