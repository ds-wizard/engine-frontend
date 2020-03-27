module Wizard.Public.Login.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, style, type_)
import Html.Events exposing (..)
import Markdown
import Shared.Html exposing (fa, faSet)
import Shared.Locale exposing (l, lg, lx)
import Wizard.Common.Api.Auth as AuthApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.AuthServiceConfig exposing (AuthServiceConfig)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.Login.View"


lx_ : String -> AppState -> Html Msg
lx_ =
    lx "Wizard.Public.Login.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        form =
            formView appState model

        splitScreenClass =
            "col-12 d-flex align-items-center"

        content =
            case appState.config.info.loginInfo of
                Just loginInfo ->
                    [ div [ class <| splitScreenClass ++ " justify-content-start col-lg-7 col-md-6 side-info" ]
                        [ Markdown.toHtml [] loginInfo ]
                    , div [ class <| splitScreenClass ++ " justify-content-center col-lg-5 col-md-6 side-login" ]
                        [ form ]
                    ]

                Nothing ->
                    [ form ]
    in
    div [ class "row justify-content-center Public__Login" ]
        content


externalButton : AppState -> AuthServiceConfig -> Html Msg
externalButton appState config =
    let
        background =
            config.style
                |> Maybe.andThen .background
                |> Maybe.withDefault "#333"

        color =
            config.style
                |> Maybe.andThen .color
                |> Maybe.withDefault "#fff"

        icon =
            config.style
                |> Maybe.andThen .icon
                |> Maybe.map (\i -> fa i)
                |> Maybe.withDefault (faSet "login.externalService" appState)
    in
    a
        [ class "btn external-login-button"
        , href <| AuthApi.authRedirectUrl config.id appState
        , style "color" color
        , style "background" background
        ]
        [ icon, text config.name ]


formView : AppState -> Model -> Html Msg
formView appState model =
    let
        loginForm =
            [ div [ class "form-group" ]
                [ span [ class "input-icon" ] [ fa "fas fa-envelope" ]
                , input [ onInput Email, id "email", type_ "text", class "form-control", placeholder <| lg "user.email" appState ] []
                ]
            , div [ class "form-group" ]
                [ span [ class "input-icon" ] [ fa "fas fa-key" ]
                , input [ onInput Password, id "password", type_ "password", class "form-control", placeholder <| lg "user.password" appState ] []
                ]
            , div [ class "form-group d-flex align-items-baseline justify-content-between" ]
                [ linkTo appState (Routes.PublicRoute ForgottenPasswordRoute) [] [ lx_ "form.link" appState ]
                , ActionButton.submit appState <| ActionButton.SubmitConfig (l_ "form.submit" appState) model.loggingIn
                ]
            ]

        externalLogin =
            if List.length appState.config.auth.external.services > 0 then
                [ div [ class "external-login-separator" ]
                    [ lx_ "connectWith" appState ]
                ]
                    ++ List.map (externalButton appState) appState.config.auth.external.services

            else
                []
    in
    div [ class "align-self-center col-xs-10 col-sm-8 col-md-6 col-lg-4" ]
        [ form [ onSubmit DoLogin, class "card bg-light" ]
            [ div [ class "card-header" ] [ lx_ "form.title" appState ]
            , div [ class "card-body" ]
                ([ FormResult.view appState model.loggingIn ]
                    ++ loginForm
                    ++ externalLogin
                )
            ]
        ]
