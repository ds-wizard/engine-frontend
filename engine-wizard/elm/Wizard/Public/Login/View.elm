module Wizard.Public.Login.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, placeholder, type_)
import Html.Events exposing (..)
import Markdown
import Shared.Locale exposing (l, lg, lgx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Common.View exposing (publicForm)
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.Login.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        form =
            loginForm appState model

        splitScreenClass =
            "col-12 d-flex justify-content-center align-items-center"

        content =
            case appState.config.client.loginInfo of
                Just loginInfo ->
                    [ div [ class <| splitScreenClass ++ " col-lg-7 col-md-6 side-info" ]
                        [ Markdown.toHtml [] loginInfo ]
                    , div [ class <| splitScreenClass ++ " col-lg-5 col-md-6 side-login" ]
                        [ form ]
                    ]

                Nothing ->
                    [ form ]
    in
    div [ class "row justify-content-center Public__Login" ]
        content


loginForm : AppState -> Model -> Html Msg
loginForm appState model =
    let
        formContent =
            div []
                [ div [ class "form-group" ]
                    [ label [ for "email" ] [ lgx "user.email" appState ]
                    , input [ onInput Email, id "email", type_ "text", class "form-control", placeholder <| lg "user.email" appState ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "password" ] [ lgx "user.password" appState ]
                    , input [ onInput Password, id "password", type_ "password", class "form-control", placeholder <| lg "user.password" appState ] []
                    ]
                ]

        formConfig =
            { title = l_ "form.title" appState
            , submitMsg = DoLogin
            , actionResult = model.loggingIn
            , submitLabel = l_ "form.submit" appState
            , formContent = formContent
            , link = Just ( Routes.PublicRoute ForgottenPasswordRoute, l_ "form.link" appState )
            }
    in
    publicForm appState formConfig
