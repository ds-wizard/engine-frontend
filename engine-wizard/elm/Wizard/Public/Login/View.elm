module Wizard.Public.Login.View exposing (view)

import Html exposing (Html, div, form, input, span)
import Html.Attributes exposing (class, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Shared.Html exposing (fa)
import Shared.Locale exposing (l, lg, lx)
import Shared.Markdown as Markdown
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
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
            case appState.config.lookAndFeel.loginInfo of
                Just loginInfo ->
                    [ div
                        [ class <| splitScreenClass ++ " justify-content-start col-lg-7 col-md-6 side-info"
                        , dataCy "login_side-info"
                        ]
                        [ Markdown.toHtml [] loginInfo ]
                    , div [ class <| splitScreenClass ++ " justify-content-center col-lg-5 col-md-6 side-login" ]
                        [ form ]
                    ]

                Nothing ->
                    [ form ]
    in
    div [ class "row justify-content-center Public__Login" ]
        content


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
                [ linkTo appState Routes.publicForgottenPassword [] [ lx_ "form.link" appState ]
                , ActionButton.submit appState <| ActionButton.SubmitConfig (l_ "form.submit" appState) model.loggingIn
                ]
            ]

        externalLogin =
            if List.length appState.config.authentication.external.services > 0 then
                div [ class "external-login-separator", dataCy "login_external_separator" ]
                    [ lx_ "connectWith" appState ]
                    :: List.map (ExternalLoginButton.view appState) appState.config.authentication.external.services

            else
                []
    in
    div [ class "align-self-center col-xs-10 col-sm-8 col-md-6 col-lg-4" ]
        [ form [ onSubmit DoLogin, class "card bg-light" ]
            [ div [ class "card-header" ] [ lx_ "form.title" appState ]
            , div [ class "card-body" ]
                (FormResult.view appState model.loggingIn
                    :: loginForm
                    ++ externalLogin
                )
            ]
        ]
