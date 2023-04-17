module Wizard.Public.Login.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, form, input, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, id, pattern, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Html.Keyed
import Maybe.Extra as Maybe
import Shared.Components.MarkdownOrHtml as MarkdownOrHtml
import Shared.Html exposing (emptyNode, fa)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Announcements as Announcements
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ExternalLoginButton as ExternalLoginButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        form =
            if model.codeRequired then
                ( "code", codeFormView appState model )

            else
                ( "login", loginFormView appState model )

        loginInfoSidebar =
            ( "login-info-sidebar"
            , Maybe.unwrap emptyNode (MarkdownOrHtml.view [ class "mt-4" ]) appState.config.dashboardAndLoginScreen.loginInfoSidebar
            )

        content =
            case appState.config.dashboardAndLoginScreen.loginInfo of
                Just loginInfo ->
                    let
                        splitScreenClass =
                            "col-12 d-flex align-items-center"
                    in
                    [ ( "side-info"
                      , div
                            [ class <| splitScreenClass ++ " justify-content-start col-xl-8 col-lg-7 side-info"
                            , dataCy "login_side-info"
                            ]
                            [ MarkdownOrHtml.view [ class "flex-grow-1" ] loginInfo ]
                      )
                    , ( "login-form"
                      , Html.Keyed.node "div"
                            [ class <| splitScreenClass ++ " justify-content-start align-items-stretch flex-column col-xl-4 col-lg-5 col-md-6 col-sm-8 side-login" ]
                            [ form, loginInfoSidebar ]
                      )
                    ]

                Nothing ->
                    [ ( "login-form-only"
                      , Html.Keyed.node "div" [ class "col-xl-4 col-lg-5 col-md-6 col-sm-8" ] [ form, loginInfoSidebar ]
                      )
                    ]

        announcements =
            ( "announcements", Announcements.viewLoginScreen appState.config.dashboardAndLoginScreen.announcements )
    in
    Html.Keyed.node "div"
        [ class "row justify-content-center Public__Login" ]
        (announcements :: content)


loginFormView : AppState -> Model -> Html Msg
loginFormView appState model =
    let
        loginForm =
            [ div [ class "form-group" ]
                [ span [ class "input-icon" ] [ fa "fas fa-envelope" ]
                , input [ onInput Email, id "email", type_ "text", class "form-control", placeholder <| gettext "Email" appState.locale ] []
                ]
            , div [ class "form-group" ]
                [ span [ class "input-icon" ] [ fa "fas fa-key" ]
                , input [ onInput Password, id "password", type_ "password", class "form-control", placeholder <| gettext "Password" appState.locale ] []
                ]
            , div [ class "form-group d-flex align-items-baseline justify-content-between" ]
                [ linkTo appState Routes.publicForgottenPassword [] [ text (gettext "Forgot your password?" appState.locale) ]
                , ActionButton.submit appState <| ActionButton.SubmitConfig (gettext "Log In" appState.locale) model.loggingIn
                ]
            ]

        externalLogin =
            if List.length appState.config.authentication.external.services > 0 then
                div [ class "external-login-separator", dataCy "login_external_separator" ]
                    [ text (gettext "Or connect with" appState.locale) ]
                    :: List.map (ExternalLoginButton.view appState) appState.config.authentication.external.services

            else
                []
    in
    div []
        [ form [ onSubmit DoLogin, class "card bg-light" ]
            [ div [ class "card-header" ] [ text (gettext "Log In" appState.locale) ]
            , div [ class "card-body" ]
                (FormResult.view appState model.loggingIn
                    :: loginForm
                    ++ externalLogin
                )
            ]
        ]


codeFormView : AppState -> Model -> Html Msg
codeFormView appState model =
    div [ class "align-self-center col-xs-10 col-sm-8 col-md-6 col-lg-4" ]
        [ form [ onSubmit DoLogin, class "card bg-light" ]
            [ div [ class "card-header" ] [ text (gettext "Log In" appState.locale) ]
            , div [ class "card-body" ]
                [ FormResult.view appState model.loggingIn
                , p [] [ text (gettext "Please enter the authentication code from your email to verify your identity." appState.locale) ]
                , div [ class "form-group" ]
                    [ span [ class "input-icon" ] [ fa "fas fa-unlock-alt" ]
                    , input
                        [ onInput Code
                        , id "code"
                        , type_ "text"
                        , attribute "inputmode" "numeric"
                        , pattern "[0-9]*"
                        , class "form-control"
                        , placeholder <| gettext "Authentication Code" appState.locale
                        ]
                        []
                    ]
                , div [ class "form-group mt-0" ]
                    [ ActionButton.submitWithAttrs appState
                        { label = gettext "Verify" appState.locale
                        , result = model.loggingIn
                        , attrs = [ class "w-100", disabled (Maybe.isNothing (String.toInt model.code)) ]
                        }
                    ]
                ]
            ]
        ]
