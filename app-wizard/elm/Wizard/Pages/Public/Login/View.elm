module Wizard.Pages.Public.Login.View exposing (view)

import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (fa, fas)
import Common.Components.FormResult as FormResult
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Utils.MarkdownOrHtml as MarkdownOrHtml
import Gettext exposing (gettext)
import Html exposing (Html, a, div, form, input, p, span, text)
import Html.Attributes exposing (attribute, class, disabled, id, pattern, placeholder, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Extra as Html
import Html.Keyed
import Maybe.Extra as Maybe
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Components.Announcements as Announcements
import Wizard.Components.ExternalLoginButton as ExternalLoginButton
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Login.Models exposing (Model)
import Wizard.Pages.Public.Login.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    if Admin.isEnabled appState.config.admin then
        Html.nothing

    else
        let
            form =
                if model.codeRequired then
                    ( "code", codeFormView appState model )

                else
                    ( "login", loginFormView appState model )

            loginInfoSidebar =
                ( "login-info-sidebar"
                , Maybe.unwrap Html.nothing (MarkdownOrHtml.toHtml [ class "mt-4", dataCy "login_info-sidebar" ]) appState.config.dashboardAndLoginScreen.loginInfoSidebar
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
                                , dataCy "login_info"
                                ]
                                [ MarkdownOrHtml.toHtml [ class "flex-grow-1" ] loginInfo ]
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
        viewExternalLoginButton service =
            ExternalLoginButton.view
                { onClick = ExternalLoginOpenId service
                , service = service
                }

        externalLoginButtons =
            List.map viewExternalLoginButton appState.config.authentication.external.services
    in
    if appState.config.authentication.internal.nonAdminLoginEnabled || model.adminLoginVisible then
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
                    [ linkTo Routes.publicForgottenPassword [] [ text (gettext "Forgot your password?" appState.locale) ]
                    , ActionButton.submit <| ActionButton.SubmitConfig (gettext "Log In" appState.locale) model.loggingIn
                    ]
                ]

            externalLogin =
                if List.isEmpty externalLoginButtons then
                    []

                else
                    div [ class "external-login-separator", dataCy "login_external_separator" ]
                        [ text (gettext "Or connect with" appState.locale) ]
                        :: externalLoginButtons
        in
        div []
            [ form [ onSubmit DoLogin, class "card bg-light" ]
                [ div [ class "card-header" ] [ text (gettext "Log In" appState.locale) ]
                , div [ class "card-body" ]
                    (FormResult.view model.loggingIn
                        :: loginForm
                        ++ externalLogin
                    )
                ]
            ]

    else
        div []
            [ div [ class "class card bg-light" ]
                [ div [ class "card-header fw-bold d-flex justify-content-between" ]
                    [ text (gettext "Log In" appState.locale)
                    , a (onClick ShowAdminLogin :: class "text-link" :: tooltipLeft (gettext "Login as admin" appState.locale))
                        [ fas "fa-user-shield" ]
                    ]
                , div
                    [ class "card-body" ]
                    externalLoginButtons
                ]
            ]


codeFormView : AppState -> Model -> Html Msg
codeFormView appState model =
    div []
        [ form [ onSubmit DoLogin, class "card bg-light" ]
            [ div [ class "card-header" ] [ text (gettext "Log In" appState.locale) ]
            , div [ class "card-body" ]
                [ FormResult.view model.loggingIn
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
                    [ ActionButton.submitWithAttrs
                        { label = gettext "Verify" appState.locale
                        , result = model.loggingIn
                        , attrs = [ class "w-100", disabled (Maybe.isNothing (String.toInt model.code)) ]
                        }
                    ]
                ]
            ]
        ]
