module Wizard.Layouts.Layout exposing
    ( app
    , misconfigured
    , mixedApp
    , public
    )

import Browser exposing (Document)
import Common.Components.AIAssistant as AIAssistant
import Common.Components.NewsModal as NewsModal
import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, div, img, li, nav, text, ul)
import Html.Attributes exposing (class, classList, src)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Wizard.Components.CookieConsent as CookieConsent
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.Menu.View as Menu exposing (viewAboutModal, viewReportIssueModal)
import Wizard.Components.SessionModal as SessionModal
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Models exposing (Model, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


misconfigured : AppState -> Document Msg
misconfigured appState =
    let
        html =
            Page.illustratedMessage
                { illustration = Undraw.bugFixing
                , heading = gettext "Configuration Error" appState.locale
                , lines =
                    [ gettext "Application is not configured correctly and cannot run." appState.locale
                    , gettext "Please, contact the administrator." appState.locale
                    ]
                , cy = "misconfigured"
                }
    in
    { title = gettext "Configuration Error" appState.locale
    , body = [ html ]
    }


mixedApp : Model -> Html Msg -> Document Msg
mixedApp model =
    if model.appState.config.user == Nothing then
        publicApp model

    else
        app model


public : Model -> Html Msg -> Document Msg
public model content =
    let
        html =
            div [ class "public" ]
                [ publicHeader model
                , div [ class "container-fluid container-max" ] [ content ]
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html, CookieConsent.view model.appState ]
    }


publicApp : Model -> Html Msg -> Document Msg
publicApp model content =
    let
        html =
            div
                [ class "public public--app"
                , classList [ ( "app-fullscreen", AppState.isFullscreen model.appState ) ]
                ]
                [ publicHeader model
                , div [ class "container-fluid d-flex justify-content-center" ] [ content ]
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html, CookieConsent.view model.appState ]
    }


publicHeader : Model -> Html Msg
publicHeader model =
    let
        links =
            if userLoggedIn model then
                [ li [ class "nav-item" ]
                    [ linkTo Routes.appHome
                        [ class "nav-link", dataCy "public_nav_go-to-app" ]
                        [ text (gettext "Go to App" model.appState.locale) ]
                    ]
                ]

            else
                let
                    signUpLink =
                        if model.appState.config.authentication.internal.registration.enabled && not (Admin.isEnabled model.appState.config.admin) then
                            li [ class "nav-item" ]
                                [ linkTo Routes.publicSignup
                                    [ class "nav-link", dataCy "public_nav_sign-up" ]
                                    [ text (gettext "Sign Up" model.appState.locale) ]
                                ]

                        else
                            Html.nothing
                in
                [ li [ class "nav-item" ]
                    [ linkTo (Routes.publicLogin (Just (Routing.toUrl model.appState.route)))
                        [ class "nav-link", dataCy "public_nav_login" ]
                        [ text (gettext "Log In" model.appState.locale) ]
                    ]
                , signUpLink
                ]
    in
    nav [ class "navbar navbar-expand-lg fixed-top shadow-sm bg-body top-navigation" ]
        [ div
            [ class "container-fluid"
            ]
            [ div [ class "navbar-header" ]
                [ linkTo Routes.publicHome
                    [ class "navbar-brand", dataCy "nav_app-title" ]
                    [ img [ class "logo-img", src (LookAndFeelConfig.getLogoUrl model.appState.config.lookAndFeel) ] []
                    , text <| LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
                    ]
                ]
            , ul [ class "nav navbar-nav ms-auto flex-row" ] links
            ]
        ]


app : Model -> Html Msg -> Document Msg
app model content =
    let
        rightPanelVisible =
            not model.appState.session.rightPanelCollapsed && model.appState.config.features.aiAssistantEnabled

        rightPanel =
            if rightPanelVisible then
                div [ class "right-panel" ]
                    [ div [ class "right-panel-content" ]
                        [ AIAssistant.view
                            { locale = model.appState.locale
                            , wrapMsg = Wizard.Msgs.AIAssistantMsg
                            , closeMsg = Wizard.Msgs.SetRightPanelCollapsed True
                            }
                            model.aiAssistantState
                        ]
                    ]

            else
                Html.nothing

        html =
            div
                [ class "app-view"
                , classList
                    [ ( "side-navigation-collapsed", model.appState.session.sidebarCollapsed )
                    , ( "app-fullscreen", AppState.isFullscreen model.appState )
                    , ( "app-right-panel", rightPanelVisible )
                    ]
                ]
                [ Menu.view model
                , div [ class "page row justify-content-center" ]
                    [ content ]
                , rightPanel
                , viewReportIssueModal model.appState model.menuModel.reportIssueOpen
                , viewAboutModal model.appState model.menuModel.aboutOpen model.menuModel.recentlyCopied model.menuModel.apiBuildInfo
                , SessionModal.expiresSoonModal model.appState
                , SessionModal.expiredModal model.appState
                , Html.map Wizard.Msgs.NewsModalMsg <| NewsModal.view model.appState.locale model.newsModalModel
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html, CookieConsent.view model.appState ]
    }
