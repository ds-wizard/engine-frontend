module Wizard.Common.View.Layout exposing
    ( app
    , misconfigured
    , mixedApp
    , public
    )

import Browser exposing (Document)
import Gettext exposing (gettext)
import Html exposing (Html, div, img, li, nav, text, ul)
import Html.Attributes exposing (class, classList, src)
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Html exposing (emptyNode)
import Shared.Undraw as Undraw
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.AIAssistant as AIAssistant
import Wizard.Common.Components.CookieConsent as CookieConsent
import Wizard.Common.Components.SessionModal as SessionModal
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.Menu.View as Menu exposing (viewAboutModal, viewLanguagesModal, viewReportIssueModal)
import Wizard.Common.View.Page as Page
import Wizard.Models exposing (Model, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


misconfigured : AppState -> Document Msg
misconfigured appState =
    let
        html =
            Page.illustratedMessage
                { image = Undraw.bugFixing
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
                [ publicHeader False model
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
                [ publicHeader True model
                , div [ class "container-fluid d-flex justify-content-center" ] [ content ]
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html, CookieConsent.view model.appState ]
    }


publicHeader : Bool -> Model -> Html Msg
publicHeader fluidFull model =
    let
        links =
            if userLoggedIn model then
                [ li [ class "nav-item" ]
                    [ linkTo model.appState
                        Routes.appHome
                        [ class "nav-link", dataCy "public_nav_go-to-app" ]
                        [ text (gettext "Go to App" model.appState.locale) ]
                    ]
                ]

            else
                let
                    signUpLink =
                        if model.appState.config.authentication.internal.registration.enabled && not (Admin.isEnabled model.appState.config.admin) then
                            li [ class "nav-item" ]
                                [ linkTo model.appState
                                    Routes.publicSignup
                                    [ class "nav-link", dataCy "public_nav_sign-up" ]
                                    [ text (gettext "Sign Up" model.appState.locale) ]
                                ]

                        else
                            emptyNode
                in
                [ li [ class "nav-item" ]
                    [ linkTo model.appState
                        (Routes.publicLogin (Just (Routing.toUrl model.appState model.appState.route)))
                        [ class "nav-link", dataCy "public_nav_login" ]
                        [ text (gettext "Log In" model.appState.locale) ]
                    ]
                , signUpLink
                ]
    in
    nav [ class "navbar navbar-expand-sm fixed-top px-3 top-navigation" ]
        [ div [ class "container-fluid", classList [ ( "container-max", not fluidFull ) ] ]
            [ div [ class "navbar-header" ]
                [ linkTo model.appState
                    Routes.publicHome
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
        rightPanel =
            if not model.appState.session.rightPanelCollapsed then
                div [ class "right-panel" ]
                    [ div [ class "right-panel-content" ]
                        [ AIAssistant.view
                            { appState = model.appState
                            , wrapMsg = Wizard.Msgs.AIAssistantMsg
                            , closeMsg = Wizard.Msgs.SetRightPanelCollapsed True
                            }
                            model.aiAssistantState
                        ]
                    ]

            else
                emptyNode

        html =
            div
                [ class "app-view"
                , classList
                    [ ( "side-navigation-collapsed", model.appState.session.sidebarCollapsed )
                    , ( "app-fullscreen", AppState.isFullscreen model.appState )
                    , ( "app-right-panel", not model.appState.session.rightPanelCollapsed )
                    ]
                ]
                [ Menu.view model
                , div [ class "page row justify-content-center" ]
                    [ content ]
                , rightPanel
                , viewReportIssueModal model.appState model.menuModel.reportIssueOpen
                , viewAboutModal model.appState model.menuModel.aboutOpen model.menuModel.recentlyCopied model.menuModel.apiBuildInfo
                , viewLanguagesModal model.appState model.menuModel.languagesOpen
                , SessionModal.expiresSoonModal model.appState
                , SessionModal.expiredModal model.appState
                ]
    in
    { title = LookAndFeelConfig.getAppTitle model.appState.config.lookAndFeel
    , body = [ html, CookieConsent.view model.appState ]
    }
