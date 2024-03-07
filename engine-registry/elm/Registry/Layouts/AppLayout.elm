module Registry.Layouts.AppLayout exposing (AppLayoutConfig, app)

import Browser exposing (Document)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, header, img, li, main_, section, small, text, ul)
import Html.Attributes exposing (class, classList, height, href, src)
import Html.Events exposing (onClick)
import Registry.Components.FontAwesome exposing (fas)
import Registry.Data.AppState exposing (AppState)
import Registry.Routes as Routes


type alias AppLayoutConfig msg =
    { openAboutModalMsg : msg
    , logoutMsg : msg
    , openCloseMenuMsg : Bool -> msg
    , content : Html msg
    , aboutModal : Html msg
    , menuVisible : Bool
    }


app : AppState -> AppLayoutConfig msg -> Document msg
app appState cfg =
    let
        appTitle =
            Maybe.withDefault "DSW Registry" appState.appTitle
    in
    { title = appTitle
    , body =
        [ main_ []
            [ viewHeader appState cfg appTitle
            , section [ class "container pt-lg-5" ]
                [ cfg.content ]
            ]
        , cfg.aboutModal
        ]
    }


viewHeader : AppState -> AppLayoutConfig msg -> String -> Html msg
viewHeader appState cfg appTitle =
    header [ class "shadow-sm navbar-fixed" ]
        [ div [ class "navbar-sticky bg-white" ]
            [ div [ class "navbar navbar-expand-lg navbar-light" ]
                [ div [ class "container" ]
                    [ a
                        [ class "navbar-brand"
                        , href (Routes.toUrl Routes.home)
                        ]
                        [ div [ class "d-flex" ]
                            [ img [ src "/img/logo.svg", height 30, class "me-1" ] []
                            , text appTitle
                            ]
                        ]
                    , div [ class "navbar-toolbar d-flex flex-shrink-0 align-items-center" ]
                        (a [ class "navbar-tool", onClick cfg.openAboutModalMsg ]
                            [ div [ class "navbar-tool-icon-box" ]
                                [ fas "fa-lg fa-info-circle" ]
                            ]
                            :: profileNavigation appState cfg
                            ++ [ a [ class "navbar-tool d-lg-none d-sm-flex", onClick (cfg.openCloseMenuMsg (not cfg.menuVisible)) ]
                                    [ div [ class "navbar-tool-icon-box" ]
                                        [ fas "fa-lg fa-bars" ]
                                    ]
                               ]
                        )
                    ]
                ]
            , div [ class "navbar navbar-expand-lg navbar-light" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-collapse collapse", classList [ ( "show", cfg.menuVisible ) ] ]
                        [ ul [ class "navbar-nav" ]
                            [ li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , classList [ ( "active", appState.route == Routes.Home ) ]
                                    , href (Routes.toUrl Routes.home)
                                    ]
                                    [ fas "fa-home"
                                    , text (gettext "Home" appState.locale)
                                    ]
                                ]
                            , li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , classList [ ( "active", appState.route == Routes.KnowledgeModels ) ]
                                    , href (Routes.toUrl Routes.knowledgeModels)
                                    ]
                                    [ fas "fa-sitemap"
                                    , text (gettext "Knowledge Models" appState.locale)
                                    ]
                                ]
                            , li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , classList [ ( "active", appState.route == Routes.DocumentTemplates ) ]
                                    , href (Routes.toUrl Routes.documentTemplates)
                                    ]
                                    [ fas "fa-file-code"
                                    , text (gettext "Document Templates" appState.locale)
                                    ]
                                ]
                            , li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , classList [ ( "active", appState.route == Routes.Locales ) ]
                                    , href (Routes.toUrl Routes.locales)
                                    ]
                                    [ fas "fa-language"
                                    , text (gettext "Locales" appState.locale)
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


profileNavigation : AppState -> AppLayoutConfig msg -> List (Html msg)
profileNavigation appState cfg =
    if appState.config.authentication.publicRegistrationEnabled then
        case appState.session of
            Just session ->
                [ a
                    [ class "navbar-tool d-sm-flex d-lg-none"
                    , href (Routes.toUrl Routes.organizationDetail)
                    ]
                    [ div [ class "navbar-tool-icon-box" ]
                        [ fas "fa-lg fa-user" ]
                    ]
                , a
                    [ class "navbar-tool d-sm-flex d-lg-none"
                    , onClick cfg.logoutMsg
                    ]
                    [ div [ class "navbar-tool-icon-box" ]
                        [ fas "fa-lg fa-sign-out-alt" ]
                    ]
                , div [ class "navbar-profile d-lg-flex d-none" ]
                    [ div [ class "navbar-tool-icon-box" ]
                        [ fas "fa-lg fa-user" ]
                    , div [ class "d-flex flex-column justify-content-center" ]
                        [ small [ class "organization-name" ] [ text session.organizationName ]
                        , div [ class "text-muted" ]
                            [ a [ href (Routes.toUrl Routes.organizationDetail) ] [ text (gettext "Edit" appState.locale) ]
                            , text " • "
                            , a [ onClick cfg.logoutMsg ] [ text (gettext "Logout" appState.locale) ]
                            ]
                        ]
                    ]
                ]

            Nothing ->
                [ a
                    [ class "navbar-tool"
                    , href (Routes.toUrl Routes.login)
                    ]
                    [ div [ class "navbar-tool-icon-box" ]
                        [ fas "fa-lg fa-user" ]
                    ]
                ]

    else
        []
