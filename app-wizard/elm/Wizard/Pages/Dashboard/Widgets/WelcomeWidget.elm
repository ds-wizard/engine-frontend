module Wizard.Pages.Dashboard.Widgets.WelcomeWidget exposing (view)

import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import String.Format as String
import Wizard.Data.AppState exposing (AppState)


view : AppState -> Html msg
view appState =
    let
        welcomeText =
            case appState.config.user of
                Just user ->
                    String.format (gettext "Welcome, %s!" appState.locale) [ user.firstName ]

                Nothing ->
                    gettext "Welcome!" appState.locale
    in
    div [ class "col-12" ]
        [ div [ class "WelcomeRoleWidget px-4 py-5 bg-light rounded-3 position-relative overflow-hidden mb-3" ]
            [ div [ class "container-fluid py-3 position-relative" ]
                [ h1 [ class "fs-3 fw-bold" ] [ text welcomeText ]
                ]
            , Undraw.explore
            ]
        ]
