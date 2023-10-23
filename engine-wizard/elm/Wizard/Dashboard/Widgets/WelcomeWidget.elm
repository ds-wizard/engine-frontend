module Wizard.Dashboard.Widgets.WelcomeWidget exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Shared.Auth.Role as Role
import Shared.Undraw as Undraw
import String.Format as String
import Wizard.Common.AppState as AppState exposing (AppState)


view : AppState -> Html msg
view appState =
    let
        welcomeText =
            case appState.config.user of
                Just user ->
                    String.format (gettext "Welcome, %s!" appState.locale) [ user.firstName ]

                Nothing ->
                    gettext "Welcome!" appState.locale

        roleText =
            Role.switch (AppState.getUserRole appState)
                (gettext "As an admin, you configure the instance and manage user accounts." appState.locale)
                (gettext "As a data steward, you prepare knowledge models, document templates, and project templates for researchers." appState.locale)
                (gettext "As a researcher, you create and collaborate on data management plans." appState.locale)
                ""
    in
    div [ class "col-12" ]
        [ div [ class "WelcomeRoleWidget px-4 py-5 bg-light rounded-3 position-relative overflow-hidden mb-3" ]
            [ div [ class "container-fluid py-3 position-relative" ]
                [ h1 [ class "fs-3 fw-bold" ] [ text welcomeText ]
                , p [ class "col-sm-12 col-lg-6 col-sm-12 fs-5 m-0" ] [ text roleText ]
                ]
            , Undraw.explore
            ]
        ]
