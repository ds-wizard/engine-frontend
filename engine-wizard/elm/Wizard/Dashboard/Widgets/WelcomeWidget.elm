module Wizard.Dashboard.Widgets.WelcomeWidget exposing (view)

import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Shared.Auth.Role as Role
import Shared.Auth.Session as Session
import Shared.Locale exposing (l, lf)
import Shared.Undraw as Undraw
import Wizard.Common.AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Dashboard.Widgets.WelcomeWidget"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Dashboard.Widgets.WelcomeWidget"


view : AppState -> Html msg
view appState =
    let
        welcomeText =
            case appState.session.user of
                Just user ->
                    lf_ "welcomeName" [ user.firstName ] appState

                Nothing ->
                    l_ "welcomeDefault" appState

        roleText =
            Role.switch (Session.getUserRole appState.session)
                (l_ "roleAdmin" appState)
                (l_ "roleDataSteward" appState)
                (l_ "roleResearcher" appState)
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
