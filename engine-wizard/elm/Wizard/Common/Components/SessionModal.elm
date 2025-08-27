module Wizard.Common.Components.SessionModal exposing
    ( expiredModal
    , expiresSoonModal
    )

import Gettext exposing (gettext)
import Html exposing (Html, text)
import String.Format as String
import Wizard.Auth.Msgs
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Data.Session as Session
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing as Routing


expiresSoonModal : AppState -> Html Wizard.Msgs.Msg
expiresSoonModal appState =
    let
        logoutMsg =
            Just (Routing.toUrl appState.route)
                |> Routes.publicLogin
                |> Wizard.Auth.Msgs.LogoutTo
                |> Wizard.Msgs.AuthMsg

        modalContent =
            [ text
                (String.format
                    (gettext "Your session expires in less than %s minutes. Log in again to refresh it." appState.locale)
                    [ String.fromInt Session.expirationWarningMins ]
                )
            ]

        visible =
            AppState.sessionExpiresSoon appState
                && not (AppState.sessionExpired appState)
                && not appState.sessionExpiresSoonModalHidden

        cfg =
            Modal.confirmConfig (gettext "Session Expires Soon" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigAction (gettext "Log in again" appState.locale) logoutMsg
                |> Modal.confirmConfigCancelMsg Wizard.Msgs.HideSessionExpiresSoonModal
                |> Modal.confirmConfigDataCy "session-modal_expires-soon"
    in
    Modal.confirm appState cfg


expiredModal : AppState -> Html Wizard.Msgs.Msg
expiredModal appState =
    let
        logoutMsg =
            Just (Routing.toUrl appState.route)
                |> Routes.publicLogin
                |> Wizard.Auth.Msgs.LogoutTo
                |> Wizard.Msgs.AuthMsg

        modalContent =
            [ text (gettext "Your session has expired. You need to log in again." appState.locale) ]

        cfg =
            Modal.confirmConfig (gettext "Session Expired" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (AppState.sessionExpired appState)
                |> Modal.confirmConfigAction (gettext "Log in again" appState.locale) logoutMsg
                |> Modal.confirmConfigDataCy "session-modal_expired"
    in
    Modal.confirm appState cfg
