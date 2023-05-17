module Wizard.Common.Components.SessionModal exposing
    ( expiredModal
    , expiresSoonModal
    )

import ActionResult
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Shared.Auth.Session as Session
import String.Format as String
import Wizard.Auth.Msgs
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing as Routing


expiresSoonModal : AppState -> Html Wizard.Msgs.Msg
expiresSoonModal appState =
    let
        logoutMsg =
            Just (Routing.toUrl appState appState.route)
                |> Routes.publicLogin
                |> Wizard.Auth.Msgs.LogoutTo
                |> Wizard.Msgs.AuthMsg
    in
    Modal.confirm appState
        { modalTitle = gettext "Session Expires Soon" appState.locale
        , modalContent =
            [ text
                (String.format
                    (gettext "Your session expires in less than %s minutes. Log in again to refresh it." appState.locale)
                    [ String.fromInt Session.expirationWarningMins ]
                )
            ]
        , visible = AppState.sessionExpiresSoon appState && not (AppState.sessionExpired appState) && not appState.sessionExpiresSoonModalHidden
        , actionResult = ActionResult.Unset
        , actionName = gettext "Log in again" appState.locale
        , actionMsg = logoutMsg
        , cancelMsg = Just Wizard.Msgs.HideSessionExpiresSoonModal
        , dangerous = False
        , dataCy = "session-modal_expires-soon"
        }


expiredModal : AppState -> Html Wizard.Msgs.Msg
expiredModal appState =
    let
        logoutMsg =
            Just (Routing.toUrl appState appState.route)
                |> Routes.publicLogin
                |> Wizard.Auth.Msgs.LogoutTo
                |> Wizard.Msgs.AuthMsg
    in
    Modal.confirm appState
        { modalTitle = gettext "Session Expired" appState.locale
        , modalContent =
            [ text (gettext "Your session has expired. You need to log in again." appState.locale)
            ]
        , visible = AppState.sessionExpired appState
        , actionResult = ActionResult.Unset
        , actionName = gettext "Log in again" appState.locale
        , actionMsg = logoutMsg
        , cancelMsg = Nothing
        , dangerous = False
        , dataCy = "session-modal_expired"
        }
