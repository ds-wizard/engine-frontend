module Common.Components.SessionModal exposing
    ( expired
    , expiresSoon
    , expiryNotice
    )

{-| The session is running out, or has.

Three pieces, because the warning has to be seen at two volumes. `expiresSoon`
interrupts once, a few minutes before the end, and can be put away.
`expiryNotice` is the quiet version that stays in the navigation's footer with
the time left, so someone who dismissed the dialog can still see how long they
have and log in again from there. `expired` interrupts and cannot be put away,
because nothing the page does works any more.

The notice is deliberately not a live region. It counts down every second, and
a region that announces a new number every second is a screen reader that
cannot say anything else; the dialog is the announcement.

-}

import Common.Data.Context exposing (WithLocale)
import Common.Data.Session as Session
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import String.Format as String
import Ui.Button as Button
import Ui.Modal as Modal
import Ui.Text as Text


{-| A confirmation: log in again, or carry on until it runs out. Closing it —
Cancel, Escape, the backdrop — is `onClose`, which should hide it for the rest
of this session so it does not come straight back.
-}
expiresSoon : WithLocale a -> { open : Bool, onLogin : msg, onClose : msg } -> Html msg
expiresSoon appState config =
    Modal.confirm
        { title = gettext "Session Expires Soon" appState.locale
        , open = config.open
        , onClose = config.onClose
        , body =
            [ Text.body
                (String.format
                    (gettext "Your session expires in less than %s minutes. Log in again to refresh it." appState.locale)
                    [ String.fromInt Session.expirationWarningMins ]
                )
            ]
        , confirmLabel = gettext "Log in again" appState.locale
        , onConfirm = config.onLogin
        }
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "session-modal_expires-soon"
        |> Modal.toHtml


{-| The session is gone. There is one button, and every way of closing the
dialog does what it does: a dismissed dialog would leave a page whose every
request fails, and the browser closes a dialog on Escape whether or not the
application wants it to, so the close has to lead somewhere that works.
-}
expired : WithLocale a -> { open : Bool, onLogin : msg } -> Html msg
expired appState config =
    Modal.plain
        { title = gettext "Session Expired" appState.locale
        , open = config.open
        , onClose = config.onLogin
        , body = [ Text.body (gettext "Your session has expired. You need to log in again." appState.locale) ]
        }
        |> Modal.withActions [ Button.primary (gettext "Log in again" appState.locale) config.onLogin ]
        |> Modal.withTestId "session-modal_expired"
        |> Modal.toHtml


{-| The time left and a way to log in again, for the application frame's
footer. `remaining` is already formatted ("4:59").
-}
expiryNotice : WithLocale a -> { remaining : String, onLogin : msg } -> Html msg
expiryNotice appState config =
    div [ class "c-session-notice" ]
        [ Text.bodySmall (String.format (gettext "Your session expires in %s" appState.locale) [ config.remaining ])
        , Button.toHtml (Button.small (Button.secondary (gettext "Log in again" appState.locale) config.onLogin))
        ]
