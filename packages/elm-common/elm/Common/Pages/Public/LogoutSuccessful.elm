module Common.Pages.Public.LogoutSuccessful exposing
    ( init
    , view
    )

{-| "You have been successfully logged out." Someone who still has a session —
they came back to the page — is sent to the application instead.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.Session as Session
import Common.Pages.Public.Layout as PublicLayout
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Illustration as Illustration
import Ui.Page.Message as Message


init : AppState -> Cmd msg
init appState =
    if Session.exists appState.session then
        Routes.navigate appState Routes.appHome

    else
        Cmd.none


view : AppState -> Html msg
view appState =
    PublicLayout.app appState
        [ Message.message
            { title = gettext "Logged out" appState.locale
            , description = gettext "You have been successfully logged out." appState.locale
            }
            |> Message.withIllustration (Illustration.toHtml Illustration.success)
            |> Message.toHtml
        ]
