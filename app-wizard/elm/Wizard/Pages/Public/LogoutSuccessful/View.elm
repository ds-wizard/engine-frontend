module Wizard.Pages.Public.LogoutSuccessful.View exposing (view)

import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)


view : AppState -> Html msg
view appState =
    Page.illustratedMessage
        { illustration = Undraw.windyDay
        , heading = gettext "Logged out" appState.locale
        , lines = [ gettext "You have been successfully logged out." appState.locale ]
        , cy = "logout-successful"
        }
