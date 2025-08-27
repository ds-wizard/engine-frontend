module Wizard.Public.LogoutSuccessful.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Components.Undraw as Undraw
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page


view : AppState -> Html msg
view appState =
    Page.illustratedMessage
        { image = Undraw.windyDay
        , heading = gettext "Logged out" appState.locale
        , lines = [ gettext "You have been successfully logged out." appState.locale ]
        , cy = "logout-successful"
        }
