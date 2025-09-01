module Wizard.Pages.Public.LogoutSuccessful.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Components.Page as Page
import Shared.Components.Undraw as Undraw
import Wizard.Data.AppState exposing (AppState)


view : AppState -> Html msg
view appState =
    Page.illustratedMessage
        { image = Undraw.windyDay
        , heading = gettext "Logged out" appState.locale
        , lines = [ gettext "You have been successfully logged out." appState.locale ]
        , cy = "logout-successful"
        }
