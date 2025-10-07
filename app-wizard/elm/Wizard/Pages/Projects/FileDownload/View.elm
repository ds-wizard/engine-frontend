module Wizard.Pages.Projects.FileDownload.View exposing (view)

import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.FileDownload.Models exposing (Model)
import Wizard.Pages.Projects.FileDownload.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (always (viewSuccess appState)) model.urlResponse


viewSuccess : AppState -> Html Msg
viewSuccess appState =
    Page.illustratedMessage
        { illustration = Undraw.exportFiles
        , heading = gettext "Downloading" appState.locale
        , lines = [ gettext "Your download should start shortly." appState.locale ]
        , cy = "download-success"
        }
