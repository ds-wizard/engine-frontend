module Wizard.Projects.DocumentDownload.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Components.Undraw as Undraw
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Projects.FileDownload.Models exposing (Model)
import Wizard.Projects.FileDownload.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (always (viewSuccess appState)) model.urlResponse


viewSuccess : AppState -> Html Msg
viewSuccess appState =
    Page.illustratedMessage
        { image = Undraw.exportFiles
        , heading = gettext "Downloading" appState.locale
        , lines = [ gettext "Your download should start shortly." appState.locale ]
        , cy = "download-success"
        }
