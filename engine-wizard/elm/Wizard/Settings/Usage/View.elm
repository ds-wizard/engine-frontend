module Wizard.Settings.Usage.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Data.Usage exposing (Usage)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.UsageTable as UsageTable
import Wizard.Common.View.Page as Page
import Wizard.Settings.Usage.Models exposing (Model)
import Wizard.Settings.Usage.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState) model.usage


viewContent : AppState -> Usage -> Html Msg
viewContent appState usage =
    div [ class "Usage" ]
        [ Page.header (gettext "Usage" appState.locale) []
        , UsageTable.view appState False usage
        ]
