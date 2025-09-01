module Wizard.Pages.Settings.Usage.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Components.Page as Page
import Wizard.Api.Models.Usage exposing (Usage)
import Wizard.Components.UsageTable as UsageTable
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Usage.Models exposing (Model)
import Wizard.Pages.Settings.Usage.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState) model.usage


viewContent : AppState -> Usage -> Html Msg
viewContent appState usage =
    div [ class "Usage" ]
        [ Page.header (gettext "Usage" appState.locale) []
        , UsageTable.view appState False usage
        ]
