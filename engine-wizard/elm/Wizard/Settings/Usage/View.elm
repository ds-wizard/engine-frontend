module Wizard.Settings.Usage.View exposing (view)

import Html exposing (Html, div)
import Shared.Data.Usage exposing (Usage)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.UsageTable as UsageTable
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.Page as Page
import Wizard.Settings.Usage.Models exposing (Model)
import Wizard.Settings.Usage.Msgs exposing (Msg)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Usage.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState) model.usage


viewContent : AppState -> Usage -> Html Msg
viewContent appState usage =
    div [ wideDetailClass "Usage" ]
        [ Page.header (l_ "title" appState) []
        , UsageTable.view appState usage
        ]
