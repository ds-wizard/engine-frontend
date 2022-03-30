module Wizard.Settings.Plans.View exposing (view)

import Html exposing (Html, div)
import Shared.Data.Plan exposing (Plan)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.PlansList as PlansList
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.Page as Page
import Wizard.Settings.Plans.Models exposing (Model)
import Wizard.Settings.Plans.Msgs exposing (Msg)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Plans.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState) model.plans


viewContent : AppState -> List Plan -> Html Msg
viewContent appState plans =
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , PlansList.view appState { actions = Nothing } plans
        ]
