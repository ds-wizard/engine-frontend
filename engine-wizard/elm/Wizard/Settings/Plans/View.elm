module Wizard.Settings.Plans.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div)
import Shared.Data.Plan exposing (Plan)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.PlansList as PlansList
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.Page as Page
import Wizard.Settings.Plans.Models exposing (Model)
import Wizard.Settings.Plans.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState) model.plans


viewContent : AppState -> List Plan -> Html Msg
viewContent appState plans =
    div [ wideDetailClass "" ]
        [ Page.header (gettext "Plans" appState.locale) []
        , PlansList.view appState { actions = Nothing } plans
        ]
