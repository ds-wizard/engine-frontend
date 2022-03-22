module Wizard.Settings.Plans.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Plan exposing (Plan)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (wideDetailClass)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Page as Page
import Wizard.Settings.Plans.Models exposing (Model)
import Wizard.Settings.Plans.Msgs exposing (Msg)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Plans.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Plans.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewContent appState) model.plans


viewContent : AppState -> List Plan -> Html Msg
viewContent appState plans =
    let
        viewTrialBadge plan =
            if plan.test then
                span [ class "badge badge-secondary ml-2" ] [ lx_ "badge.trial" appState ]

            else
                emptyNode

        viewActiveBadge plan =
            if TimeUtils.isBetween plan.since plan.until appState.currentTime then
                span [ class "badge badge-success ml-2" ] [ lx_ "badge.active" appState ]

            else
                emptyNode

        viewPlanTime time =
            TimeUtils.toReadableDate appState.timeZone time

        viewPlan plan =
            tr []
                [ td [] [ text plan.name, viewTrialBadge plan, viewActiveBadge plan ]
                , td [] [ text (Maybe.unwrap "-" String.fromInt plan.users) ]
                , td [] [ text (viewPlanTime plan.since) ]
                , td [] [ text (viewPlanTime plan.until) ]
                ]

        plansTable =
            table [ class "table table-striped" ]
                [ thead []
                    [ tr []
                        [ th [] [ lx_ "table.plan" appState ]
                        , th [] [ lx_ "table.users" appState ]
                        , th [] [ lx_ "table.from" appState ]
                        , th [] [ lx_ "table.to" appState ]
                        ]
                    ]
                , tbody [] (List.map viewPlan plans)
                ]

        noPlans =
            Flash.info appState (l_ "noPlans" appState)

        content =
            if List.isEmpty plans then
                noPlans

            else
                plansTable
    in
    div [ wideDetailClass "" ]
        [ Page.header (l_ "title" appState) []
        , content
        ]
