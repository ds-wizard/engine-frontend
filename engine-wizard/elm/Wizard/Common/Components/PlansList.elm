module Wizard.Common.Components.PlansList exposing (view)

import Html exposing (Html, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.Plan exposing (Plan)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lgx, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.PlansList"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.PlansList"


type alias ViewConfig msg =
    { actions : Maybe (Plan -> List (Html msg)) }


view : AppState -> ViewConfig msg -> List Plan -> Html msg
view appState cfg plans =
    let
        viewTrialBadge plan =
            if plan.test then
                span [ class "badge badge-secondary ml-2" ] [ lx_ "badge.trial" appState ]

            else
                emptyNode

        viewActiveBadge plan =
            let
                active =
                    case ( plan.since, plan.until ) of
                        ( Just since, Just until ) ->
                            TimeUtils.isBetween since until appState.currentTime

                        ( Just since, Nothing ) ->
                            TimeUtils.isAfter since appState.currentTime

                        ( Nothing, Just until ) ->
                            TimeUtils.isBefore until appState.currentTime

                        ( Nothing, Nothing ) ->
                            True
            in
            if active then
                span [ class "badge badge-success ml-2" ] [ lx_ "badge.active" appState ]

            else
                emptyNode

        viewPlanTime mbTime =
            case mbTime of
                Just time ->
                    TimeUtils.toReadableDate appState.timeZone time

                Nothing ->
                    "-"

        viewPlan plan =
            let
                planActions =
                    case cfg.actions of
                        Just actions ->
                            td [ class "text-center" ] (actions plan)

                        Nothing ->
                            emptyNode
            in
            tr []
                [ td [ dataCy "plans-list_name" ] [ text plan.name, viewTrialBadge plan, viewActiveBadge plan ]
                , td [ dataCy "plans-list_users" ] [ text (Maybe.unwrap "-" String.fromInt plan.users) ]
                , td [ dataCy "plans-list_from" ] [ text (viewPlanTime plan.since) ]
                , td [ dataCy "plans-list_to" ] [ text (viewPlanTime plan.until) ]
                , planActions
                ]

        headerActions =
            if Maybe.isJust cfg.actions then
                th [] []

            else
                emptyNode

        plansTable =
            table [ class "table table-striped table-hover" ]
                [ thead []
                    [ tr []
                        [ th [] [ lx_ "table.plan" appState ]
                        , th [] [ lgx "appPlan.users" appState ]
                        , th [] [ lgx "appPlan.from" appState ]
                        , th [] [ lgx "appPlan.to" appState ]
                        , headerActions
                        ]
                    ]
                , tbody [] (List.map viewPlan plans)
                ]

        noPlans =
            Flash.info appState (l_ "noPlans" appState)
    in
    if List.isEmpty plans then
        noPlans

    else
        plansTable
