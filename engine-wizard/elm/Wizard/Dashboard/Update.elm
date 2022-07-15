module Wizard.Dashboard.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard
import Wizard.Dashboard.Models exposing (CurrentDashboard(..), Model)
import Wizard.Dashboard.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case model.currentDashboard of
        ResearcherDashboard ->
            Cmd.map ResearcherDashboardMsg <|
                ResearcherDashboard.fetchData appState

        AdminDashboard ->
            Cmd.map AdminDashboardMsg <|
                AdminDashboard.fetchData appState

        _ ->
            Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        ResearcherDashboardMsg researcherDashboardMsg ->
            ( { model | researcherDashboardModel = ResearcherDashboard.update researcherDashboardMsg appState model.researcherDashboardModel }
            , Cmd.none
            )

        AdminDashboardMsg adminDashboardMsg ->
            ( { model | adminDashboardModel = AdminDashboard.update adminDashboardMsg appState model.adminDashboardModel }
            , Cmd.none
            )
