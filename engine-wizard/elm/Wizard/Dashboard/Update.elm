module Wizard.Dashboard.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
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

        DataStewardDashboard ->
            Cmd.map DataStewardDashboardMsg <|
                DataStewardDashboard.fetchData appState

        AdminDashboard ->
            Cmd.map AdminDashboardMsg <|
                AdminDashboard.fetchData appState

        _ ->
            Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        ResearcherDashboardMsg researcherDashboardMsg ->
            let
                ( researcherDashboardModel, cmd ) =
                    ResearcherDashboard.update Wizard.Msgs.logoutMsg researcherDashboardMsg appState model.researcherDashboardModel
            in
            ( { model | researcherDashboardModel = researcherDashboardModel }
            , cmd
            )

        DataStewardDashboardMsg dataStewardDashboardMsg ->
            let
                ( dataStewardDashboardModel, cmd ) =
                    DataStewardDashboard.update Wizard.Msgs.logoutMsg dataStewardDashboardMsg appState model.dataStewardDashboardModel
            in
            ( { model | dataStewardDashboardModel = dataStewardDashboardModel }
            , cmd
            )

        AdminDashboardMsg adminDashboardMsg ->
            let
                ( adminDashboardModel, cmd ) =
                    AdminDashboard.update Wizard.Msgs.logoutMsg adminDashboardMsg appState model.adminDashboardModel
            in
            ( { model | adminDashboardModel = adminDashboardModel }
            , cmd
            )
