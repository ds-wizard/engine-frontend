module Wizard.Pages.Dashboard.Update exposing
    ( fetchData
    , update
    )

import Common.Utils.Driver as Driver exposing (TourConfig)
import Gettext exposing (gettext)
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Dashboard.Dashboards.AdminDashboard as AdminDashboard
import Wizard.Pages.Dashboard.Dashboards.DataStewardDashboard as DataStewardDashboard
import Wizard.Pages.Dashboard.Dashboards.ResearcherDashboard as ResearcherDashboard
import Wizard.Pages.Dashboard.Models exposing (CurrentDashboard(..), Model)
import Wizard.Pages.Dashboard.Msgs exposing (Msg(..))
import Wizard.Utils.Driver as Driver
import Wizard.Utils.TourId as TourId


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        fetchDashboarData =
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
    in
    Cmd.batch
        [ fetchDashboarData
        , Driver.init (tour appState)
        ]


tour : AppState -> TourConfig
tour appState =
    let
        firstStep =
            if Admin.isEnabled appState.config.admin then
                { title = gettext "Welcome to the Data Management Planner" appState.locale
                , description = gettext "We'll guide you through creating your data management plan." appState.locale
                }

            else
                { title = gettext "Welcome to Data Stewardship Wizard" appState.locale
                , description = gettext "We'll guide you through creating your data management plan." appState.locale
                }
    in
    Driver.fromAppState TourId.dashboard appState
        |> Driver.addStep
            { element = Nothing
            , popover = firstStep
            }
        |> Driver.addStep
            { element = Just "#menu_projects"
            , popover =
                { title = gettext "Projects" appState.locale
                , description = gettext "Create and manage your data management plans here." appState.locale
                }
            }


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
