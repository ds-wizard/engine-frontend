module Wizard.Pages.Dashboard.Update exposing
    ( fetchData
    , update
    )

import Common.Utils.Driver as Driver exposing (TourConfig)
import Gettext exposing (gettext)
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Dashboard.Dashboards.WidgetDashboard as WidgetDashboard
import Wizard.Pages.Dashboard.Models exposing (CurrentDashboard(..), Model)
import Wizard.Pages.Dashboard.Msgs exposing (Msg(..))
import Wizard.Utils.Driver as Driver
import Wizard.Utils.TourId as TourId


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        fetchDashboarData =
            case model.currentDashboard of
                WidgetDashboard ->
                    Cmd.map WidgetDashboardMsg <|
                        WidgetDashboard.fetchData appState

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
        WidgetDashboardMsg widgetDashboardMsg ->
            let
                updateConfig =
                    { locale = appState.locale
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( widgetDashboardModel, cmd ) =
                    WidgetDashboard.update updateConfig widgetDashboardMsg model.widgetDashboardModel
            in
            ( { model | widgetDashboardModel = widgetDashboardModel }
            , cmd
            )
