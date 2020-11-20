module Wizard.Dashboard.Update exposing (fetchData, update)

import Shared.Api.Levels as LevelsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing (DashboardWidget(..))
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setLevels, setQuestionnaires)
import Wizard.Common.Api exposing (applyResult, applyResultTransform)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Dashboard.Models exposing (Model)
import Wizard.Dashboard.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        widgets =
            AppState.getDashboardWidgets appState

        pagination =
            PaginationQueryString.withSort (Just "updatedAt") PaginationQueryString.SortDESC PaginationQueryString.empty
    in
    if List.any (\w -> w == DMPWorkflowDashboardWidget || w == LevelsQuestionnaireDashboardWidget) widgets then
        Cmd.batch
            [ LevelsApi.getLevels appState GetLevelsCompleted
            , QuestionnairesApi.getQuestionnaires pagination appState GetQuestionnairesCompleted
            ]

    else
        Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetLevelsCompleted result ->
            applyResult appState
                { setResult = setLevels
                , defaultError = lg "apiError.levels.getListError" appState
                , model = model
                , result = result
                }

        GetQuestionnairesCompleted result ->
            applyResultTransform appState
                { setResult = setQuestionnaires
                , defaultError = lg "apiError.questionnaires.getListError" appState
                , model = model
                , result = result
                , transform = .items
                }

        _ ->
            ( model, Cmd.none )
