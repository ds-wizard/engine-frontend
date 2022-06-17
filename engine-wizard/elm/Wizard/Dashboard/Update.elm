module Wizard.Dashboard.Update exposing
    ( fetchData
    , update
    )

import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing (DashboardWidget(..))
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setQuestionnaires)
import Wizard.Common.Api exposing (applyResultTransform)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Dashboard.Models exposing (Model)
import Wizard.Dashboard.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        widgets =
            AppState.getDashboardWidgets appState
    in
    if List.any (\w -> w == DMPWorkflowDashboardWidget || w == LevelsQuestionnaireDashboardWidget) widgets then
        let
            pagination =
                PaginationQueryString.withSort (Just "updatedAt") PaginationQueryString.SortDESC PaginationQueryString.empty

            mbUserUuid =
                Session.getUserUuid appState.session
        in
        QuestionnairesApi.getQuestionnaires { isTemplate = Just False, userUuids = mbUserUuid, userUuidsOp = Nothing, projectTags = Nothing, projectTagsOp = Nothing } pagination appState GetQuestionnairesCompleted

    else
        Cmd.none


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update (GetQuestionnairesCompleted result) appState model =
    applyResultTransform appState
        { setResult = setQuestionnaires
        , defaultError = lg "apiError.questionnaires.getListError" appState
        , model = model
        , result = result
        , transform = .items
        }
