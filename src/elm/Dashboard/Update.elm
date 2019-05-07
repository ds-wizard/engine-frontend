module Dashboard.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Levels as LevelsApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (getServerError)
import Common.AppState as AppState exposing (AppState)
import Common.Config exposing (Widget(..))
import Dashboard.Models exposing (Model)
import Dashboard.Msgs exposing (Msg(..))
import Msgs


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        widgets =
            AppState.getDashboardWidgets appState
    in
    if List.any (\w -> w == DMPWorkflow || w == LevelsQuestionnaire) widgets then
        Cmd.batch
            [ LevelsApi.getLevels appState GetLevelsCompleted
            , QuestionnairesApi.getQuestionnaires appState GetQuestionnairesCompleted
            ]

    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        GetLevelsCompleted result ->
            let
                newModel =
                    case result of
                        Ok levels ->
                            { model | levels = Success levels }

                        Err error ->
                            { model | levels = getServerError error "Unable to get levels" }

                cmd =
                    getResultCmd result
            in
            ( newModel, cmd )

        GetQuestionnairesCompleted result ->
            let
                newModel =
                    case result of
                        Ok questionnaires ->
                            { model | questionnaires = Success questionnaires }

                        Err error ->
                            { model | questionnaires = getServerError error "Unable to get levels" }

                cmd =
                    getResultCmd result
            in
            ( newModel, cmd )
