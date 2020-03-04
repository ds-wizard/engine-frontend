module Wizard.Dashboard.Update exposing (fetchData, update)

import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.Api.Levels as LevelsApi
import Wizard.Common.Api.Questionnaires as QuestionnairesApi
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Config exposing (Widget(..))
import Wizard.Common.Setters exposing (setLevels, setQuestionnaires)
import Wizard.Dashboard.Models exposing (Model)
import Wizard.Dashboard.Msgs exposing (Msg(..))
import Wizard.Msgs


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


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        GetLevelsCompleted result ->
            applyResult
                { setResult = setLevels
                , defaultError = lg "apiError.levels.getListError" appState
                , model = model
                , result = result
                }

        GetQuestionnairesCompleted result ->
            applyResult
                { setResult = setQuestionnaires
                , defaultError = lg "apiError.questionnaires.getListError" appState
                , model = model
                , result = result
                }

        _ ->
            ( model, Cmd.none )
