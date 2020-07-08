module Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing
    ( DashboardWidget(..)
    , decoder
    , dictDecoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type DashboardWidget
    = DMPWorkflowDashboardWidget
    | LevelsQuestionnaireDashboardWidget
    | WelcomeDashboardWidget


dictDecoder : Decoder (Dict String (List DashboardWidget))
dictDecoder =
    D.dict (D.list decoder)


decoder : Decoder DashboardWidget
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "DMPWorkflow" ->
                        D.succeed DMPWorkflowDashboardWidget

                    "LevelsQuestionnaire" ->
                        D.succeed LevelsQuestionnaireDashboardWidget

                    "Welcome" ->
                        D.succeed WelcomeDashboardWidget

                    widgetType ->
                        D.fail <| "Unknown widget: " ++ widgetType
            )


encode : DashboardWidget -> E.Value
encode widget =
    case widget of
        DMPWorkflowDashboardWidget ->
            E.string "DMPWorkflow"

        LevelsQuestionnaireDashboardWidget ->
            E.string "LevelsQuestionnaire"

        WelcomeDashboardWidget ->
            E.string "Welcome"
