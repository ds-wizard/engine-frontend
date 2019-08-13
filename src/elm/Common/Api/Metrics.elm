module Common.Api.Metrics exposing (getMetrics)

import Common.Api exposing (ToMsg, jwtGet)
import Common.AppState exposing (AppState)
import Json.Decode as D
import KMEditor.Common.KnowledgeModel.Metric as Metric exposing (Metric)


getMetrics : AppState -> ToMsg (List Metric) msg -> Cmd msg
getMetrics =
    jwtGet "/metrics" (D.list Metric.decoder)
