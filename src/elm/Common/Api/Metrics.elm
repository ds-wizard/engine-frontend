module Common.Api.Metrics exposing (getMetrics)

import Common.Api exposing (ToMsg, jwtGet)
import Common.AppState exposing (AppState)
import KMEditor.Common.Models.Entities exposing (Metric, metricListDecoder)


getMetrics : AppState -> ToMsg (List Metric) msg -> Cmd msg
getMetrics =
    jwtGet "/metrics" metricListDecoder
