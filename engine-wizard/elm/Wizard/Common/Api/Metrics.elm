module Wizard.Common.Api.Metrics exposing (getMetrics)

import Json.Decode as D
import Wizard.Common.Api exposing (ToMsg, jwtGet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Common.KnowledgeModel.Metric as Metric exposing (Metric)


getMetrics : AppState -> ToMsg (List Metric) msg -> Cmd msg
getMetrics =
    jwtGet "/metrics" (D.list Metric.decoder)
