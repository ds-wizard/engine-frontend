module Shared.Api.Metrics exposing (getMetrics)

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtOrHttpGet)
import Shared.Data.KnowledgeModel.Metric as Metric exposing (Metric)


getMetrics : AbstractAppState a -> ToMsg (List Metric) msg -> Cmd msg
getMetrics =
    jwtOrHttpGet "/metrics" (D.list Metric.decoder)
