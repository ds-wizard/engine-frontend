module Shared.Data.SummaryReport exposing
    ( ChapterReport
    , IndicationReport(..)
    , MetricReport
    , SummaryReport
    , TotalReport
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Shared.Data.SummaryReport.AnsweredIndicationData as AnsweredIndicationData exposing (AnsweredIndicationData)


type alias SummaryReport =
    { totalReport : TotalReport
    , chapterReports : List ChapterReport
    }


type alias TotalReport =
    { metrics : List MetricReport
    , indications : List IndicationReport
    }


type alias ChapterReport =
    { chapterUuid : String
    , metrics : List MetricReport
    , indications : List IndicationReport
    }


type alias MetricReport =
    { metricUuid : String
    , measure : Float
    }


type IndicationReport
    = AnsweredIndication AnsweredIndicationData
    | PhasesAnsweredIndication AnsweredIndicationData


decoder : Decoder SummaryReport
decoder =
    Decode.succeed SummaryReport
        |> required "totalReport" totalReportDecoder
        |> required "chapterReports" (Decode.list chapterReportDecoder)


totalReportDecoder : Decoder TotalReport
totalReportDecoder =
    Decode.succeed TotalReport
        |> required "metrics" (Decode.list metricReportDecoder)
        |> required "indications" (Decode.list indicationReportDecoder)


chapterReportDecoder : Decoder ChapterReport
chapterReportDecoder =
    Decode.succeed ChapterReport
        |> required "chapterUuid" Decode.string
        |> required "metrics" (Decode.list metricReportDecoder)
        |> required "indications" (Decode.list indicationReportDecoder)


metricReportDecoder : Decoder MetricReport
metricReportDecoder =
    Decode.succeed MetricReport
        |> required "metricUuid" Decode.string
        |> required "measure" Decode.float


indicationReportDecoder : Decoder IndicationReport
indicationReportDecoder =
    Decode.oneOf
        [ when indicationType ((==) "AnsweredIndication") answeredIndicationDecoder
        , when indicationType ((==) "PhasesAnsweredIndication") phasesAnsweredIndicationDecoder
        ]


indicationType : Decoder String
indicationType =
    Decode.field "indicationType" Decode.string


answeredIndicationDecoder : Decoder IndicationReport
answeredIndicationDecoder =
    Decode.map AnsweredIndication AnsweredIndicationData.decoder


phasesAnsweredIndicationDecoder : Decoder IndicationReport
phasesAnsweredIndicationDecoder =
    Decode.map PhasesAnsweredIndication AnsweredIndicationData.decoder
