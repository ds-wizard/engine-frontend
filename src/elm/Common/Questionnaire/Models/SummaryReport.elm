module Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport, answeredIndicationDecoder, chapterReportDecoder, indicationReportDecoder, indicationType, metricReportDecoder, summaryReportDecoder)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)


type alias SummaryReport =
    { chapterReports : List ChapterReport }


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


type alias AnsweredIndicationData =
    { answeredQuestions : Int
    , unansweredQuestions : Int
    }


summaryReportDecoder : Decoder SummaryReport
summaryReportDecoder =
    Decode.succeed SummaryReport
        |> required "chapterReports" (Decode.list chapterReportDecoder)


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
        [ when indicationType ((==) "AnsweredIndication") answeredIndicationDecoder ]


indicationType : Decoder String
indicationType =
    Decode.field "indicationType" Decode.string


answeredIndicationDecoder : Decoder IndicationReport
answeredIndicationDecoder =
    Decode.succeed AnsweredIndicationData
        |> required "answeredQuestions" Decode.int
        |> required "unansweredQuestions" Decode.int
        |> Decode.map AnsweredIndication
