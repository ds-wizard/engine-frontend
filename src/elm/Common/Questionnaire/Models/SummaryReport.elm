module Common.Questionnaire.Models.SummaryReport exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (decode, required)


type alias SummaryReport =
    { chapterReports : List ChapterReport }


type alias ChapterReport =
    { chapterUuid : String
    , metrics : List MetricReport
    , indications : List IndicationReport
    }


type alias MetricReport =
    { uuid : String
    , measure : Float
    }


type IndicationReport
    = AnsweredIndication AnsweredIndicationData


type alias AnsweredIndicationData =
    { answered : Int
    , unanswered : Int
    }


summaryReportDecoder : Decoder SummaryReport
summaryReportDecoder =
    decode SummaryReport
        |> required "chapterReports" (Decode.list chapterReportDecoder)


chapterReportDecoder : Decoder ChapterReport
chapterReportDecoder =
    decode ChapterReport
        |> required "chapterUuid" Decode.string
        |> required "metrics" (Decode.list metricReportDecoder)
        |> required "indications" (Decode.list indicationReportDecoder)


metricReportDecoder : Decoder MetricReport
metricReportDecoder =
    decode MetricReport
        |> required "uuid" Decode.string
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
    decode AnsweredIndicationData
        |> required "answered" Decode.int
        |> required "unanswered" Decode.int
        |> Decode.map AnsweredIndication
