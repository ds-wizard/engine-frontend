module Wizard.Common.Questionnaire.Views.SummaryReport exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Round
import Shared.Locale exposing (l, lf, lgx, lx)
import String exposing (fromFloat, fromInt)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model, reportCanvasId, totalReportId)
import Wizard.Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport)
import Wizard.Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModels
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Questionnaire.Views.SummaryReport"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.Questionnaire.Views.SummaryReport"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Questionnaire.Views.SummaryReport"


view : AppState -> Model -> SummaryReport -> Html Msg
view appState model summaryReport =
    let
        title =
            [ h2 [] [ lgx "questionnaire.summaryReport" appState ] ]

        totalReport =
            [ viewIndications appState summaryReport.totalReport.indications
            , viewMetrics appState model.metrics summaryReport.totalReport.metrics totalReportId
            , hr [] []
            ]

        chapters =
            viewChapters appState model model.metrics summaryReport

        metricDescriptions =
            [ viewMetricsDescriptions appState model.metrics ]
    in
    div [ class "summary-report" ]
        (List.concat [ title, totalReport, chapters, metricDescriptions ])


viewChapters : AppState -> Model -> List Metric -> SummaryReport -> List (Html Msg)
viewChapters appState model metrics summaryReport =
    List.map (viewChapterReport appState model metrics) summaryReport.chapterReports


viewChapterReport : AppState -> Model -> List Metric -> ChapterReport -> Html Msg
viewChapterReport appState model metrics chapterReport =
    let
        chapterTitle =
            model.questionnaire.knowledgeModel
                |> KnowledgeModels.getChapter chapterReport.chapterUuid
                |> Maybe.map .title
                |> Maybe.withDefault ""
    in
    div []
        [ h3 [] [ text chapterTitle ]
        , viewIndications appState chapterReport.indications
        , viewMetrics appState metrics chapterReport.metrics chapterReport.chapterUuid
        ]


viewMetrics : AppState -> List Metric -> List MetricReport -> String -> Html Msg
viewMetrics appState metrics metricReports canvasId =
    let
        content =
            if List.length metricReports == 0 then
                []

            else if List.length metricReports > 2 then
                [ div [ class "col-xs-12 col-xl-6" ] [ viewMetricsTable appState metrics metricReports ]
                , div [ class "col-xs-12 col-xl-6" ] [ viewMetricsChart metrics canvasId ]
                ]

            else
                [ div [ class "col-12" ] [ viewMetricsTable appState metrics metricReports ] ]
    in
    div [ class "row" ] content


viewIndications : AppState -> List IndicationReport -> Html Msg
viewIndications appState indications =
    table [ class "indication-table" ] (List.map (viewIndication appState) indications)


viewIndication : AppState -> IndicationReport -> Html Msg
viewIndication appState indicationReport =
    case indicationReport of
        AnsweredIndication data ->
            viewAnsweredIndication appState (lf_ "answeredIndication.label") data

        LevelsAnsweredIndication data ->
            viewAnsweredIndication appState (lf_ "levelsAnsweredIndication.label") data


viewAnsweredIndication : AppState -> (List String -> AppState -> String) -> AnsweredIndicationData -> Html Msg
viewAnsweredIndication appState title data =
    let
        progress =
            toFloat data.answeredQuestions / (toFloat <| data.answeredQuestions + data.unansweredQuestions)

        answered =
            fromInt data.answeredQuestions

        all =
            fromInt <| data.answeredQuestions + data.unansweredQuestions
    in
    tr [ class "indication" ]
        [ td [] [ text <| title [ answered, all ] appState ]
        , td [] [ viewProgressBar "bg-info" progress ]
        ]


viewMetricsTable : AppState -> List Metric -> List MetricReport -> Html Msg
viewMetricsTable appState metrics metricReports =
    table [ class "table table-metrics-report" ]
        [ thead []
            [ tr []
                [ th [] [ lgx "metric" appState ]
                , th [ colspan 2 ] [ lgx "metric.measure" appState ]
                ]
            ]
        , tbody []
            (List.map (viewMetricReportRow metrics) metricReports)
        ]


viewMetricReportRow : List Metric -> MetricReport -> Html Msg
viewMetricReportRow metrics metricReport =
    tr []
        [ td [] [ text <| getTitleByUuid metrics metricReport.metricUuid ]
        , td [] [ text <| Round.round 2 metricReport.measure ]
        , td [] [ viewProgressBarWithColors metricReport.measure ]
        ]


viewProgressBarWithColors : Float -> Html msg
viewProgressBarWithColors value =
    let
        colorClass =
            (++) "bg-value-" <| String.fromInt <| (*) 10 <| round <| value * 10
    in
    viewProgressBar colorClass value


viewProgressBar : String -> Float -> Html msg
viewProgressBar colorClass value =
    let
        width =
            (fromFloat <| value * 100) ++ "%"
    in
    div [ class "progress" ]
        [ div [ class <| "progress-bar " ++ colorClass, style "width" width ] [] ]


viewMetricsChart : List Metric -> String -> Html Msg
viewMetricsChart _ canvasId =
    div [ class "metrics-chart" ]
        [ canvas [ id <| reportCanvasId canvasId ] [] ]


getTitleByUuid : List { a | uuid : String, title : String } -> String -> String
getTitleByUuid items uuid =
    List.find (.uuid >> (==) uuid) items
        |> Maybe.map .title
        |> Maybe.withDefault "Unknown"


viewMetricsDescriptions : AppState -> List Metric -> Html msg
viewMetricsDescriptions appState metrics =
    div []
        ([ h3 [] [ lx_ "metricsDescriptions.metricsExplanation" appState ] ]
            ++ List.map viewMetricDescription metrics
        )


viewMetricDescription : Metric -> Html msg
viewMetricDescription metric =
    div []
        [ h4 [] [ text <| metric.abbreviation ++ " - " ++ metric.title ]
        , p [ class "text-justify" ] [ text metric.description ]
        ]
