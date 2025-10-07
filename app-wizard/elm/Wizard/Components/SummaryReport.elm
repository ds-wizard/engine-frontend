module Wizard.Components.SummaryReport exposing
    ( Msg
    , update
    , view
    , viewIndications
    )

import ChartJS
import Common.Ports.Dom as Dom
import Common.Utils.Markdown as Markdown
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h2, h3, h4, hr, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, id, style)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Round
import String exposing (fromFloat, fromInt)
import String.Format as String
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.SummaryReport exposing (ChapterReport, IndicationReport(..), MetricReport, SummaryReport, TotalReport)
import Wizard.Api.Models.SummaryReport.AnsweredIndicationData exposing (AnsweredIndicationData)
import Wizard.Data.AppState exposing (AppState)


type Msg
    = ScrollToMetric String


update : Msg -> Cmd msg
update msg =
    case msg of
        ScrollToMetric metric ->
            Dom.scrollIntoView ("#" ++ metricId metric)



-- View


view : AppState -> SummaryReport -> Html Msg
view appState summaryReport =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Metrics" ]
        [ viewContent appState summaryReport
        ]


viewContent : AppState -> SummaryReport -> Html Msg
viewContent appState summaryReport =
    let
        title =
            [ h2 [] [ text (gettext "Summary Report" appState.locale) ] ]

        chartData =
            createTotalChartData metrics summaryReport.totalReport

        totalReport =
            [ viewIndications appState summaryReport.totalReport.indications
            , viewMetrics appState summaryReport.metrics summaryReport.totalReport.metrics chartData
            , hr [] []
            ]

        metrics =
            summaryReport.metrics

        chapters =
            viewChapters appState summaryReport

        metricDescriptions =
            if List.length metrics > 0 then
                [ viewMetricsDescriptions appState metrics ]

            else
                []
    in
    div [ class "questionnaire__summary-report container" ]
        (List.concat [ title, totalReport, chapters, metricDescriptions ])


viewChapters : AppState -> SummaryReport -> List (Html Msg)
viewChapters appState summaryReport =
    List.map (viewChapterReport appState summaryReport) summaryReport.chapterReports


viewChapterReport : AppState -> SummaryReport -> ChapterReport -> Html Msg
viewChapterReport appState summaryReport chapterReport =
    let
        chapterTitle =
            summaryReport.chapters
                |> List.find ((==) chapterReport.chapterUuid << .uuid)
                |> Maybe.unwrap "" .title

        metrics =
            summaryReport.metrics

        chartData =
            createChapterChartData metrics chapterTitle chapterReport
    in
    div []
        [ h3 [] [ text chapterTitle ]
        , viewIndications appState chapterReport.indications
        , viewMetrics appState metrics chapterReport.metrics chartData
        ]


viewMetrics : AppState -> List Metric -> List MetricReport -> ChartJS.Data -> Html Msg
viewMetrics appState metrics metricReports chartData =
    let
        content =
            if List.isEmpty metricReports then
                []

            else if List.length metricReports > 2 then
                [ div [ class "col-xs-12 col-xl-6" ] [ viewMetricsTable appState metrics metricReports ]
                , div [ class "col-xs-12 col-xl-6" ] [ viewMetricsChart chartData ]
                ]

            else
                [ div [ class "col-12" ] [ viewMetricsTable appState metrics metricReports ] ]
    in
    div [ class "row" ] content


viewIndications : AppState -> List IndicationReport -> Html msg
viewIndications appState indications =
    table [ class "indication-table" ] (List.map (viewIndication appState) indications)


viewIndication : AppState -> IndicationReport -> Html msg
viewIndication appState indicationReport =
    case indicationReport of
        AnsweredIndication data ->
            viewAnsweredIndication (String.format (gettext "Answered: %s/%s" appState.locale)) data

        PhasesAnsweredIndication data ->
            viewAnsweredIndication (String.format (gettext "Answered (current phase): %s/%s" appState.locale)) data


viewAnsweredIndication : (List String -> String) -> AnsweredIndicationData -> Html msg
viewAnsweredIndication title data =
    let
        progress =
            toFloat data.answeredQuestions / (toFloat <| data.answeredQuestions + data.unansweredQuestions)

        answered =
            fromInt data.answeredQuestions

        all =
            fromInt <| data.answeredQuestions + data.unansweredQuestions
    in
    tr [ class "indication" ]
        [ td [] [ text <| title [ answered, all ] ]
        , td [] [ viewProgressBar "bg-info" progress ]
        ]


viewMetricsTable : AppState -> List Metric -> List MetricReport -> Html Msg
viewMetricsTable appState metrics metricReports =
    table [ class "table table-metrics-report" ]
        [ thead []
            [ tr []
                [ th [] [ text (gettext "Metrics" appState.locale) ]
                , th [ colspan 2 ] [ text (gettext "Measure" appState.locale) ]
                ]
            ]
        , tbody []
            (List.map (viewMetricReportRow metrics) metricReports)
        ]


viewMetricReportRow : List Metric -> MetricReport -> Html Msg
viewMetricReportRow metrics metricReport =
    tr []
        [ td []
            [ a [ onClick (ScrollToMetric metricReport.metricUuid) ]
                [ text <| getTitleByUuid metrics metricReport.metricUuid
                ]
            ]
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


viewMetricsChart : ChartJS.Data -> Html msg
viewMetricsChart chartData =
    ChartJS.radarChart [ class "metrics-chart", ChartJS.chartData chartData ]


getTitleByUuid : List { a | uuid : String, title : String } -> String -> String
getTitleByUuid items uuid =
    List.find (.uuid >> (==) uuid) items
        |> Maybe.map .title
        |> Maybe.withDefault "Unknown"


viewMetricsDescriptions : AppState -> List Metric -> Html msg
viewMetricsDescriptions appState metrics =
    div []
        (h3 [] [ text (gettext "Metrics explanation" appState.locale) ]
            :: List.map viewMetricDescription metrics
        )


viewMetricDescription : Metric -> Html msg
viewMetricDescription metric =
    div []
        [ h4 [ id (metricId metric.uuid) ] [ text <| metric.title ]
        , Markdown.toHtml [] (Maybe.withDefault "" metric.description)
        ]


metricId : String -> String
metricId metricUuid =
    "metric-" ++ metricUuid



-- Chart helpers


createTotalChartData : List Metric -> TotalReport -> ChartJS.Data
createTotalChartData metrics totalReport =
    let
        data =
            List.map (createDataValue metrics) totalReport.metrics
    in
    createChartData data ""


createChapterChartData : List Metric -> String -> ChapterReport -> ChartJS.Data
createChapterChartData metrics label chapterReport =
    let
        data =
            List.map (createDataValue metrics) chapterReport.metrics
    in
    createChartData data label


createChartData : List ( String, Float ) -> String -> ChartJS.Data
createChartData data label =
    { labels = List.map Tuple.first data
    , datasets =
        [ { label = label
          , borderColor = "rgb(23, 162, 184)"
          , backgroundColor = "rgba(23, 162, 184, 0.5)"
          , pointBackgroundColor = "rgb(23, 162, 184)"
          , data = List.map Tuple.second data
          , stack = Nothing
          }
        ]
    }


createDataValue : List Metric -> MetricReport -> ( String, Float )
createDataValue metrics report =
    let
        label =
            List.find (.uuid >> (==) report.metricUuid) metrics
                |> Maybe.map .title
                |> Maybe.withDefault "Metric"
    in
    ( label, report.measure )
