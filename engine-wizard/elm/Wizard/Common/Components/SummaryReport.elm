module Wizard.Common.Components.SummaryReport exposing
    ( Context
    , Model
    , Msg
    , fetchData
    , init
    , update
    , view
    , viewIndications
    )

import ActionResult exposing (ActionResult(..))
import ChartJS
import Html exposing (Html, a, div, h2, h3, h4, hr, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, id, style)
import Html.Events exposing (onClick)
import List.Extra as List
import Round
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport, TotalReport)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lf, lg, lgx, lx)
import Shared.Markdown as Markdown
import String exposing (fromFloat, fromInt)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.Components.SummaryReport"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Components.SummaryReport"



-- Model


type alias Model =
    { summaryReport : ActionResult SummaryReport
    }


init : Model
init =
    { summaryReport = Loading }


type alias Context =
    { questionnaire : QuestionnaireDetail
    }



-- Update


type Msg
    = GetSummaryReportComplete (Result ApiError SummaryReport)
    | ScrollToMetric String


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState questionnaireUuid =
    QuestionnairesApi.getSummaryReport questionnaireUuid appState GetSummaryReportComplete


update : Msg -> AppState -> Model -> ( Model, Cmd msg )
update msg appState model =
    case msg of
        GetSummaryReportComplete result ->
            case result of
                Ok summaryReport ->
                    ( { model | summaryReport = Success summaryReport }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | summaryReport = ApiError.toActionResult appState (lg "apiError.questionnaires.summaryReport.fetchError" appState) error }
                    , Cmd.none
                    )

        ScrollToMetric metric ->
            ( model, Ports.scrollIntoView ("#" ++ metricId metric) )



-- View


view : AppState -> Context -> Model -> Html Msg
view appState context model =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Metrics" ]
        [ Page.actionResultView appState (viewContent appState context) model.summaryReport
        ]


viewContent : AppState -> Context -> SummaryReport -> Html Msg
viewContent appState ctx summaryReport =
    let
        title =
            [ h2 [] [ lgx "questionnaire.summaryReport" appState ] ]

        chartData =
            createTotalChartData metrics summaryReport.totalReport

        totalReport =
            [ viewIndications appState summaryReport.totalReport.indications
            , viewMetrics appState ctx summaryReport.totalReport.metrics chartData
            , hr [] []
            ]

        metrics =
            KnowledgeModel.getMetrics ctx.questionnaire.knowledgeModel

        chapters =
            viewChapters appState ctx summaryReport

        metricDescriptions =
            if List.length metrics > 0 then
                [ viewMetricsDescriptions appState metrics ]

            else
                []
    in
    div [ class "questionnaire__summary-report container" ]
        (List.concat [ title, totalReport, chapters, metricDescriptions ])


viewChapters : AppState -> Context -> SummaryReport -> List (Html Msg)
viewChapters appState ctx summaryReport =
    List.map (viewChapterReport appState ctx) summaryReport.chapterReports


viewChapterReport : AppState -> Context -> ChapterReport -> Html Msg
viewChapterReport appState ctx chapterReport =
    let
        chapterTitle =
            ctx.questionnaire.knowledgeModel
                |> KnowledgeModel.getChapter chapterReport.chapterUuid
                |> Maybe.map .title
                |> Maybe.withDefault ""

        metrics =
            KnowledgeModel.getMetrics ctx.questionnaire.knowledgeModel

        chapters =
            KnowledgeModel.getChapters ctx.questionnaire.knowledgeModel

        chartData =
            createChapterChartData metrics chapters chapterReport
    in
    div []
        [ h3 [] [ text chapterTitle ]
        , viewIndications appState chapterReport.indications
        , viewMetrics appState ctx chapterReport.metrics chartData
        ]


viewMetrics : AppState -> Context -> List MetricReport -> ChartJS.Data -> Html Msg
viewMetrics appState ctx metricReports chartData =
    let
        content =
            if List.length metricReports == 0 then
                []

            else if List.length metricReports > 2 then
                [ div [ class "col-xs-12 col-xl-6" ] [ viewMetricsTable appState ctx metricReports ]
                , div [ class "col-xs-12 col-xl-6" ] [ viewMetricsChart chartData ]
                ]

            else
                [ div [ class "col-12" ] [ viewMetricsTable appState ctx metricReports ] ]
    in
    div [ class "row" ] content


viewIndications : AppState -> List IndicationReport -> Html msg
viewIndications appState indications =
    table [ class "indication-table" ] (List.map (viewIndication appState) indications)


viewIndication : AppState -> IndicationReport -> Html msg
viewIndication appState indicationReport =
    case indicationReport of
        AnsweredIndication data ->
            viewAnsweredIndication appState (lf_ "answeredIndication.label") data

        PhasesAnsweredIndication data ->
            viewAnsweredIndication appState (lf_ "phasesAnsweredIndication.label") data


viewAnsweredIndication : AppState -> (List String -> AppState -> String) -> AnsweredIndicationData -> Html msg
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


viewMetricsTable : AppState -> Context -> List MetricReport -> Html Msg
viewMetricsTable appState ctx metricReports =
    let
        metrics =
            KnowledgeModel.getMetrics ctx.questionnaire.knowledgeModel
    in
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
        (h3 [] [ lx_ "metricsDescriptions.metricsExplanation" appState ]
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


createChapterChartData : List Metric -> List Chapter -> ChapterReport -> ChartJS.Data
createChapterChartData metrics chapters chapterReport =
    let
        data =
            List.map (createDataValue metrics) chapterReport.metrics

        label =
            List.find (.uuid >> (==) chapterReport.chapterUuid) chapters
                |> Maybe.map .title
                |> Maybe.withDefault ""
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
