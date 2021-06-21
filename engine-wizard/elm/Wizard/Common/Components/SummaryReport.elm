module Wizard.Common.Components.SummaryReport exposing
    ( Context
    , Model
    , Msg
    , fetchData
    , fetchData2
    , init
    , update
    , view
    , viewIndications
    )

import ActionResult exposing (ActionResult(..))
import ChartJS exposing (ChartConfig)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Round
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport, TotalReport)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lf, lg, lgx, lx)
import String exposing (fromFloat, fromInt)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Components.SummaryReport"


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
    , metrics : List Metric
    }



-- Update


type Msg
    = GetSummaryReportComplete (Result ApiError SummaryReport)


fetchData : AppState -> Uuid -> Model -> ( Model, Cmd Msg )
fetchData appState questionnaireUuid model =
    ( { model | summaryReport = Loading }
    , QuestionnairesApi.getSummaryReport questionnaireUuid appState GetSummaryReportComplete
    )


fetchData2 : AppState -> Uuid -> Cmd Msg
fetchData2 appState questionnaireUuid =
    QuestionnairesApi.getSummaryReport questionnaireUuid appState GetSummaryReportComplete


update : Msg -> AppState -> Context -> Model -> ( Model, Cmd msg )
update msg appState ctx model =
    case msg of
        GetSummaryReportComplete result ->
            case result of
                Ok summaryReport ->
                    let
                        chapters =
                            KnowledgeModel.getChapters ctx.questionnaire.knowledgeModel

                        chapterChartsConfigs =
                            List.map (createChapterChartConfig ctx.metrics chapters) summaryReport.chapterReports

                        totalChartConfig =
                            createTotalChartConfig ctx.metrics summaryReport.totalReport

                        cmds =
                            List.map
                                (Ports.drawMetricsChart << ChartJS.encodeChartConfig)
                                ([ totalChartConfig ] ++ chapterChartsConfigs)
                    in
                    ( { model | summaryReport = Success summaryReport }
                    , Cmd.batch cmds
                    )

                Err error ->
                    ( { model | summaryReport = ApiError.toActionResult appState (lg "apiError.questionnaires.summaryReport.fetchError" appState) error }
                    , Cmd.none
                    )



-- View


view : AppState -> Context -> Model -> Html msg
view appState context model =
    div [ class "Projects__Detail__Content Projects__Detail__Content--Metrics" ]
        [ Page.actionResultView appState (viewContent appState context) model.summaryReport
        ]


viewContent : AppState -> Context -> SummaryReport -> Html msg
viewContent appState ctx summaryReport =
    let
        title =
            [ h2 [] [ lgx "questionnaire.summaryReport" appState ] ]

        totalReport =
            [ viewIndications appState summaryReport.totalReport.indications
            , viewMetrics appState ctx summaryReport.totalReport.metrics totalReportId
            , hr [] []
            ]

        chapters =
            viewChapters appState ctx summaryReport

        metricDescriptions =
            [ viewMetricsDescriptions appState ctx.metrics ]
    in
    div [ class "questionnaire__summary-report container" ]
        (List.concat [ title, totalReport, chapters, metricDescriptions ])


viewChapters : AppState -> Context -> SummaryReport -> List (Html msg)
viewChapters appState ctx summaryReport =
    List.map (viewChapterReport appState ctx) summaryReport.chapterReports


viewChapterReport : AppState -> Context -> ChapterReport -> Html msg
viewChapterReport appState ctx chapterReport =
    let
        chapterTitle =
            ctx.questionnaire.knowledgeModel
                |> KnowledgeModel.getChapter chapterReport.chapterUuid
                |> Maybe.map .title
                |> Maybe.withDefault ""
    in
    div []
        [ h3 [] [ text chapterTitle ]
        , viewIndications appState chapterReport.indications
        , viewMetrics appState ctx chapterReport.metrics chapterReport.chapterUuid
        ]


viewMetrics : AppState -> Context -> List MetricReport -> String -> Html msg
viewMetrics appState ctx metricReports canvasId =
    let
        content =
            if List.length metricReports == 0 then
                []

            else if List.length metricReports > 2 then
                [ div [ class "col-xs-12 col-xl-6" ] [ viewMetricsTable appState ctx.metrics metricReports ]
                , div [ class "col-xs-12 col-xl-6" ] [ viewMetricsChart ctx.metrics canvasId ]
                ]

            else
                [ div [ class "col-12" ] [ viewMetricsTable appState ctx.metrics metricReports ] ]
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

        LevelsAnsweredIndication data ->
            viewAnsweredIndication appState (lf_ "levelsAnsweredIndication.label") data


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


viewMetricsTable : AppState -> List Metric -> List MetricReport -> Html msg
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


viewMetricReportRow : List Metric -> MetricReport -> Html msg
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


viewMetricsChart : List Metric -> String -> Html msg
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



-- Chart helpers


createTotalChartConfig : List Metric -> TotalReport -> ChartConfig
createTotalChartConfig metrics totalReport =
    let
        data =
            List.map (createDataValue metrics) totalReport.metrics
    in
    createChartConfig data "" totalReportId


createChapterChartConfig : List Metric -> List Chapter -> ChapterReport -> ChartConfig
createChapterChartConfig metrics chapters chapterReport =
    let
        data =
            List.map (createDataValue metrics) chapterReport.metrics

        label =
            List.find (.uuid >> (==) chapterReport.chapterUuid) chapters
                |> Maybe.map .title
                |> Maybe.withDefault ""
    in
    createChartConfig data label chapterReport.chapterUuid


createChartConfig : List ( String, Float ) -> String -> String -> ChartConfig
createChartConfig data label canvasId =
    { targetId = reportCanvasId canvasId
    , data =
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


reportCanvasId : String -> String
reportCanvasId canvasId =
    "report-" ++ canvasId


totalReportId : String
totalReportId =
    "total"
