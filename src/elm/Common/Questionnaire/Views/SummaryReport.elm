module Common.Questionnaire.Views.SummaryReport exposing (view)

import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lf, lgx, lx)
import Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model, chapterReportCanvasId)
import Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModels
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import List.Extra as List
import Round
import String exposing (fromFloat, fromInt)


l_ : String -> AppState -> String
l_ =
    l "Common.Questionnaire.Views.SummaryReport"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Common.Questionnaire.Views.SummaryReport"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Common.Questionnaire.Views.SummaryReport"


view : AppState -> Model -> SummaryReport -> Html Msg
view appState model summaryReport =
    let
        title =
            [ h2 [] [ lgx "questionnaire.summaryReport" appState ] ]

        chapters =
            viewChapters appState model model.metrics summaryReport

        metricDescriptions =
            [ viewMetricsDescriptions appState model.metrics ]
    in
    div [ class "summary-report" ]
        (List.concat [ title, chapters, metricDescriptions ])


viewChapters : AppState -> Model -> List Metric -> SummaryReport -> List (Html Msg)
viewChapters appState model metrics summaryReport =
    List.map (viewChapterReport appState model metrics) summaryReport.chapterReports


viewChapterReport : AppState -> Model -> List Metric -> ChapterReport -> Html Msg
viewChapterReport appState model metrics chapterReport =
    let
        content =
            if List.length chapterReport.metrics == 0 then
                []

            else if List.length chapterReport.metrics > 2 then
                [ div [ class "col-xs-12 col-xl-6" ] [ viewMetricsTable appState metrics chapterReport ]
                , div [ class "col-xs-12 col-xl-6" ] [ viewMetricsChart metrics chapterReport ]
                ]

            else
                [ div [ class "col-12" ] [ viewMetricsTable appState metrics chapterReport ] ]

        chapterTitle =
            model.questionnaire.knowledgeModel
                |> KnowledgeModels.getChapter chapterReport.chapterUuid
                |> Maybe.map .title
                |> Maybe.withDefault "Unknown"
    in
    div []
        [ h3 [] [ text chapterTitle ]
        , viewIndications appState chapterReport.indications
        , div [ class "row" ] content
        ]


viewIndications : AppState -> List IndicationReport -> Html Msg
viewIndications appState indications =
    div [] (List.map (viewIndication appState) indications)


viewIndication : AppState -> IndicationReport -> Html Msg
viewIndication appState indicationReport =
    case indicationReport of
        AnsweredIndication data ->
            viewAnsweredIndication appState data


viewAnsweredIndication : AppState -> AnsweredIndicationData -> Html Msg
viewAnsweredIndication appState data =
    let
        progress =
            toFloat data.answeredQuestions / (toFloat <| data.answeredQuestions + data.unansweredQuestions)

        answered =
            fromInt data.answeredQuestions

        all =
            fromInt <| data.answeredQuestions + data.unansweredQuestions
    in
    div [ class "indication" ]
        [ p [] [ text <| lf_ "answeredIndication.label" [ answered, all ] appState ]
        , viewProgressBar "bg-info" progress
        ]


viewMetricsTable : AppState -> List Metric -> ChapterReport -> Html Msg
viewMetricsTable appState metrics chapterReport =
    table [ class "table table-metrics-report" ]
        [ thead []
            [ tr []
                [ th [] [ lgx "metric" appState ]
                , th [ colspan 2 ] [ lgx "metric.measure" appState ]
                ]
            ]
        , tbody []
            (List.map (viewMetricReportRow metrics) chapterReport.metrics)
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


viewMetricsChart : List Metric -> ChapterReport -> Html Msg
viewMetricsChart _ chapterReport =
    div [ class "metrics-chart" ]
        [ canvas [ id <| chapterReportCanvasId chapterReport ] [] ]


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
