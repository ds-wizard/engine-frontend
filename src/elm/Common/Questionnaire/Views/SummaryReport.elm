module Common.Questionnaire.Views.SummaryReport exposing (view)

import Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model, chapterReportCanvasId)
import Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Common.Models.Entities exposing (Answer, Chapter, Expert, Level, Metric, Question, ResourcePageReferenceData, URLReferenceData)
import List.Extra as List
import Round
import String exposing (fromFloat, fromInt)


view : Model -> SummaryReport -> Html Msg
view model summaryReport =
    let
        title =
            [ h2 [] [ text "Summary report" ] ]

        chapters =
            viewChapters model model.metrics summaryReport

        metricDescriptions =
            [ viewMetricsDescriptions model.metrics ]
    in
    div [ class "summary-report" ]
        (List.concat [ title, chapters, metricDescriptions ])


viewChapters : Model -> List Metric -> SummaryReport -> List (Html Msg)
viewChapters model metrics summaryReport =
    List.map (viewChapterReport model metrics) summaryReport.chapterReports


viewChapterReport : Model -> List Metric -> ChapterReport -> Html Msg
viewChapterReport model metrics chapterReport =
    let
        content =
            if List.length chapterReport.metrics == 0 then
                []

            else if List.length chapterReport.metrics > 2 then
                [ div [ class "col-xs-12 col-xl-6" ] [ viewMetricsTable metrics chapterReport ]
                , div [ class "col-xs-12 col-xl-6" ] [ viewMetricsChart metrics chapterReport ]
                ]

            else
                [ div [ class "col-12" ] [ viewMetricsTable metrics chapterReport ] ]
    in
    div []
        [ h3 [] [ text <| getTitleByUuid model.questionnaire.knowledgeModel.chapters chapterReport.chapterUuid ]
        , viewIndications chapterReport.indications
        , div [ class "row" ] content
        ]


viewIndications : List IndicationReport -> Html Msg
viewIndications indications =
    div [] (List.map viewIndication indications)


viewIndication : IndicationReport -> Html Msg
viewIndication indicationReport =
    case indicationReport of
        AnsweredIndication data ->
            viewAnsweredIndication data


viewAnsweredIndication : AnsweredIndicationData -> Html Msg
viewAnsweredIndication data =
    let
        progress =
            toFloat data.answeredQuestions / (toFloat <| data.answeredQuestions + data.unansweredQuestions)
    in
    div [ class "indication" ]
        [ p [] [ text <| "Answered: " ++ fromInt data.answeredQuestions ++ "/" ++ (fromInt <| data.answeredQuestions + data.unansweredQuestions) ]
        , viewProgressBar "bg-info" progress
        ]


viewMetricsTable : List Metric -> ChapterReport -> Html Msg
viewMetricsTable metrics chapterReport =
    table [ class "table table-metrics-report" ]
        [ thead []
            [ tr []
                [ th [] [ text "Metric" ]
                , th [ colspan 2 ] [ text "Measure" ]
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


viewMetricsDescriptions : List Metric -> Html msg
viewMetricsDescriptions metrics =
    div []
        ([ h3 [] [ text "Metrics Explanation" ] ]
            ++ List.map viewMetricDescription metrics
        )


viewMetricDescription : Metric -> Html msg
viewMetricDescription metric =
    div []
        [ h4 [] [ text <| metric.abbreviation ++ " - " ++ metric.title ]
        , p [ class "text-justify" ] [ text metric.description ]
        ]
