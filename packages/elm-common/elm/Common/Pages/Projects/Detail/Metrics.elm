module Common.Pages.Projects.Detail.Metrics exposing (view)

{-| The metrics tab of one project: the summary report the server computes from
the answers — how much of the questionnaire is answered and how each metric
scores, for the project as a whole and then chapter by chapter.

It is a page module and not a `Common.Components.*` one because the project's
metrics tab is the only thing that shows a summary report. `Dsw/Components/*` is
for what two features need (PORTING, "Structure"); moving it there the day a
second one appears is a rename, while putting it there now would be a promise
nothing keeps.

**The numbers are a table, and the picture is beside it.** Each measure is
written out, with a bar colored on the value scale behind it (`Ui.ValueScale`:
red at 0, yellow at 0.5, green at 1), so the value never depends on the length
or the color of a stripe (1.4.1). A report with three metrics or more is also
drawn as a radar, as the wizard drew it — beside the table where the page is
wide enough, under it where it is not. That is decided by the width the tab is
given rather than the window's, because the same window leaves the tab two
different widths with the side panel and the navigation open or closed.

**A metric's name takes you to what it means.** Each one is a real link to its
explanation at the foot of the tab, where the wizard had a click handler on an
`a` with no `href`, which no keyboard could reach. The application acts on the
fragment (`Dsw`, `Ui.AppShell.focusFragment`): the explanation's term takes
focus as well as the view, and the tab is not loaded again for a jump within it.

**Chapter and metric names come from the knowledge model**, so they are stored
in its own language and are translated with the knowledge model's locale before
they are rendered, as they were in the wizard.

-}

import Common.Api.Models.KnowledgeModel.Chapter as Chapter
import Common.Api.Models.KnowledgeModel.Metric as Metric exposing (Metric)
import Common.Api.Models.SummaryReport exposing (ChapterReport, IndicationReport(..), MetricReport, SummaryReport)
import Common.Api.Models.SummaryReport.AnsweredIndicationData exposing (AnsweredIndicationData)
import Common.Components.Markdown as Markdown
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import List.Extra as List
import Maybe.Extra as Maybe
import Round
import String.Format as String
import Ui.Chart as Chart
import Ui.DescriptionList as DescriptionList
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Page.Content as Content
import Ui.ProgressBar as ProgressBar
import Ui.Separator as Separator
import Ui.Table as Table
import Ui.Text as Text



-- VIEW


view : AppState -> SummaryReport -> Html msg
view appState rawSummaryReport =
    let
        summaryReport =
            localize rawSummaryReport
    in
    Content.content
        { title = gettext "Summary Report" appState.locale
        , body =
            [ viewIndications appState summaryReport.totalReport.indications
            , viewMetrics appState
                summaryReport.metrics
                (gettext "Summary Report" appState.locale)
                summaryReport.totalReport.metrics
            , Separator.line
            , viewSections
                (List.map (viewChapterReport appState summaryReport) summaryReport.chapterReports
                    ++ viewMetricsDescriptions appState summaryReport.metrics
                )
            ]
        }
        |> Content.withinPage
        |> Content.toHtml


localize : SummaryReport -> SummaryReport
localize summaryReport =
    let
        knowledgeModelLocale =
            Maybe.withDefault Gettext.defaultLocale summaryReport.locale
    in
    { summaryReport
        | chapters = List.map (Chapter.localize knowledgeModelLocale) summaryReport.chapters
        , metrics = List.map (Metric.localize knowledgeModelLocale) summaryReport.metrics
    }


{-| The chapter reports and the explanations under them, further apart than the
parts of one report are, so that where one chapter ends and the next begins is
plain at a glance.
-}
viewSections : List (Html msg) -> Html msg
viewSections =
    div [ class "c-project-metrics-sections" ]


viewChapterReport : AppState -> SummaryReport -> ChapterReport -> Html msg
viewChapterReport appState summaryReport chapterReport =
    let
        chapterTitle =
            summaryReport.chapters
                |> List.find ((==) chapterReport.chapterUuid << .uuid)
                |> Maybe.unwrap "" .title
    in
    Layout.stack
        [ Text.subtitle chapterTitle
        , viewIndications appState chapterReport.indications
        , viewMetrics appState summaryReport.metrics chapterTitle chapterReport.metrics
        ]



-- VIEW - INDICATIONS


{-| How far the questionnaire has got. Written out and drawn: the sentence is
the figure, and the bar is the same figure to the eye.
-}
viewIndications : AppState -> List IndicationReport -> Html msg
viewIndications appState indications =
    Layout.stack (List.map (viewIndication appState) indications)


viewIndication : AppState -> IndicationReport -> Html msg
viewIndication appState indicationReport =
    case indicationReport of
        AnsweredIndication data ->
            viewAnsweredIndication appState (String.format (gettext "Answered: %s/%s" appState.locale)) data

        PhasesAnsweredIndication data ->
            viewAnsweredIndication appState (String.format (gettext "Answered (current phase): %s/%s" appState.locale)) data


viewAnsweredIndication : AppState -> (List String -> String) -> AnsweredIndicationData -> Html msg
viewAnsweredIndication appState toTitle data =
    let
        allQuestions =
            data.answeredQuestions + data.unansweredQuestions

        title =
            toTitle [ String.fromInt data.answeredQuestions, String.fromInt allQuestions ]
    in
    Layout.stack
        [ Text.body title
        , ProgressBar.toHtmlWithWords (UiWords.progressBar appState)
            { label = title
            , value = toFloat data.answeredQuestions
            , max = toFloat allQuestions
            , tone = ProgressBar.Brand
            }
        ]



-- VIEW - METRICS


{-| The table, and with three metrics or more the radar beside it — two
metrics are a line and one is a point, neither of which is a shape worth
drawing. `label` names the chart: the chapter, or the report as a whole.
-}
viewMetrics : AppState -> List Metric -> String -> List MetricReport -> Html msg
viewMetrics appState metrics label metricReports =
    if List.isEmpty metricReports then
        text ""

    else if List.length metricReports > 2 then
        div [ class "c-project-metrics" ]
            [ div [ class "c-project-metrics__layout" ]
                [ viewMetricsTable appState metrics metricReports
                , viewMetricsChart appState metrics label metricReports
                ]
            ]

    else
        viewMetricsTable appState metrics metricReports


viewMetricsTable : AppState -> List Metric -> List MetricReport -> Html msg
viewMetricsTable appState metrics metricReports =
    Table.table
        [ Table.column (gettext "Metrics" appState.locale) (viewMetricName metrics)
        , Table.column (gettext "Measure" appState.locale) (viewMeasure appState metrics)
        ]
        metricReports
        |> Table.withRowTestId (\row -> "metrics_row_" ++ row.metricUuid)
        |> Table.withWords (UiWords.table appState)
        |> Table.toHtml


{-| The name, as a link to the explanation — when there is one to go to. A
metric the report does not describe keeps a plain name rather than a link that
lands nowhere.
-}
viewMetricName : List Metric -> MetricReport -> Html msg
viewMetricName metrics metricReport =
    case findMetric metrics metricReport.metricUuid of
        Just metric ->
            Link.link { label = metric.title, url = "#" ++ metricId metric.uuid }
                |> Link.toHtml

        Nothing ->
            text unknownMetric


{-| The measure and how full it is, in one cell: the bar is named after the
metric, so out of the row it is still clear which measure it is. Its color is
the value scale's for the measure, which is the figure beside it said again.
-}
viewMeasure : AppState -> List Metric -> MetricReport -> Html msg
viewMeasure appState metrics metricReport =
    Layout.inline
        [ text (Round.round 2 metricReport.measure)
        , ProgressBar.toHtmlWithWords (UiWords.progressBar appState)
            { label = metricTitle metrics metricReport.metricUuid
            , value = metricReport.measure
            , max = 1
            , tone = ProgressBar.ByValue
            }
        ]


{-| The same measures as the table beside it, as a shape. The spokes run from
0 to 1 whatever the scores, as the measures do, so a project scoring 0.4
everywhere looks like one. One series with nothing to tell it from, so no
legend.
-}
viewMetricsChart : AppState -> List Metric -> String -> List MetricReport -> Html msg
viewMetricsChart appState metrics label metricReports =
    Chart.radar
        { label = label
        , categories = List.map (metricTitle metrics << .metricUuid) metricReports
        , series =
            [ Chart.series
                { label = gettext "Measure" appState.locale
                , values = List.map .measure metricReports
                }
            ]
        }
        |> Chart.withRange { min = 0, max = 1 }
        |> Chart.withLegend False
        |> Chart.withHiddenCaption
        |> Chart.toHtml


findMetric : List Metric -> String -> Maybe Metric
findMetric metrics uuid =
    List.find (.uuid >> (==) uuid) metrics


metricTitle : List Metric -> String -> String
metricTitle metrics uuid =
    findMetric metrics uuid
        |> Maybe.unwrap unknownMetric .title


unknownMetric : String
unknownMetric =
    "Unknown"


{-| Where a metric's name takes you, as the wizard named it.
-}
metricId : String -> String
metricId metricUuid =
    "metric-" ++ metricUuid


{-| What each metric means, under the tables that use them. A description list,
so a metric's name and its explanation are tied together in the markup rather
than by sitting next to each other; each term is the target of the links to it.
-}
viewMetricsDescriptions : AppState -> List Metric -> List (Html msg)
viewMetricsDescriptions appState metrics =
    if List.isEmpty metrics then
        []

    else
        [ Layout.stack
            [ Text.subtitle (gettext "Metrics explanation" appState.locale)
            , DescriptionList.toHtml (List.map viewMetricDescription metrics)
            ]
        ]


viewMetricDescription : Metric -> DescriptionList.Entry msg
viewMetricDescription metric =
    DescriptionList.withHtml
        { term = metric.title
        , description = [ Markdown.view { headingOffset = 3 } (Maybe.withDefault "" metric.description) ]
        }
        |> DescriptionList.withTargetId (metricId metric.uuid)
