module Common.Questionnaire.View exposing (..)

import Common.Html exposing (emptyNode, fa)
import Common.Questionnaire.Models exposing (ActivePage(..), Feedback, FeedbackForm, FormExtraData, Model, QuestionnaireDetail)
import Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(AnsweredIndication), MetricReport, SummaryReport)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(FeedbackMsg), Msg(..))
import Common.Types exposing (ActionResult(Success, Unset), combine)
import Common.View exposing (fullPageActionResultView, modalView)
import Common.View.Forms exposing (inputGroup, textAreaGroup)
import FormEngine.View exposing (FormViewConfig, viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Chapter, Metric)
import List.Extra as List


type alias ViewQuestionnaireConfig =
    { showExtraActions : Bool
    }


viewQuestionnaire : ViewQuestionnaireConfig -> Model -> Html Msg
viewQuestionnaire cfg model =
    let
        extraActions =
            if cfg.showExtraActions then
                extraNavigation model.activePage
            else
                emptyNode
    in
    div [ class "Questionnaire row" ]
        [ div [ class "col-sm-12 col-md-3 col-lg-3 col-xl-3" ]
            [ chapterList model
            , extraActions
            ]
        , div [ class "col-sm-11 col-md-8 col-lg-8 col-xl-7" ]
            (pageView model)
        , feedbackModal model
        ]


chapterList : Model -> Html Msg
chapterList model =
    let
        activeChapter =
            case model.activePage of
                PageChapter chapter _ ->
                    Just chapter

                _ ->
                    Nothing
    in
    div [ class "nav nav-pills flex-column" ]
        (List.map (chapterListChapter activeChapter) model.questionnaire.knowledgeModel.chapters)


chapterListChapter : Maybe Chapter -> Chapter -> Html Msg
chapterListChapter activeChapter chapter =
    a
        [ classList [ ( "nav-link", True ), ( "active", activeChapter == Just chapter ) ]
        , onClick <| SetActiveChapter chapter
        ]
        [ text chapter.title ]


extraNavigation : ActivePage -> Html Msg
extraNavigation activePage =
    div [ class "nav nav-pills flex-column" ]
        [ a
            [ classList [ ( "nav-link", True ), ( "active", activePage == PageSummaryReport ) ]
            , onClick ViewSummaryReport
            ]
            [ text "Summary Report" ]
        ]


pageView : Model -> List (Html Msg)
pageView model =
    case model.activePage of
        PageNone ->
            [ emptyNode ]

        PageChapter chapter form ->
            [ chapterHeader chapter
            , viewForm formConfig form |> Html.map FormMsg
            ]

        PageSummaryReport ->
            [ fullPageActionResultView (viewSummary model) (combine model.metrics model.summaryReport) ]


chapterHeader : Chapter -> Html Msg
chapterHeader chapter =
    div []
        [ h2 [] [ text chapter.title ]
        , p [ class "chapter-description" ] [ text chapter.text ]
        ]


formConfig : FormViewConfig CustomFormMessage FormExtraData
formConfig =
    { customActions = [ ( "fa-exclamation-circle", FeedbackMsg ) ]
    , viewExtraData = Just viewExtraData
    }


viewExtraData : FormExtraData -> Html msg
viewExtraData data =
    p [ class "extra-data" ]
        [ viewResourcePageReferences data.resourcePageReferences
        , viewUrlReferences data.urlReferences
        ]


viewResourcePageReferences : List String -> Html msg
viewResourcePageReferences references =
    if List.length references == 0 then
        emptyNode
    else
        span []
            ([ fa "book"
             , span [] [ text "Data Stewardship for Open Science:" ]
             ]
                ++ List.map viewResourcePageReference references
            )


viewResourcePageReference : String -> Html msg
viewResourcePageReference shortUuid =
    a [ href <| "/book-references/" ++ shortUuid, target "_blank" ]
        [ text shortUuid ]


viewUrlReferences : List ( String, String ) -> Html msg
viewUrlReferences references =
    if List.length references == 0 then
        emptyNode
    else
        span []
            ([ fa "external-link"
             , span [] [ text "External links:" ]
             ]
                ++ List.map viewUrlReference references
            )


viewUrlReference : ( String, String ) -> Html msg
viewUrlReference ( anchor, url ) =
    a [ href url, target "_blank" ]
        [ text anchor ]


viewSummary : Model -> ( List Metric, SummaryReport ) -> Html Msg
viewSummary model ( metrics, summaryReport ) =
    let
        title =
            [ h2 [] [ text "Summary report" ] ]

        chapters =
            viewChapters model metrics summaryReport

        metricDescriptions =
            [ viewMetricsDescriptions metrics ]
    in
    div [ class "summary-report" ]
        (List.concat [ title, chapters, metricDescriptions ])


viewChapters : Model -> List Metric -> SummaryReport -> List (Html Msg)
viewChapters model metrics summaryReport =
    List.map (viewChapterReport model metrics) summaryReport.chapterReports


viewChapterReport : Model -> List Metric -> ChapterReport -> Html Msg
viewChapterReport model metrics chapterReport =
    div []
        [ h3 [] [ text <| getTitleByUuid model.questionnaire.knowledgeModel.chapters chapterReport.chapterUuid ]
        , viewIndications chapterReport.indications
        , viewMetrics metrics chapterReport
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
            toFloat data.answered / (toFloat <| data.answered + data.unanswered)
    in
    div [ class "indication" ]
        [ p [] [ text <| "Answered: " ++ toString data.answered ++ "/" ++ (toString <| data.answered + data.unanswered) ]
        , viewProgressBar "bg-info" progress
        ]


viewMetrics : List Metric -> ChapterReport -> Html Msg
viewMetrics metrics chapterReport =
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
        [ td [] [ text <| getTitleByUuid metrics metricReport.uuid ]
        , td [] [ text <| toString metricReport.measure ]
        , td [] [ viewProgressBarWithColors metricReport.measure ]
        ]


viewProgressBarWithColors : Float -> Html msg
viewProgressBarWithColors value =
    let
        colorClass =
            if value < 0.33 then
                "bg-danger"
            else if value < 0.66 then
                "bg-warning"
            else
                "bg-success"
    in
    viewProgressBar colorClass value


viewProgressBar : String -> Float -> Html msg
viewProgressBar colorClass value =
    let
        width =
            (toString <| value * 100) ++ "%"
    in
    div [ class "progress" ]
        [ div [ class <| "progress-bar " ++ colorClass, style [ ( "width", width ) ] ] [] ]


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


feedbackModal : Model -> Html Msg
feedbackModal model =
    let
        visible =
            case model.feedback of
                Unset ->
                    False

                _ ->
                    True

        modalContent =
            case model.sendingFeedback of
                Success _ ->
                    case model.feedbackResult of
                        Just feedback ->
                            [ p []
                                [ text "You can follow the GitHub "
                                , a [ href feedback.issueUrl, target "_blank" ]
                                    [ text <| "issue " ++ toString feedback.issueId ]
                                , text "."
                                ]
                            ]

                        Nothing ->
                            [ emptyNode ]

                _ ->
                    feedbackModalContent model

        ( actionName, actionMsg ) =
            case model.sendingFeedback of
                Success _ ->
                    ( "Done", CloseFeedback )

                _ ->
                    ( "Send", SendFeedbackForm )

        modalConfig =
            { modalTitle = "Feedback"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.sendingFeedback
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = CloseFeedback
            }
    in
    modalView modalConfig


feedbackModalContent : Model -> List (Html Msg)
feedbackModalContent model =
    let
        feedbackList =
            case model.feedback of
                Success feedbacks ->
                    if List.length feedbacks > 0 then
                        div []
                            [ div []
                                [ text "There are already some issues reported with this question" ]
                            , ul [] (List.map feedbackIssue feedbacks)
                            ]
                    else
                        emptyNode

                _ ->
                    emptyNode
    in
    [ div [ class "alert alert-info" ]
        [ text "If you found something wrong with the question, you can send us your feedback how to improve it." ]
    , feedbackList
    , inputGroup model.feedbackForm "title" "Title" |> Html.map FeedbackFormMsg
    , textAreaGroup model.feedbackForm "content" "Description" |> Html.map FeedbackFormMsg
    ]


feedbackIssue : Feedback -> Html Msg
feedbackIssue feedback =
    li []
        [ a [ href feedback.issueUrl, target "_blank" ]
            [ text feedback.title ]
        ]
