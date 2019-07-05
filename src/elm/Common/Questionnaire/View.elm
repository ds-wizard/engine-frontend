module Common.Questionnaire.View exposing
    ( ViewQuestionnaireConfig
    , viewQuestionnaire
    )

import ActionResult exposing (ActionResult(..))
import Common.ApiError exposing (ApiError)
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa)
import Common.Questionnaire.Models exposing (ActivePage(..), Feedback, FeedbackForm, FormExtraData, Model, QuestionnaireDetail, calculateUnansweredQuestions, chapterReportCanvasId)
import Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(..), MetricReport, SummaryReport)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Common.View.FormGroup as FormGroup
import Common.View.Modal as Modal
import Common.View.Page as Page
import FormEngine.View exposing (FormRenderer, FormViewConfig, viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import KMEditor.Common.Models.Entities exposing (Answer, Chapter, Expert, Level, Metric, Question, ResourcePageReferenceData, URLReferenceData, getQuestionRequiredLevel)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Questionnaires.Common.Questionnaire as Questionnaire
import Roman exposing (toRomanNumber)
import Round
import String exposing (fromFloat, fromInt)


type alias ViewQuestionnaireConfig =
    { showExtraActions : Bool
    , showExtraNavigation : Bool
    , levels : Maybe (List Level)
    , getExtraQuestionClass : String -> Maybe String
    , forceDisabled : Bool
    , createRenderer : List Level -> List Metric -> FormRenderer CustomFormMessage Question Answer ApiError
    }


viewQuestionnaire : ViewQuestionnaireConfig -> AppState -> Model -> Html Msg
viewQuestionnaire cfg appState model =
    let
        level =
            case cfg.levels of
                Just levels ->
                    levelSelection cfg appState model levels model.questionnaire.level

                Nothing ->
                    emptyNode

        extraActions =
            if cfg.showExtraNavigation then
                extraNavigation model.activePage

            else
                emptyNode
    in
    div [ class "Questionnaire" ]
        [ div [ class "chapter-list" ]
            [ level
            , chapterList appState model
            , extraActions
            ]
        , div [ id "questionnaire-body", class "questionnaire-body" ]
            (pageView appState cfg model)
        , feedbackModal model
        ]


levelSelection : ViewQuestionnaireConfig -> AppState -> Model -> List Level -> Int -> Html Msg
levelSelection cfg appState model levels selectedLevel =
    let
        isDisabled =
            cfg.forceDisabled || (not <| Questionnaire.isEditable appState model.questionnaire)
    in
    div [ class "level-selection card bg-light" ]
        [ div [ class "card-body" ]
            [ label [] [ text "Current Phase" ]
            , select [ class "form-control", onInput SetLevel, disabled isDisabled ]
                (List.map (levelSelectionOption selectedLevel) levels)
            ]
        ]


levelSelectionOption : Int -> Level -> Html Msg
levelSelectionOption selectedLevel level =
    option [ value (fromInt level.level), selected (selectedLevel == level.level) ]
        [ text level.title ]


chapterList : AppState -> Model -> Html Msg
chapterList appState model =
    let
        activeChapter =
            case model.activePage of
                PageChapter chapter _ ->
                    Just chapter

                _ ->
                    Nothing
    in
    div [ class "nav nav-pills flex-column" ]
        (List.indexedMap (chapterListChapter appState model activeChapter) model.questionnaire.knowledgeModel.chapters)


chapterListChapter : AppState -> Model -> Maybe Chapter -> Int -> Chapter -> Html Msg
chapterListChapter appState model activeChapter order chapter =
    a
        [ classList [ ( "nav-link", True ), ( "active", activeChapter == Just chapter ) ]
        , onClick <| SetActiveChapter chapter
        ]
        [ span [ class "chapter-number" ] [ text <| (toRomanNumber <| order + 1) ++ ". " ]
        , span [ class "chapter-name" ] [ text chapter.title ]
        , viewChapterAnsweredIndication appState model chapter
        ]


viewChapterAnsweredIndication : AppState -> Model -> Chapter -> Html Msg
viewChapterAnsweredIndication appState model chapter =
    let
        effectiveLevel =
            if appState.config.levelsEnabled then
                model.questionnaire.level

            else
                100

        unanswered =
            calculateUnansweredQuestions appState effectiveLevel model.questionnaire.replies chapter
    in
    if unanswered > 0 then
        span [ class "badge badge-light badge-pill" ] [ text <| fromInt unanswered ]

    else
        fa "check"


extraNavigation : ActivePage -> Html Msg
extraNavigation activePage =
    div [ class "nav nav-pills flex-column" ]
        [ a
            [ classList [ ( "nav-link", True ), ( "active", activePage == PageSummaryReport ) ]
            , onClick ViewSummaryReport
            ]
            [ text "Summary Report" ]
        ]


pageView : AppState -> ViewQuestionnaireConfig -> Model -> List (Html Msg)
pageView appState cfg model =
    case model.activePage of
        PageNone ->
            [ emptyNode ]

        PageChapter chapter form ->
            [ chapterHeader model chapter
            , viewForm (formConfig appState cfg model) form |> Html.map FormMsg
            ]

        PageSummaryReport ->
            [ Page.actionResultView (viewSummary model) model.summaryReport ]


chapterHeader : Model -> Chapter -> Html Msg
chapterHeader model chapter =
    let
        chapterNumber =
            model.questionnaire.knowledgeModel.chapters
                |> List.indexedMap (\i c -> ( i, c ))
                |> List.find (\( i, c ) -> c.uuid == chapter.uuid)
                |> Maybe.map (\( i, c ) -> i + 1)
                |> Maybe.withDefault 1
                |> toRomanNumber
    in
    div []
        [ h2 [] [ text <| chapterNumber ++ ". " ++ chapter.title ]
        , Markdown.toHtml [ class "chapter-description" ] chapter.text
        ]


formConfig : AppState -> ViewQuestionnaireConfig -> Model -> FormViewConfig CustomFormMessage Question Answer ApiError
formConfig appState cfg model =
    { customActions =
        if cfg.showExtraActions then
            [ ( "fa-exclamation-circle", FeedbackMsg ) ]

        else
            []
    , isDesirable =
        if Maybe.isNothing cfg.levels then
            Just <| always False

        else
            Just (getQuestionRequiredLevel >> Maybe.map ((>=) model.questionnaire.level) >> Maybe.withDefault False)
    , disabled = cfg.forceDisabled || (not <| Questionnaire.isEditable appState model.questionnaire)
    , getExtraQuestionClass = cfg.getExtraQuestionClass
    , renderer = cfg.createRenderer (Maybe.withDefault [] cfg.levels) model.metrics
    }


viewSummary : Model -> SummaryReport -> Html Msg
viewSummary model summaryReport =
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
viewMetricsChart metrics chapterReport =
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
                                    [ text <| "issue " ++ fromInt feedback.issueId ]
                                , text "."
                                ]
                            ]

                        Nothing ->
                            [ emptyNode ]

                _ ->
                    feedbackModalContent model

        ( actionName, actionMsg, cancelMsg ) =
            case model.sendingFeedback of
                Success _ ->
                    ( "Done", CloseFeedback, Nothing )

                _ ->
                    ( "Send", SendFeedbackForm, Just <| CloseFeedback )

        modalConfig =
            { modalTitle = "Feedback"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.sendingFeedback
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = cancelMsg
            , dangerous = False
            }
    in
    Modal.confirm modalConfig


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
    , FormGroup.input model.feedbackForm "title" "Title" |> Html.map FeedbackFormMsg
    , FormGroup.textarea model.feedbackForm "content" "Description" |> Html.map FeedbackFormMsg
    ]


feedbackIssue : Feedback -> Html Msg
feedbackIssue feedback =
    li []
        [ a [ href feedback.issueUrl, target "_blank" ]
            [ text feedback.title ]
        ]
