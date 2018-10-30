module Common.Questionnaire.View exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode, fa)
import Common.Questionnaire.Models exposing (ActivePage(..), Feedback, FeedbackForm, FormExtraData, Model, QuestionnaireDetail, calculateUnansweredQuestions)
import Common.Questionnaire.Models.SummaryReport exposing (AnsweredIndicationData, ChapterReport, IndicationReport(AnsweredIndication), MetricReport, SummaryReport)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(FeedbackMsg), Msg(..))
import Common.View exposing (fullPageActionResultView, modalView)
import Common.View.Forms exposing (inputGroup, textAreaGroup)
import FormEngine.View exposing (FormViewConfig, viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import KMEditor.Common.Models.Entities exposing (Chapter, Expert, Level, Metric, ResourcePageReferenceData, URLReferenceData)
import List.Extra as List
import Round


type alias ViewQuestionnaireConfig =
    { showExtraActions : Bool
    , levels : Maybe (List Level)
    }


viewQuestionnaire : ViewQuestionnaireConfig -> Model -> Html Msg
viewQuestionnaire cfg model =
    let
        levels =
            case cfg.levels of
                Just levels ->
                    levelSelection levels model.questionnaire.level

                Nothing ->
                    emptyNode

        extraActions =
            if cfg.showExtraActions then
                extraNavigation model.activePage
            else
                emptyNode
    in
    div [ class "Questionnaire row" ]
        [ div [ class "col-sm-12 col-md-4 col-lg-4 col-xl-3" ]
            [ levels
            , chapterList model
            , extraActions
            ]
        , div [ class "col-sm-11 col-md-8 col-lg-8 col-xl-7" ]
            (pageView (cfg.levels |> Maybe.withDefault []) model)
        , feedbackModal model
        ]


levelSelection : List Level -> Int -> Html Msg
levelSelection levels selectedLevel =
    div [ class "level-selection card bg-light" ]
        [ div [ class "card-body" ]
            [ label [] [ text "Current Phase" ]
            , select [ class "form-control", onInput SetLevel ]
                (List.map (levelSelectionOption selectedLevel) levels)
            ]
        ]


levelSelectionOption : Int -> Level -> Html Msg
levelSelectionOption selectedLevel level =
    option [ value (toString level.level), selected (selectedLevel == level.level) ]
        [ text level.title ]


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
        (List.map (chapterListChapter model activeChapter) model.questionnaire.knowledgeModel.chapters)


chapterListChapter : Model -> Maybe Chapter -> Chapter -> Html Msg
chapterListChapter model activeChapter chapter =
    a
        [ classList [ ( "nav-link", True ), ( "active", activeChapter == Just chapter ) ]
        , onClick <| SetActiveChapter chapter
        ]
        [ text chapter.title
        , viewChapterAnsweredIndication model chapter
        ]


viewChapterAnsweredIndication : Model -> Chapter -> Html Msg
viewChapterAnsweredIndication model chapter =
    let
        unanswered =
            calculateUnansweredQuestions model.questionnaire.level model.questionnaire.replies chapter
    in
    if unanswered > 0 then
        span [ class "badge badge-light badge-pill" ] [ text <| toString unanswered ]
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


pageView : List Level -> Model -> List (Html Msg)
pageView levels model =
    case model.activePage of
        PageNone ->
            [ emptyNode ]

        PageChapter chapter form ->
            [ chapterHeader chapter
            , viewForm (formConfig levels) form |> Html.map FormMsg
            ]

        PageSummaryReport ->
            [ fullPageActionResultView (viewSummary model) (ActionResult.combine model.metrics model.summaryReport) ]


chapterHeader : Chapter -> Html Msg
chapterHeader chapter =
    div []
        [ h2 [] [ text chapter.title ]
        , p [ class "chapter-description" ] [ text chapter.text ]
        ]


formConfig : List Level -> FormViewConfig CustomFormMessage FormExtraData
formConfig levels =
    { customActions = [ ( "fa-exclamation-circle", FeedbackMsg ) ]
    , viewExtraData = Just (viewExtraData levels)
    }


viewExtraData : List Level -> FormExtraData -> Html msg
viewExtraData levels data =
    p [ class "extra-data" ]
        [ viewRequiredLevel levels data.requiredLevel
        , viewResourcePageReferences data.resourcePageReferences
        , viewUrlReferences data.urlReferences
        , viewExperts data.experts
        ]


viewRequiredLevel : List Level -> Maybe Int -> Html msg
viewRequiredLevel levels questionLevel =
    case List.find (.level >> (==) (questionLevel |> Maybe.withDefault 0)) levels of
        Just level ->
            span []
                [ span [ class "caption" ]
                    [ fa "check-square-o"
                    , text "Desirable: "
                    , span [] [ text level.title ]
                    ]
                ]

        Nothing ->
            emptyNode


type alias ViewExtraItemsConfig a msg =
    { icon : String
    , label : String
    , viewItem : a -> Html msg
    }


viewExtraItems : ViewExtraItemsConfig a msg -> List a -> Html msg
viewExtraItems cfg list =
    if List.length list == 0 then
        emptyNode
    else
        let
            items =
                List.map cfg.viewItem list
                    |> List.intersperse (span [ class "separator" ] [ text ", " ])
        in
        span []
            (span [ class "caption" ] [ fa cfg.icon, text (cfg.label ++ ": ") ] :: items)


viewResourcePageReferences : List ResourcePageReferenceData -> Html msg
viewResourcePageReferences =
    viewExtraItems
        { icon = "book"
        , label = "Data Stewardship for Open Science"
        , viewItem = viewResourcePageReference
        }


viewResourcePageReference : ResourcePageReferenceData -> Html msg
viewResourcePageReference data =
    a [ href <| "/book-references/" ++ data.shortUuid, target "_blank" ]
        [ text data.shortUuid ]


viewUrlReferences : List URLReferenceData -> Html msg
viewUrlReferences =
    viewExtraItems
        { icon = "external-link"
        , label = "External Links"
        , viewItem = viewUrlReference
        }


viewUrlReference : URLReferenceData -> Html msg
viewUrlReference data =
    a [ href data.url, target "_blank" ]
        [ text data.label ]


viewExperts : List Expert -> Html msg
viewExperts =
    viewExtraItems
        { icon = "address-book-o"
        , label = "Experts"
        , viewItem = viewExpert
        }


viewExpert : Expert -> Html msg
viewExpert expert =
    span []
        [ text expert.name
        , text " ("
        , a [ href <| "mailto:" ++ expert.email ] [ text expert.email ]
        , text ")"
        ]


ifNotEmpty : List a -> (List a -> Html msg) -> Html msg
ifNotEmpty list fn =
    if List.length list == 0 then
        emptyNode
    else
        fn list


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
            toFloat data.answeredQuestions / (toFloat <| data.answeredQuestions + data.unansweredQuestions)
    in
    div [ class "indication" ]
        [ p [] [ text <| "Answered: " ++ toString data.answeredQuestions ++ "/" ++ (toString <| data.answeredQuestions + data.unansweredQuestions) ]
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
        [ td [] [ text <| getTitleByUuid metrics metricReport.metricUuid ]
        , td [] [ text <| Round.round 2 metricReport.measure ]
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
