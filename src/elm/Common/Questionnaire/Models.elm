module Common.Questionnaire.Models exposing
    ( ActivePage(..)
    , FormExtraData
    , Model
    , addLabel
    , calculateUnansweredQuestions
    , chapterReportCanvasId
    , createChapterForm
    , createChartConfig
    , getActiveChapter
    , getReply
    , initialModel
    , removeLabel
    , removeLabelsFromItem
    , setActiveChapter
    , setLevel
    , updateReplies
    )

import ActionResult exposing (ActionResult(..))
import ChartJS exposing (ChartConfig)
import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.FormEngine.Model exposing (..)
import Common.Questionnaire.Models.Feedback exposing (Feedback)
import Common.Questionnaire.Models.FeedbackForm as FeedbackForm exposing (FeedbackForm)
import Common.Questionnaire.Models.SummaryReport exposing (ChapterReport, MetricReport, SummaryReport)
import Form
import KMEditor.Common.Events.Event exposing (Event)
import KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.Expert exposing (Expert)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import KMEditor.Common.KnowledgeModel.Question.CommonQuestionData exposing (CommonQuestionData)
import KMEditor.Common.KnowledgeModel.Question.ListQuestionData exposing (ListQuestionData)
import KMEditor.Common.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import KMEditor.Common.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import KMEditor.Common.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import List.Extra as List
import Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import String exposing (fromInt)
import Utils exposing (boolToInt)


type alias Model =
    { questionnaire : QuestionnaireDetail
    , events : List Event
    , activePage : ActivePage
    , feedback : ActionResult (List Feedback)
    , feedbackQuestionUuid : Maybe String
    , feedbackForm : Form.Form CustomFormError FeedbackForm
    , sendingFeedback : ActionResult String
    , feedbackResult : Maybe Feedback
    , metrics : List Metric
    , summaryReport : ActionResult SummaryReport
    , dirty : Bool
    }


type ActivePage
    = PageNone
    | PageChapter Chapter (Form Question Answer)
    | PageSummaryReport
    | PageTodos


type alias FormExtraData =
    { resourcePageReferences : List ResourcePageReferenceData
    , urlReferences : List URLReferenceData
    , experts : List Expert
    , requiredLevel : Maybe Int
    }


initialModel : AppState -> QuestionnaireDetail -> List Metric -> List Event -> Model
initialModel appState questionnaire metrics events =
    let
        activePage =
            case List.head (KnowledgeModel.getChapters questionnaire.knowledgeModel) of
                Just chapter ->
                    PageChapter chapter (createChapterForm appState questionnaire.knowledgeModel metrics questionnaire chapter)

                Nothing ->
                    PageNone
    in
    { questionnaire = questionnaire
    , events = events
    , activePage = activePage
    , feedback = Unset
    , feedbackQuestionUuid = Nothing
    , feedbackForm = FeedbackForm.initEmpty
    , sendingFeedback = Unset
    , feedbackResult = Nothing
    , metrics = metrics
    , summaryReport = Unset
    , dirty = False
    }



{- Form creation -}


createChapterForm : AppState -> KnowledgeModel -> List Metric -> QuestionnaireDetail -> Chapter -> Form Question Answer
createChapterForm appState km metrics questionnaire chapter =
    createForm
        { items = List.map (createQuestionFormItem appState km metrics) (KnowledgeModel.getChapterQuestions chapter.uuid km) }
        questionnaire.replies
        [ chapter.uuid ]


createQuestionFormItem : AppState -> KnowledgeModel -> List Metric -> Question -> FormItem Question Answer
createQuestionFormItem appState km metrics question =
    let
        descriptor =
            createFormItemDescriptor question
    in
    case question of
        OptionsQuestion commonData _ ->
            ChoiceFormItem descriptor <| List.map (createAnswerOption appState km metrics) (KnowledgeModel.getQuestionAnswers commonData.uuid km)

        ListQuestion commonData _ ->
            GroupFormItem descriptor (createGroupItems appState km metrics commonData)

        ValueQuestion _ questionData ->
            case questionData.valueType of
                NumberQuestionValueType ->
                    NumberFormItem descriptor

                TextQuestionValueType ->
                    TextFormItem descriptor

                _ ->
                    StringFormItem descriptor

        IntegrationQuestion _ questionData ->
            List.find (.uuid >> (==) questionData.integrationUuid) (KnowledgeModel.getIntegrations km)
                |> Maybe.map (\i -> TypeHintFormItem descriptor { logo = i.logo, url = i.itemUrl })
                |> Maybe.withDefault (TextFormItem descriptor)


createFormItemDescriptor : Question -> FormItemDescriptor Question
createFormItemDescriptor question =
    { name = Question.getUuid question
    , question = question
    }


createAnswerOption : AppState -> KnowledgeModel -> List Metric -> Answer -> Option Question Answer
createAnswerOption appState km metrics answer =
    let
        descriptor =
            createOptionFormDescriptor answer
    in
    case KnowledgeModel.getAnswerFollowupQuestions answer.uuid km of
        [] ->
            SimpleOption descriptor

        followUps ->
            DetailedOption descriptor (List.map (createQuestionFormItem appState km metrics) followUps)


createOptionFormDescriptor : Answer -> OptionDescriptor Answer
createOptionFormDescriptor answer =
    { name = answer.uuid
    , option = answer
    }


createGroupItems : AppState -> KnowledgeModel -> List Metric -> CommonQuestionData -> List (FormItem Question Answer)
createGroupItems appState km metrics commonData =
    List.map (createQuestionFormItem appState km metrics) (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid km)



{- Form helpers -}


updateReplies : Model -> Model
updateReplies model =
    let
        replies =
            case model.activePage of
                PageChapter chapter form ->
                    getFormValues [ chapter.uuid ] form
                        ++ model.questionnaire.replies
                        |> List.uniqueBy .path
                        |> List.filter (.value >> isEmptyReply >> not)

                _ ->
                    model.questionnaire.replies
    in
    { model | questionnaire = QuestionnaireDetail.updateReplies replies model.questionnaire }


setActiveChapter : AppState -> Chapter -> Model -> Model
setActiveChapter appState chapter model =
    { model
        | activePage = PageChapter chapter (createChapterForm appState model.questionnaire.knowledgeModel model.metrics model.questionnaire chapter)
    }


getActiveChapter : Model -> Maybe Chapter
getActiveChapter model =
    case model.activePage of
        PageChapter chapter _ ->
            Just chapter

        _ ->
            Nothing


setLevel : QuestionnaireDetail -> Int -> QuestionnaireDetail
setLevel questionnaire level =
    { questionnaire | level = level }


addLabel : Model -> String -> Model
addLabel model path =
    let
        labels =
            [ { path = path, value = [ todoUuid ] } ]
                ++ model.questionnaire.labels
                |> List.uniqueBy .path
    in
    { model
        | questionnaire = QuestionnaireDetail.updateLabels labels model.questionnaire
        , dirty = True
    }


removeLabel : Model -> String -> Model
removeLabel model path =
    let
        labels =
            List.filter (not << (==) path << .path) model.questionnaire.labels
    in
    { model
        | questionnaire = QuestionnaireDetail.updateLabels labels model.questionnaire
        , dirty = True
    }


removeLabelsFromItem : Model -> List String -> Int -> Model
removeLabelsFromItem model path index =
    let
        activeChapterUuid =
            Maybe.withDefault "" <| Maybe.map .uuid <| getActiveChapter model

        fullPath =
            activeChapterUuid :: path

        pathString =
            String.join "." fullPath

        pathLength =
            List.length fullPath

        getIndex p =
            String.split "." p
                |> List.drop pathLength
                |> List.head
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault -1

        decrementIndex p =
            let
                parts =
                    String.split "." p
            in
            String.join "." <|
                List.take pathLength parts
                    ++ [ String.fromInt <| getIndex p - 1 ]
                    ++ List.drop (pathLength + 1) parts

        filter label =
            if String.startsWith pathString label.path then
                if getIndex label.path < index then
                    Just label

                else if getIndex label.path == index then
                    Nothing

                else
                    Just { label | path = decrementIndex label.path }

            else
                Just label

        labels =
            List.filterMap filter model.questionnaire.labels
    in
    { model
        | questionnaire = QuestionnaireDetail.updateLabels labels model.questionnaire
        , dirty = True
    }


todoUuid : String
todoUuid =
    "615b9028-5e3f-414f-b245-12d2ae2eeb20"



{- Indications calculations -}


calculateUnansweredQuestions : AppState -> KnowledgeModel -> Int -> FormValues -> Chapter -> Int
calculateUnansweredQuestions appState km currentLevel replies chapter =
    KnowledgeModel.getChapterQuestions chapter.uuid km
        |> List.map (evaluateQuestion appState km currentLevel replies [ chapter.uuid ])
        |> List.foldl (+) 0


getReply : FormValues -> String -> Maybe ReplyValue
getReply replies path =
    List.find (.path >> (==) path) replies
        |> Maybe.map .value


evaluateQuestion : AppState -> KnowledgeModel -> Int -> FormValues -> List String -> Question -> Int
evaluateQuestion appState km currentLevel replies path question =
    let
        currentPath =
            path ++ [ Question.getUuid question ]

        requiredNow =
            (Question.getRequiredLevel question |> Maybe.withDefault 100) <= currentLevel

        rawValue =
            getReply replies (String.join "." currentPath)

        adjustedValue =
            if Question.isList question then
                case rawValue of
                    Nothing ->
                        Just <| ItemListReply 0

                    _ ->
                        rawValue

            else
                rawValue
    in
    case adjustedValue of
        Just value ->
            case question of
                OptionsQuestion _ questionData ->
                    questionData.answerUuids
                        |> List.find ((==) (getAnswerUuid value))
                        |> Maybe.map (evaluateFollowups appState km currentLevel replies currentPath)
                        |> Maybe.withDefault 1

                ListQuestion commonData _ ->
                    let
                        itemCount =
                            getItemListCount value
                    in
                    if itemCount > 0 then
                        List.range 0 (itemCount - 1)
                            |> List.map (evaluateAnswerItem appState km currentLevel replies currentPath (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid km))
                            |> List.foldl (+) 0

                    else
                        boolToInt requiredNow

                _ ->
                    0

        Nothing ->
            if requiredNow then
                1

            else
                0


evaluateFollowups : AppState -> KnowledgeModel -> Int -> FormValues -> List String -> String -> Int
evaluateFollowups appState km currentLevel replies path answerUuid =
    let
        currentPath =
            path ++ [ answerUuid ]
    in
    KnowledgeModel.getAnswerFollowupQuestions answerUuid km
        |> List.map (evaluateQuestion appState km currentLevel replies currentPath)
        |> List.foldl (+) 0


evaluateAnswerItem : AppState -> KnowledgeModel -> Int -> FormValues -> List String -> List Question -> Int -> Int
evaluateAnswerItem appState km currentLevel replies path questions index =
    let
        currentPath =
            path ++ [ fromInt index ]
    in
    questions
        |> List.map (evaluateQuestion appState km currentLevel replies currentPath)
        |> List.foldl (+) 0


createChartConfig : List Metric -> List Chapter -> ChapterReport -> ChartConfig
createChartConfig metrics chapters chapterReport =
    let
        data =
            List.map (createDataValue metrics) chapterReport.metrics

        label =
            List.find (.uuid >> (==) chapterReport.chapterUuid) chapters
                |> Maybe.map .title
                |> Maybe.withDefault "Chapter"
    in
    { targetId = chapterReportCanvasId chapterReport
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


chapterReportCanvasId : ChapterReport -> String
chapterReportCanvasId chapterReport =
    "chapter-report-" ++ chapterReport.chapterUuid
