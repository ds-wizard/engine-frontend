module Common.Questionnaire.Models exposing
    ( ActivePage(..)
    , Feedback
    , FeedbackForm
    , FormExtraData
    , Model
    , QuestionnaireDetail
    , calculateUnansweredQuestions
    , chapterReportCanvasId
    , createChartConfig
    , encodeFeedbackFrom
    , encodeQuestionnaireDetail
    , feedbackDecoder
    , feedbackFormValidation
    , feedbackListDecoder
    , initEmptyFeedbackFrom
    , initialModel
    , questionnaireDetailDecoder
    , setActiveChapter
    , setLevel
    , updateReplies
    )

import ActionResult exposing (ActionResult(..))
import ChartJS exposing (ChartConfig)
import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Questionnaire.Models.SummaryReport exposing (ChapterReport, MetricReport, SummaryReport)
import Form
import Form.Validate as Validate exposing (..)
import FormEngine.Model exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (Event)
import KnowledgeModels.Common.PackageDetail as PackageDetail exposing (PackageDetail)
import List.Extra as List
import Questionnaires.Common.Models.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
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
    | PageChapter Chapter (Form FormExtraData)
    | PageSummaryReport


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
            case List.head questionnaire.knowledgeModel.chapters of
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
    , feedbackForm = initEmptyFeedbackFrom
    , sendingFeedback = Unset
    , feedbackResult = Nothing
    , metrics = metrics
    , summaryReport = Unset
    , dirty = False
    }


type alias QuestionnaireDetail =
    { uuid : String
    , name : String
    , package : PackageDetail
    , knowledgeModel : KnowledgeModel
    , replies : FormValues
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , ownerUuid : Maybe String
    }


questionnaireDetailDecoder : Decoder QuestionnaireDetail
questionnaireDetailDecoder =
    Decode.succeed QuestionnaireDetail
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" PackageDetail.decoder
        |> required "knowledgeModel" knowledgeModelDecoder
        |> required "replies" decodeFormValues
        |> required "level" Decode.int
        |> required "accessibility" QuestionnaireAccessibility.decoder
        |> required "ownerUuid" (Decode.maybe Decode.string)


encodeQuestionnaireDetail : QuestionnaireDetail -> Encode.Value
encodeQuestionnaireDetail questionnaire =
    Encode.object
        [ ( "name", Encode.string questionnaire.name )
        , ( "accessibility", QuestionnaireAccessibility.encode questionnaire.accessibility )
        , ( "replies", encodeFormValues questionnaire.replies )
        , ( "level", Encode.int questionnaire.level )
        ]


type alias FeedbackForm =
    { title : String
    , content : String
    }


initEmptyFeedbackFrom : Form.Form CustomFormError FeedbackForm
initEmptyFeedbackFrom =
    Form.initial [] feedbackFormValidation


feedbackFormValidation : Validation CustomFormError FeedbackForm
feedbackFormValidation =
    Validate.map2 FeedbackForm
        (Validate.field "title" Validate.string)
        (Validate.field "content" Validate.string)


encodeFeedbackFrom : String -> String -> FeedbackForm -> Encode.Value
encodeFeedbackFrom questionUuid packageId form =
    Encode.object
        [ ( "questionUuid", Encode.string questionUuid )
        , ( "packageId", Encode.string packageId )
        , ( "title", Encode.string form.title )
        , ( "content", Encode.string form.content )
        ]


type alias Feedback =
    { title : String
    , issueId : Int
    , issueUrl : String
    }


feedbackDecoder : Decoder Feedback
feedbackDecoder =
    Decode.succeed Feedback
        |> required "title" Decode.string
        |> required "issueId" Decode.int
        |> required "issueUrl" Decode.string


feedbackListDecoder : Decoder (List Feedback)
feedbackListDecoder =
    Decode.list feedbackDecoder



{- Form creation -}


createChapterForm : AppState -> KnowledgeModel -> List Metric -> QuestionnaireDetail -> Chapter -> Form FormExtraData
createChapterForm appState km metrics questionnaire chapter =
    createForm { items = List.map (createQuestionFormItem appState km metrics) chapter.questions } questionnaire.replies [ chapter.uuid ]


createQuestionFormItem : AppState -> KnowledgeModel -> List Metric -> Question -> FormItem FormExtraData
createQuestionFormItem appState km metrics question =
    let
        descriptor =
            createFormItemDescriptor question
    in
    case question of
        OptionsQuestion data ->
            ChoiceFormItem descriptor (List.map (createAnswerOption appState km metrics) data.answers)

        ListQuestion data ->
            GroupFormItem descriptor (createGroupItems appState km metrics data)

        ValueQuestion data ->
            case data.valueType of
                NumberValueType ->
                    NumberFormItem descriptor

                TextValueType ->
                    TextFormItem descriptor

                _ ->
                    StringFormItem descriptor

        IntegrationQuestion data ->
            List.find (.uuid >> (==) data.integrationUuid) km.integrations
                |> Maybe.map (\i -> TypeHintFormItem descriptor { logo = i.logo, url = i.itemUrl })
                |> Maybe.withDefault (TextFormItem descriptor)


createFormItemDescriptor : Question -> FormItemDescriptor FormExtraData
createFormItemDescriptor question =
    { name = getQuestionUuid question
    , label = getQuestionTitle question
    , text = getQuestionText question
    , extraData = createQuestionExtraData question
    }


createQuestionExtraData : Question -> Maybe FormExtraData
createQuestionExtraData question =
    let
        foldReferences reference extraData =
            case reference of
                ResourcePageReference data ->
                    { extraData | resourcePageReferences = extraData.resourcePageReferences ++ [ data ] }

                URLReference data ->
                    { extraData | urlReferences = extraData.urlReferences ++ [ data ] }

                _ ->
                    extraData

        newExtraData =
            { resourcePageReferences = []
            , urlReferences = []
            , experts = getQuestionExperts question
            , requiredLevel = getQuestionRequiredLevel question
            }
    in
    Just <| List.foldl foldReferences newExtraData <| getQuestionReferences question


createAnswerOption : AppState -> KnowledgeModel -> List Metric -> Answer -> Option FormExtraData
createAnswerOption appState km metrics answer =
    let
        descriptor =
            createOptionFormDescriptor metrics answer
    in
    case answer.followUps of
        FollowUps [] ->
            SimpleOption descriptor

        FollowUps followUps ->
            DetailedOption descriptor (List.map (createQuestionFormItem appState km metrics) followUps)


createOptionFormDescriptor : List Metric -> Answer -> OptionDescriptor
createOptionFormDescriptor metrics answer =
    { name = answer.uuid
    , label = answer.label
    , text = answer.advice
    , badges = createBadges metrics answer
    }


createBadges : List Metric -> Answer -> Maybe (List ( String, String ))
createBadges metrics answer =
    let
        getMetricName uuid =
            List.find ((==) uuid << .uuid) metrics
                |> Maybe.map .title
                |> Maybe.withDefault "Unknown"

        getBadgeClass value =
            (++) "badge-value-" <| String.fromInt <| (*) 10 <| round <| value * 10

        createBadge metricMeasure =
            ( getBadgeClass metricMeasure.measure, getMetricName metricMeasure.metricUuid )

        metricExists measure =
            List.find ((==) measure.metricUuid << .uuid) metrics /= Nothing
    in
    if List.isEmpty answer.metricMeasures then
        Nothing

    else
        List.filter metricExists answer.metricMeasures
            |> List.map createBadge
            |> Just


createGroupItems : AppState -> KnowledgeModel -> List Metric -> ListQuestionData -> List (FormItem FormExtraData)
createGroupItems appState km metrics questionData =
    let
        itemNameExtraData =
            { resourcePageReferences = []
            , urlReferences = []
            , experts = []
            , requiredLevel = questionData.requiredLevel
            }

        itemName =
            StringFormItem
                { name = "itemName"
                , label = questionData.itemTemplateTitle
                , text = Nothing
                , extraData = Just itemNameExtraData
                }

        questions =
            List.map (createQuestionFormItem appState km metrics) questionData.itemTemplateQuestions
    in
    if appState.config.itemTitleEnabled then
        itemName :: questions

    else
        questions



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
    { model | questionnaire = updateQuestionnaireReplies replies model.questionnaire }


updateQuestionnaireReplies : FormValues -> QuestionnaireDetail -> QuestionnaireDetail
updateQuestionnaireReplies replies questionnaire =
    { questionnaire | replies = replies }


setActiveChapter : AppState -> Chapter -> Model -> Model
setActiveChapter appState chapter model =
    { model
        | activePage = PageChapter chapter (createChapterForm appState model.questionnaire.knowledgeModel model.metrics model.questionnaire chapter)
    }


setLevel : QuestionnaireDetail -> Int -> QuestionnaireDetail
setLevel questionnaire level =
    { questionnaire | level = level }



{- Indications calculations -}


calculateUnansweredQuestions : AppState -> Int -> FormValues -> Chapter -> Int
calculateUnansweredQuestions appState currentLevel replies chapter =
    chapter.questions
        |> List.map (evaluateQuestion appState currentLevel replies [ chapter.uuid ])
        |> List.foldl (+) 0


getReply : FormValues -> String -> Maybe ReplyValue
getReply replies path =
    List.find (.path >> (==) path) replies
        |> Maybe.map .value


evaluateQuestion : AppState -> Int -> FormValues -> List String -> Question -> Int
evaluateQuestion appState currentLevel replies path question =
    let
        currentPath =
            path ++ [ getQuestionUuid question ]

        requiredNow =
            (getQuestionRequiredLevel question |> Maybe.withDefault 100) <= currentLevel

        rawValue =
            getReply replies (String.join "." currentPath)

        adjustedValue =
            if isQuestionList question then
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
                OptionsQuestion data ->
                    data.answers
                        |> List.find (.uuid >> (==) (getAnswerUuid value))
                        |> Maybe.map (evaluateFollowups appState currentLevel replies currentPath)
                        |> Maybe.withDefault 1

                ListQuestion data ->
                    let
                        itemCount =
                            getItemListCount value
                    in
                    if itemCount > 0 then
                        List.range 0 (itemCount - 1)
                            |> List.map (evaluateAnswerItem appState currentLevel replies currentPath requiredNow data.itemTemplateQuestions)
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


evaluateFollowups : AppState -> Int -> FormValues -> List String -> Answer -> Int
evaluateFollowups appState currentLevel replies path answer =
    let
        currentPath =
            path ++ [ answer.uuid ]
    in
    getFollowUpQuestions answer
        |> List.map (evaluateQuestion appState currentLevel replies currentPath)
        |> List.foldl (+) 0


evaluateAnswerItem : AppState -> Int -> FormValues -> List String -> Bool -> List Question -> Int -> Int
evaluateAnswerItem appState currentLevel replies path requiredNow questions index =
    let
        currentPath =
            path ++ [ fromInt index ]

        answerItem =
            if requiredNow && appState.config.itemTitleEnabled then
                getReply replies (String.join "." <| currentPath ++ [ "itemName" ])
                    |> Maybe.map isEmptyReply
                    |> Maybe.withDefault True
                    |> boolToInt

            else
                0
    in
    questions
        |> List.map (evaluateQuestion appState currentLevel replies currentPath)
        |> List.foldl (+) 0
        |> (+) answerItem


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
