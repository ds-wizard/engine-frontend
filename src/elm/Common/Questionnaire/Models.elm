module Common.Questionnaire.Models exposing (..)

import Common.Form exposing (CustomFormError)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport)
import Common.Types exposing (ActionResult(Unset))
import Form
import Form.Validate as Validate exposing (..)
import FormEngine.Model exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models.Entities exposing (..)
import KMPackages.Common.Models exposing (PackageDetail, packageDetailDecoder)
import List.Extra as List
import Utils exposing (stringToInt)


type alias Model =
    { questionnaire : QuestionnaireDetail
    , activePage : ActivePage
    , feedback : ActionResult (List Feedback)
    , feedbackQuestionUuid : Maybe String
    , feedbackForm : Form.Form CustomFormError FeedbackForm
    , sendingFeedback : ActionResult String
    , feedbackResult : Maybe Feedback
    , metrics : ActionResult (List Metric)
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


initialModel : QuestionnaireDetail -> Model
initialModel questionnaire =
    let
        activePage =
            case List.head questionnaire.knowledgeModel.chapters of
                Just chapter ->
                    PageChapter chapter (createChapterForm chapter questionnaire.replies)

                Nothing ->
                    PageNone
    in
    { questionnaire = questionnaire
    , activePage = activePage
    , feedback = Unset
    , feedbackQuestionUuid = Nothing
    , feedbackForm = initEmptyFeedbackFrom
    , sendingFeedback = Unset
    , feedbackResult = Nothing
    , metrics = Unset
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
    }


questionnaireDetailDecoder : Decoder QuestionnaireDetail
questionnaireDetailDecoder =
    decode QuestionnaireDetail
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" packageDetailDecoder
        |> required "knowledgeModel" knowledgeModelDecoder
        |> required "replies" decodeFormValues
        |> required "level" Decode.int


encodeQuestionnaireDetail : QuestionnaireDetail -> Encode.Value
encodeQuestionnaireDetail questionnaire =
    Encode.object
        [ ( "replies", encodeFormValues questionnaire.replies )
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
    decode Feedback
        |> required "title" Decode.string
        |> required "issueId" Decode.int
        |> required "issueUrl" Decode.string


feedbackListDecoder : Decoder (List Feedback)
feedbackListDecoder =
    Decode.list feedbackDecoder



{- Form creation -}


createChapterForm : Chapter -> FormValues -> Form FormExtraData
createChapterForm chapter values =
    createForm { items = List.map createQuestionFormItem chapter.questions } values [ chapter.uuid ]


createQuestionFormItem : Question -> FormItem FormExtraData
createQuestionFormItem question =
    let
        descriptor =
            createFormItemDescriptor question
    in
    case question.type_ of
        "options" ->
            ChoiceFormItem descriptor (List.map createAnswerOption (question.answers |> Maybe.withDefault []))

        "list" ->
            GroupFormItem descriptor (createGroupItems question)

        "number" ->
            NumberFormItem descriptor

        "text" ->
            TextFormItem descriptor

        _ ->
            StringFormItem descriptor


createFormItemDescriptor : Question -> FormItemDescriptor FormExtraData
createFormItemDescriptor question =
    { name = question.uuid
    , label = question.title
    , text = question.text
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

        extraData =
            { resourcePageReferences = []
            , urlReferences = []
            , experts = question.experts
            , requiredLevel = question.requiredLevel
            }
    in
    Just <| List.foldl foldReferences extraData question.references


createAnswerOption : Answer -> Option FormExtraData
createAnswerOption answer =
    let
        descriptor =
            createOptionFormDescriptor answer
    in
    case answer.followUps of
        FollowUps [] ->
            SimpleOption descriptor

        FollowUps followUps ->
            DetailedOption descriptor (List.map createQuestionFormItem followUps)


createOptionFormDescriptor : Answer -> OptionDescriptor
createOptionFormDescriptor answer =
    { name = answer.uuid
    , label = answer.label
    , text = answer.advice
    }


createGroupItems : Question -> List (FormItem FormExtraData)
createGroupItems question =
    case question.answerItemTemplate of
        Just answerItemTemplate ->
            let
                itemName =
                    StringFormItem { name = "itemName", label = answerItemTemplate.title, text = Nothing, extraData = Nothing }

                questions =
                    List.map createQuestionFormItem <| getQuestions answerItemTemplate.questions
            in
            itemName :: questions

        _ ->
            []


getQuestions : AnswerItemTemplateQuestions -> List Question
getQuestions (AnswerItemTemplateQuestions questions) =
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
                        |> List.filter (.value >> (/=) "")

                _ ->
                    model.questionnaire.replies
    in
    { model | questionnaire = updateQuestionnaireReplies replies model.questionnaire }


updateQuestionnaireReplies : FormValues -> QuestionnaireDetail -> QuestionnaireDetail
updateQuestionnaireReplies replies questionnaire =
    { questionnaire | replies = replies }


setActiveChapter : Chapter -> Model -> Model
setActiveChapter chapter model =
    { model
        | activePage = PageChapter chapter (createChapterForm chapter model.questionnaire.replies)
    }


setLevel : QuestionnaireDetail -> Int -> QuestionnaireDetail
setLevel questionnaire level =
    { questionnaire | level = level }



{- Indications calculations -}


calculateUnansweredQuestions : Int -> FormValues -> Chapter -> Int
calculateUnansweredQuestions currentLevel replies chapter =
    chapter.questions
        |> List.map (evaluateQuestion currentLevel replies [ chapter.uuid ])
        |> List.foldl (+) 0


getReply : FormValues -> String -> Maybe String
getReply replies path =
    List.find (.path >> (==) path) replies
        |> Maybe.map .value


evaluateQuestion : Int -> FormValues -> List String -> Question -> Int
evaluateQuestion currentLevel replies path question =
    let
        currentPath =
            path ++ [ question.uuid ]

        requiredNow =
            (question.requiredLevel |> Maybe.withDefault 100) <= currentLevel

        rawValue =
            getReply replies (String.join "." currentPath)

        adjustedValue =
            if question.type_ == "list" then
                case rawValue of
                    Nothing ->
                        Just "1"

                    _ ->
                        rawValue
            else
                rawValue
    in
    case adjustedValue of
        Just value ->
            case question.type_ of
                "options" ->
                    question.answers
                        |> Maybe.withDefault []
                        |> List.find (.uuid >> (==) value)
                        |> Maybe.map (evaluateFollowups currentLevel replies currentPath)
                        |> Maybe.withDefault 1

                "list" ->
                    let
                        questions =
                            getAnswerItemTemplateQuestions question

                        itemCount =
                            stringToInt value
                    in
                    List.range 0 (itemCount - 1)
                        |> List.map (evaluateAnswerItem currentLevel replies currentPath questions)
                        |> List.foldl (+) 0

                _ ->
                    0

        Nothing ->
            if requiredNow then
                1
            else
                0


evaluateFollowups : Int -> FormValues -> List String -> Answer -> Int
evaluateFollowups currentLevel replies path answer =
    let
        currentPath =
            path ++ [ answer.uuid ]
    in
    getFollowUpQuestions answer
        |> List.map (evaluateQuestion currentLevel replies currentPath)
        |> List.foldl (+) 0


evaluateAnswerItem : Int -> FormValues -> List String -> List Question -> Int -> Int
evaluateAnswerItem currentLevel replies path questions index =
    let
        currentPath =
            path ++ [ toString index ]
    in
    questions
        |> List.map (evaluateQuestion currentLevel replies currentPath)
        |> List.foldl (+) 0
