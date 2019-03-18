module Common.Questionnaire.Models exposing
    ( ActivePage(..)
    , Feedback
    , FeedbackForm
    , FormExtraData
    , Model
    , QuestionnaireDetail
    , calculateUnansweredQuestions
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
import Common.Form exposing (CustomFormError)
import Common.Questionnaire.Models.SummaryReport exposing (SummaryReport)
import Form
import Form.Validate as Validate exposing (..)
import FormEngine.Model exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models.Entities exposing (..)
import KnowledgeModels.Common.Models exposing (PackageDetail, packageDetailDecoder)
import List.Extra as List
import String exposing (fromInt)
import Utils exposing (boolToInt)


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
    , private : Bool
    }


questionnaireDetailDecoder : Decoder QuestionnaireDetail
questionnaireDetailDecoder =
    Decode.succeed QuestionnaireDetail
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" packageDetailDecoder
        |> required "knowledgeModel" knowledgeModelDecoder
        |> required "replies" decodeFormValues
        |> required "level" Decode.int
        |> required "private" Decode.bool


encodeQuestionnaireDetail : QuestionnaireDetail -> Encode.Value
encodeQuestionnaireDetail questionnaire =
    Encode.object
        [ ( "name", Encode.string questionnaire.name )
        , ( "private", Encode.bool questionnaire.private )
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


createChapterForm : Chapter -> FormValues -> Form FormExtraData
createChapterForm chapter values =
    createForm { items = List.map createQuestionFormItem chapter.questions } values [ chapter.uuid ]


createQuestionFormItem : Question -> FormItem FormExtraData
createQuestionFormItem question =
    let
        descriptor =
            createFormItemDescriptor question
    in
    case question of
        OptionsQuestion data ->
            ChoiceFormItem descriptor (List.map createAnswerOption data.answers)

        ListQuestion data ->
            GroupFormItem descriptor (createGroupItems data)

        ValueQuestion data ->
            case data.valueType of
                NumberValueType ->
                    NumberFormItem descriptor

                TextValueType ->
                    TextFormItem descriptor

                _ ->
                    StringFormItem descriptor


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


createGroupItems : ListQuestionData -> List (FormItem FormExtraData)
createGroupItems questionData =
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
            List.map createQuestionFormItem questionData.itemTemplateQuestions
    in
    itemName :: questions



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


getReply : FormValues -> String -> Maybe ReplyValue
getReply replies path =
    List.find (.path >> (==) path) replies
        |> Maybe.map .value


evaluateQuestion : Int -> FormValues -> List String -> Question -> Int
evaluateQuestion currentLevel replies path question =
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
                        |> Maybe.map (evaluateFollowups currentLevel replies currentPath)
                        |> Maybe.withDefault 1

                ListQuestion data ->
                    let
                        itemCount =
                            getItemListCount value
                    in
                    if itemCount > 0 then
                        List.range 0 (itemCount - 1)
                            |> List.map (evaluateAnswerItem currentLevel replies currentPath requiredNow data.itemTemplateQuestions)
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


evaluateFollowups : Int -> FormValues -> List String -> Answer -> Int
evaluateFollowups currentLevel replies path answer =
    let
        currentPath =
            path ++ [ answer.uuid ]
    in
    getFollowUpQuestions answer
        |> List.map (evaluateQuestion currentLevel replies currentPath)
        |> List.foldl (+) 0


evaluateAnswerItem : Int -> FormValues -> List String -> Bool -> List Question -> Int -> Int
evaluateAnswerItem currentLevel replies path requiredNow questions index =
    let
        currentPath =
            path ++ [ fromInt index ]

        answerItem =
            if requiredNow then
                getReply replies (String.join "." <| currentPath ++ [ "itemName" ])
                    |> Maybe.map isEmptyReply
                    |> Maybe.withDefault True
                    |> boolToInt

            else
                0
    in
    questions
        |> List.map (evaluateQuestion currentLevel replies currentPath)
        |> List.foldl (+) 0
        |> (+) answerItem
