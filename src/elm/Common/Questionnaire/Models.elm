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
    }


questionnaireDetailDecoder : Decoder QuestionnaireDetail
questionnaireDetailDecoder =
    decode QuestionnaireDetail
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" packageDetailDecoder
        |> required "knowledgeModel" knowledgeModelDecoder
        |> required "replies" decodeFormValues


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
    , text = Just question.text
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
