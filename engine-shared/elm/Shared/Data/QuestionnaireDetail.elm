module Shared.Data.QuestionnaireDetail exposing
    ( QuestionnaireDetail
    , calculateUnansweredQuestionsForChapter
    , clearReplyValue
    , decoder
    , encode
    , getTodos
    , hasReply
    , isEditable
    , isOwner
    , setLabels
    , setLevel
    , setReplyValue
    , todoUuid
    , todosLength
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireDetail.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)
import Shared.Utils exposing (boolToInt)
import Uuid exposing (Uuid)


type alias QuestionnaireDetail =
    { uuid : Uuid
    , name : String
    , package : Package
    , knowledgeModel : KnowledgeModel
    , replies : Dict String ReplyValue
    , level : Int
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    , permissions : List Permission
    , selectedTagUuids : List String
    , templateId : Maybe String
    , template : Maybe TemplateSuggestion
    , formatUuid : Maybe Uuid
    , format : Maybe TemplateFormat
    , labels : Dict String (List String)
    }


decoder : Decoder QuestionnaireDetail
decoder =
    D.succeed QuestionnaireDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "package" Package.decoder
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "replies" (D.dict ReplyValue.decoder)
        |> D.required "level" D.int
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "selectedTagUuids" (D.list D.string)
        |> D.required "templateId" (D.maybe D.string)
        |> D.required "template" (D.maybe TemplateSuggestion.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "format" (D.maybe TemplateFormat.decoder)
        |> D.required "labels" (D.dict (D.list D.string))


encode : QuestionnaireDetail -> E.Value
encode questionnaire =
    E.object
        [ ( "replies", E.dict identity ReplyValue.encode questionnaire.replies )
        , ( "level", E.int questionnaire.level )
        , ( "labels", E.dict identity (E.list E.string) questionnaire.labels )
        ]


isEditable : AbstractAppState a -> QuestionnaireDetail -> Bool
isEditable appState questionnaire =
    let
        owner =
            isOwner appState questionnaire

        isReadonly =
            if questionnaire.sharing == AnyoneWithLinkEditQuestionnaire then
                False

            else if Session.exists appState.session then
                questionnaire.visibility == VisibleViewQuestionnaire || (questionnaire.visibility == PrivateQuestionnaire && not owner)

            else
                questionnaire.sharing == AnyoneWithLinkViewQuestionnaire
    in
    owner || not isReadonly


isOwner : AbstractAppState a -> QuestionnaireDetail -> Bool
isOwner appState questionnaire =
    let
        admin =
            UserInfo.isAdmin appState.session.user

        owner =
            matchOwner questionnaire appState.session.user
    in
    admin || owner


matchOwner : QuestionnaireDetail -> Maybe UserInfo -> Bool
matchOwner questionnaire mbUser =
    List.any (.member >> .uuid >> Just >> (==) (Maybe.map .uuid mbUser)) questionnaire.permissions


setLevel : Int -> QuestionnaireDetail -> QuestionnaireDetail
setLevel level questionnaire =
    { questionnaire | level = level }


setReplyValue : String -> ReplyValue -> QuestionnaireDetail -> QuestionnaireDetail
setReplyValue path replyValue questionnaire =
    { questionnaire | replies = Dict.insert path replyValue questionnaire.replies }


clearReplyValue : String -> QuestionnaireDetail -> QuestionnaireDetail
clearReplyValue path questionnaire =
    { questionnaire | replies = Dict.remove path questionnaire.replies }


setLabels : String -> List String -> QuestionnaireDetail -> QuestionnaireDetail
setLabels path labels questionnaire =
    { questionnaire | labels = Dict.insert path labels questionnaire.labels }


todosLength : QuestionnaireDetail -> Int
todosLength =
    List.length << getTodos


getTodos : QuestionnaireDetail -> List QuestionnaireTodo
getTodos questionnaire =
    List.concatMap
        (getChapterTodos questionnaire)
        (KnowledgeModel.getChapters questionnaire.knowledgeModel)


getChapterTodos : QuestionnaireDetail -> Chapter -> List QuestionnaireTodo
getChapterTodos questionnaire chapter =
    List.concatMap
        (getQuestionTodos questionnaire chapter [ chapter.uuid ])
        (KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel)


getQuestionTodos : QuestionnaireDetail -> Chapter -> List String -> Question -> List QuestionnaireTodo
getQuestionTodos questionnaire chapter path question =
    let
        km =
            questionnaire.knowledgeModel

        currentPath =
            path ++ [ Question.getUuid question ]

        questionTodo =
            if hasTodo questionnaire (pathToString currentPath) then
                [ { chapter = chapter
                  , question = question
                  , path = pathToString currentPath
                  }
                ]

            else
                []

        childTodos =
            case getReply questionnaire (pathToString currentPath) of
                Just replyValue ->
                    case question of
                        OptionsQuestion commonData _ ->
                            case List.find (.uuid >> (==) (ReplyValue.getAnswerUuid replyValue)) (KnowledgeModel.getQuestionAnswers commonData.uuid km) of
                                Just answer ->
                                    List.concatMap
                                        (getQuestionTodos questionnaire chapter (currentPath ++ [ answer.uuid ]))
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km)

                                Nothing ->
                                    []

                        ListQuestion commonData _ ->
                            let
                                getItemQuestionTodos itemUuid =
                                    List.concatMap
                                        (getQuestionTodos questionnaire chapter (currentPath ++ [ itemUuid ]))
                                        (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid km)
                            in
                            List.concatMap getItemQuestionTodos (ReplyValue.getItemUuids replyValue)

                        _ ->
                            []

                Nothing ->
                    []
    in
    questionTodo ++ childTodos


getReply : QuestionnaireDetail -> String -> Maybe ReplyValue
getReply questionnaire path =
    Dict.get path questionnaire.replies


hasTodo : QuestionnaireDetail -> String -> Bool
hasTodo questionnaire path =
    Maybe.unwrap False (List.member todoUuid) (Dict.get path questionnaire.labels)


pathToString : List String -> String
pathToString =
    String.join "."


todoUuid : String
todoUuid =
    "615b9028-5e3f-414f-b245-12d2ae2eeb20"


hasReply : String -> QuestionnaireDetail -> Bool
hasReply path questionnaire =
    Maybe.unwrap False (not << ReplyValue.isEmpty) (getReply questionnaire path)



-- Evaluations


calculateUnansweredQuestionsForChapter : QuestionnaireDetail -> Int -> Chapter -> Int
calculateUnansweredQuestionsForChapter questionnaire currentLevel chapter =
    KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire currentLevel [ chapter.uuid ])
        |> List.foldl (+) 0


evaluateQuestion : QuestionnaireDetail -> Int -> List String -> Question -> Int
evaluateQuestion questionnaire currentLevel path question =
    let
        currentPath =
            path ++ [ Question.getUuid question ]

        requiredNow =
            (Question.getRequiredLevel question |> Maybe.withDefault 100) <= currentLevel

        rawValue =
            Dict.get (pathToString currentPath) questionnaire.replies

        adjustedValue =
            if Question.isList question then
                case rawValue of
                    Nothing ->
                        Just <| ItemListReply []

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
                        |> List.find ((==) (ReplyValue.getAnswerUuid value))
                        |> Maybe.map (evaluateFollowups questionnaire currentLevel currentPath)
                        |> Maybe.withDefault 1

                ListQuestion commonData _ ->
                    let
                        itemUuids =
                            ReplyValue.getItemUuids value
                    in
                    if not (List.isEmpty itemUuids) then
                        itemUuids
                            |> List.map (evaluateAnswerItem questionnaire currentLevel currentPath (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid questionnaire.knowledgeModel))
                            |> List.foldl (+) 0

                    else
                        boolToInt requiredNow

                _ ->
                    if ReplyValue.isEmpty value then
                        boolToInt requiredNow

                    else
                        0

        Nothing ->
            boolToInt requiredNow


evaluateFollowups : QuestionnaireDetail -> Int -> List String -> String -> Int
evaluateFollowups questionnaire currentLevel path answerUuid =
    let
        currentPath =
            path ++ [ answerUuid ]
    in
    KnowledgeModel.getAnswerFollowupQuestions answerUuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire currentLevel currentPath)
        |> List.foldl (+) 0


evaluateAnswerItem : QuestionnaireDetail -> Int -> List String -> List Question -> String -> Int
evaluateAnswerItem questionnaire currentLevel path questions uuid =
    let
        currentPath =
            path ++ [ uuid ]
    in
    questions
        |> List.map (evaluateQuestion questionnaire currentLevel currentPath)
        |> List.foldl (+) 0
