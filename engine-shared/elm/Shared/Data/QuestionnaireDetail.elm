module Shared.Data.QuestionnaireDetail exposing
    ( QuestionnaireDetail
    , calculateUnansweredQuestionsForChapter
    , clearReplyValue
    , createQuestionnaireDetail
    , decoder
    , encode
    , getTodos
    , getVersionByEventUuid
    , hasReply
    , isAnonymousProject
    , isCurrentVersion
    , isEditor
    , isOwner
    , isVersion
    , lastVisibleEvent
    , setLabels
    , setPhaseUuid
    , setReply
    , todoUuid
    , todosLength
    , updateContent
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
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
import Shared.Data.QuestionnaireContent exposing (QuestionnaireContent)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.Reply as Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnairePerm as QuestionnairePerm
import Shared.Data.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Data.UserInfo as UserInfo
import Shared.Utils exposing (boolToInt)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)


type alias QuestionnaireDetail =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , isTemplate : Bool
    , package : Package
    , knowledgeModel : KnowledgeModel
    , replies : Dict String Reply
    , phaseUuid : Maybe Uuid
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    , permissions : List Permission
    , selectedTagUuids : List String
    , templateId : Maybe String
    , template : Maybe TemplateSuggestion
    , formatUuid : Maybe Uuid
    , format : Maybe TemplateFormat
    , labels : Dict String (List String)
    , events : List QuestionnaireEvent
    , versions : List QuestionnaireVersion
    }


decoder : Decoder QuestionnaireDetail
decoder =
    D.succeed QuestionnaireDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "package" Package.decoder
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "selectedTagUuids" (D.list D.string)
        |> D.required "templateId" (D.maybe D.string)
        |> D.required "template" (D.maybe TemplateSuggestion.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "format" (D.maybe TemplateFormat.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "events" (D.list QuestionnaireEvent.decoder)
        |> D.required "versions" (D.list QuestionnaireVersion.decoder)


encode : QuestionnaireDetail -> E.Value
encode questionnaire =
    E.object
        [ ( "phaseUuid", E.maybe Uuid.encode questionnaire.phaseUuid )
        , ( "labels", E.dict identity (E.list E.string) questionnaire.labels )
        ]


isEditor : AbstractAppState a -> QuestionnaireDetail -> Bool
isEditor appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.edit


isOwner : AbstractAppState a -> QuestionnaireDetail -> Bool
isOwner appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.admin


isAnonymousProject : QuestionnaireDetail -> Bool
isAnonymousProject questionnaire =
    List.isEmpty questionnaire.permissions


createQuestionnaireDetail : Package -> KnowledgeModel -> QuestionnaireDetail
createQuestionnaireDetail package km =
    { uuid = Uuid.nil
    , name = ""
    , description = Nothing
    , isTemplate = False
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , permissions = []
    , package = package
    , knowledgeModel = km
    , replies = Dict.fromList []
    , phaseUuid = Maybe.andThen Uuid.fromString (List.head km.phaseUuids)
    , selectedTagUuids = []
    , templateId = Nothing
    , template = Nothing
    , formatUuid = Nothing
    , format = Nothing
    , labels = Dict.fromList []
    , events = []
    , versions = []
    }


hasPerm : AbstractAppState a -> QuestionnaireDetail -> String -> Bool
hasPerm appState questionnaire role =
    let
        mbUser =
            appState.session.user

        isAuthenticated =
            Session.exists appState.session

        globalPerms =
            if UserInfo.isAdmin mbUser then
                QuestionnairePerm.all

            else
                []

        visibilityPerms =
            if isAuthenticated then
                case questionnaire.visibility of
                    VisibleEditQuestionnaire ->
                        [ QuestionnairePerm.view, QuestionnairePerm.edit ]

                    VisibleViewQuestionnaire ->
                        [ QuestionnairePerm.view ]

                    PrivateQuestionnaire ->
                        []

            else
                []

        sharingPerms =
            case questionnaire.sharing of
                AnyoneWithLinkEditQuestionnaire ->
                    [ QuestionnairePerm.view, QuestionnairePerm.edit ]

                AnyoneWithLinkViewQuestionnaire ->
                    [ QuestionnairePerm.view ]

                RestrictedQuestionnaire ->
                    []

        userPerms =
            mbUser
                |> Maybe.andThen (\u -> List.find (.member >> .uuid >> (==) u.uuid) questionnaire.permissions)
                |> Maybe.unwrap [] .perms

        appliedPerms =
            globalPerms ++ visibilityPerms ++ sharingPerms ++ userPerms
    in
    List.member role appliedPerms


setPhaseUuid : Maybe Uuid -> QuestionnaireDetail -> QuestionnaireDetail
setPhaseUuid phaseUuid questionnaire =
    { questionnaire | phaseUuid = phaseUuid }


setReply : String -> Reply -> QuestionnaireDetail -> QuestionnaireDetail
setReply path reply questionnaire =
    { questionnaire | replies = Dict.insert path reply questionnaire.replies }


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
            case getReplyValue questionnaire (pathToString currentPath) of
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


getReplyValue : QuestionnaireDetail -> String -> Maybe ReplyValue
getReplyValue questionnaire path =
    Maybe.map .value <|
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
    Maybe.unwrap False (not << ReplyValue.isEmpty) (getReplyValue questionnaire path)


getVersionByEventUuid : { q | versions : List QuestionnaireVersion } -> Uuid -> Maybe QuestionnaireVersion
getVersionByEventUuid questionnaire eventUuid =
    List.find (.eventUuid >> (==) eventUuid) questionnaire.versions


lastVisibleEvent : QuestionnaireDetail -> Maybe QuestionnaireEvent
lastVisibleEvent =
    .events
        >> List.reverse
        >> List.dropWhile QuestionnaireEvent.isInvisible
        >> List.head


isCurrentVersion : QuestionnaireDetail -> Uuid -> Bool
isCurrentVersion questionnaire eventUuid =
    Maybe.map QuestionnaireEvent.getUuid (lastVisibleEvent questionnaire) == Just eventUuid


isVersion : QuestionnaireDetail -> QuestionnaireEvent -> Bool
isVersion questionnaire event =
    List.any (.eventUuid >> (==) (QuestionnaireEvent.getUuid event)) questionnaire.versions



-- Evaluations


calculateUnansweredQuestionsForChapter : AppState -> QuestionnaireDetail -> Chapter -> Int
calculateUnansweredQuestionsForChapter appState questionnaire chapter =
    KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion appState questionnaire [ chapter.uuid ])
        |> List.foldl (+) 0


evaluateQuestion : AppState -> QuestionnaireDetail -> List String -> Question -> Int
evaluateQuestion appState questionnaire path question =
    let
        currentPath =
            path ++ [ Question.getUuid question ]

        currentPhase =
            Maybe.withDefault Uuid.nil questionnaire.phaseUuid

        requiredNow =
            Question.isDesirable questionnaire.knowledgeModel.phaseUuids (Uuid.toString currentPhase) question

        rawValue =
            getReplyValue questionnaire (pathToString currentPath)

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
                        |> Maybe.map (evaluateFollowups appState questionnaire currentPath)
                        |> Maybe.withDefault 1

                ListQuestion commonData _ ->
                    let
                        itemUuids =
                            ReplyValue.getItemUuids value
                    in
                    if not (List.isEmpty itemUuids) then
                        itemUuids
                            |> List.map (evaluateAnswerItem appState questionnaire currentPath (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid questionnaire.knowledgeModel))
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


evaluateFollowups : AppState -> QuestionnaireDetail -> List String -> String -> Int
evaluateFollowups appState questionnaire path answerUuid =
    let
        currentPath =
            path ++ [ answerUuid ]
    in
    KnowledgeModel.getAnswerFollowupQuestions answerUuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion appState questionnaire currentPath)
        |> List.foldl (+) 0


evaluateAnswerItem : AppState -> QuestionnaireDetail -> List String -> List Question -> String -> Int
evaluateAnswerItem appState questionnaire path questions uuid =
    let
        currentPath =
            path ++ [ uuid ]
    in
    questions
        |> List.map (evaluateQuestion appState questionnaire currentPath)
        |> List.foldl (+) 0



-- Utils


updateContent : QuestionnaireDetail -> QuestionnaireContent -> QuestionnaireDetail
updateContent detail content =
    { detail
        | replies = content.replies
        , phaseUuid = content.phaseUuid
        , labels = content.labels
        , events = content.events
        , versions = content.versions
    }
