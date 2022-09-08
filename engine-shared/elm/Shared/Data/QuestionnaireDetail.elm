module Shared.Data.QuestionnaireDetail exposing
    ( QuestionCommentInfo
    , QuestionnaireDetail
    , QuestionnaireWarning
    , addComment
    , calculatePhasesAnsweredIndications
    , calculateUnansweredQuestionsForChapter
    , canComment
    , clearReplyValue
    , commentsLength
    , createQuestionnaireDetail
    , decoder
    , deleteComment
    , deleteCommentThread
    , editComment
    , getCommentCount
    , getComments
    , getItemTitle
    , getTodos
    , getVersionByEventUuid
    , getWarnings
    , hasReply
    , isAnonymousProject
    , isCurrentVersion
    , isEditor
    , isMigrating
    , isOwner
    , isVersion
    , reopenCommentThread
    , resolveCommentThread
    , setLabels
    , setPhaseUuid
    , setReply
    , todoUuid
    , todosLength
    , updateContent
    , warningsLength
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Auth.Session as Session
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireContent exposing (QuestionnaireContent)
import Shared.Data.QuestionnaireDetail.Comment exposing (Comment)
import Shared.Data.QuestionnaireDetail.CommentThread as CommentThread exposing (CommentThread)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.Reply as Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnairePerm as QuestionnairePerm
import Shared.Data.QuestionnaireVersion as QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.SummaryReport.AnsweredIndicationData exposing (AnsweredIndicationData)
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Shared.Data.Template.TemplateState as TemplateState exposing (TemplateState)
import Shared.Data.TemplateSuggestion as TemplateSuggestion exposing (TemplateSuggestion)
import Shared.Data.UserInfo as UserInfo
import Shared.Markdown as Markdown
import Shared.RegexPatterns as RegexPatterns
import Shared.Utils exposing (boolToInt)
import String.Extra as String
import Time
import Tuple.Extra as Tuple
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias QuestionnaireDetail =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , projectTags : List String
    , isTemplate : Bool
    , package : Package
    , packageVersions : List Version
    , knowledgeModel : KnowledgeModel
    , replies : Dict String Reply
    , commentThreadsMap : Dict String (List CommentThread)
    , phaseUuid : Maybe Uuid
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    , permissions : List Permission
    , selectedQuestionTagUuids : List String
    , templateId : Maybe String
    , template : Maybe TemplateSuggestion
    , templateState : Maybe TemplateState
    , formatUuid : Maybe Uuid
    , format : Maybe TemplateFormat
    , labels : Dict String (List String)
    , versions : List QuestionnaireVersion
    , migrationUuid : Maybe Uuid
    }


decoder : Decoder QuestionnaireDetail
decoder =
    D.succeed QuestionnaireDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "projectTags" (D.list D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "package" Package.decoder
        |> D.required "packageVersions" (D.list Version.decoder)
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "commentThreadsMap" (D.dict (D.list CommentThread.decoder))
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "selectedQuestionTagUuids" (D.list D.string)
        |> D.required "templateId" (D.maybe D.string)
        |> D.required "template" (D.maybe TemplateSuggestion.decoder)
        |> D.required "templateState" (D.maybe TemplateState.decoder)
        |> D.required "formatUuid" (D.maybe Uuid.decoder)
        |> D.required "format" (D.maybe TemplateFormat.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "versions" (D.list QuestionnaireVersion.decoder)
        |> D.required "migrationUuid" (D.maybe Uuid.decoder)


isEditor : AbstractAppState a -> QuestionnaireDetail -> Bool
isEditor appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.edit


isOwner : AbstractAppState a -> QuestionnaireDetail -> Bool
isOwner appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.admin


isAnonymousProject : QuestionnaireDetail -> Bool
isAnonymousProject questionnaire =
    List.isEmpty questionnaire.permissions


isMigrating : QuestionnaireDetail -> Bool
isMigrating =
    Maybe.isJust << .migrationUuid


canComment : AbstractAppState a -> QuestionnaireDetail -> Bool
canComment appState questionnaire =
    hasPerm appState questionnaire QuestionnairePerm.comment && not (isMigrating questionnaire)


createQuestionnaireDetail : Package -> KnowledgeModel -> QuestionnaireDetail
createQuestionnaireDetail package km =
    { uuid = Uuid.nil
    , name = ""
    , description = Nothing
    , projectTags = []
    , isTemplate = False
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , permissions = []
    , package = package
    , packageVersions = []
    , knowledgeModel = km
    , replies = Dict.empty
    , commentThreadsMap = Dict.empty
    , phaseUuid = Maybe.andThen Uuid.fromString (List.head km.phaseUuids)
    , selectedQuestionTagUuids = []
    , templateId = Nothing
    , template = Nothing
    , templateState = Nothing
    , formatUuid = Nothing
    , format = Nothing
    , labels = Dict.empty
    , versions = []
    , migrationUuid = Nothing
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
                        [ QuestionnairePerm.view, QuestionnairePerm.comment, QuestionnairePerm.edit ]

                    VisibleCommentQuestionnaire ->
                        [ QuestionnairePerm.view, QuestionnairePerm.comment ]

                    VisibleViewQuestionnaire ->
                        [ QuestionnairePerm.view ]

                    PrivateQuestionnaire ->
                        []

            else
                []

        sharingPerms =
            case questionnaire.sharing of
                AnyoneWithLinkEditQuestionnaire ->
                    [ QuestionnairePerm.view, QuestionnairePerm.comment, QuestionnairePerm.edit ]

                AnyoneWithLinkCommentQuestionnaire ->
                    [ QuestionnairePerm.view, QuestionnairePerm.comment ]

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


resolveCommentThread : String -> Uuid -> QuestionnaireDetail -> QuestionnaireDetail
resolveCommentThread path threadUuid =
    let
        mapCommentThread commentThread =
            { commentThread | resolved = True }
    in
    mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))


reopenCommentThread : String -> Uuid -> QuestionnaireDetail -> QuestionnaireDetail
reopenCommentThread path threadUuid =
    let
        mapCommentThread commentThread =
            { commentThread | resolved = False }
    in
    mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))


deleteCommentThread : String -> Uuid -> QuestionnaireDetail -> QuestionnaireDetail
deleteCommentThread path threadUuid =
    mapCommentThreads path (List.filter (\t -> t.uuid /= threadUuid))


addComment : String -> Uuid -> Bool -> Comment -> QuestionnaireDetail -> QuestionnaireDetail
addComment path threadUuid private comment questionnaire =
    let
        threadExists =
            Dict.get path questionnaire.commentThreadsMap
                |> Maybe.withDefault []
                |> List.any (.uuid >> (==) threadUuid)

        mapCommentThread commentThread =
            { commentThread | comments = commentThread.comments ++ [ comment ] }

        questionnaireWithThread =
            if threadExists then
                questionnaire

            else
                addCommentThread path threadUuid private comment questionnaire
    in
    mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread)) questionnaireWithThread


addCommentThread : String -> Uuid -> Bool -> Comment -> QuestionnaireDetail -> QuestionnaireDetail
addCommentThread path threadUuid private comment questionnaire =
    let
        commentThread =
            { uuid = threadUuid
            , resolved = False
            , comments = []
            , private = private
            , createdBy = comment.createdBy
            }

        commentThreads =
            Dict.get path questionnaire.commentThreadsMap
                |> Maybe.withDefault []
    in
    { questionnaire | commentThreadsMap = Dict.insert path (commentThreads ++ [ commentThread ]) questionnaire.commentThreadsMap }


editComment : String -> Uuid -> Uuid -> Time.Posix -> String -> QuestionnaireDetail -> QuestionnaireDetail
editComment path threadUuid commentUuid updatedAt newText =
    let
        mapComment comment =
            if comment.uuid == commentUuid then
                { comment | text = newText, updatedAt = updatedAt }

            else
                comment

        mapCommentThread commentThread =
            { commentThread | comments = List.map mapComment commentThread.comments }
    in
    mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))


deleteComment : String -> Uuid -> Uuid -> QuestionnaireDetail -> QuestionnaireDetail
deleteComment path threadUuid commentUuid =
    let
        mapCommentThread commentThread =
            { commentThread | comments = List.filter (\c -> c.uuid /= commentUuid) commentThread.comments }
    in
    mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))


getCommentCount : String -> QuestionnaireDetail -> Int
getCommentCount path questionnaire =
    Dict.get path questionnaire.commentThreadsMap
        |> Maybe.withDefault []
        |> List.filter (\thread -> not thread.resolved)
        |> List.map (.comments >> List.length)
        |> List.sum


mapCommentThreads : String -> (List CommentThread -> List CommentThread) -> QuestionnaireDetail -> QuestionnaireDetail
mapCommentThreads path map questionnaire =
    let
        mbCommentThreads =
            Dict.get path questionnaire.commentThreadsMap
                |> Maybe.map map
    in
    case mbCommentThreads of
        Just commentThreads ->
            { questionnaire | commentThreadsMap = Dict.insert path commentThreads questionnaire.commentThreadsMap }

        Nothing ->
            questionnaire


wrapMapCommentThread : Uuid -> (CommentThread -> CommentThread) -> CommentThread -> CommentThread
wrapMapCommentThread threadUuid mapCommentThread commentThread =
    if commentThread.uuid == threadUuid then
        mapCommentThread commentThread

    else
        commentThread


todosLength : QuestionnaireDetail -> Int
todosLength =
    List.length << getTodos


getTodos : QuestionnaireDetail -> List QuestionnaireTodo
getTodos questionnaire =
    let
        fn chapter currentPath question =
            if hasTodo questionnaire (pathToString currentPath) then
                [ { chapter = chapter
                  , question = question
                  , path = pathToString currentPath
                  }
                ]

            else
                []
    in
    concatMapVisibleQuestions fn questionnaire


commentsLength : QuestionnaireDetail -> Int
commentsLength =
    List.sum << List.map .comments << getComments


type alias QuestionCommentInfo =
    { chapter : Chapter
    , question : Question
    , path : String
    , comments : Int
    }


getComments : QuestionnaireDetail -> List QuestionCommentInfo
getComments questionnaire =
    let
        fn chapter currentPath question =
            let
                questionCommentCount =
                    Dict.get (pathToString currentPath) questionnaire.commentThreadsMap
                        |> Maybe.withDefault []
                        |> List.filter (not << .resolved)
                        |> List.map (.comments >> List.length)
                        |> List.sum
            in
            if questionCommentCount > 0 then
                [ { chapter = chapter
                  , question = question
                  , path = pathToString currentPath
                  , comments = questionCommentCount
                  }
                ]

            else
                []
    in
    concatMapVisibleQuestions fn questionnaire


type alias QuestionnaireWarning =
    { chapter : Chapter
    , question : Question
    , path : String
    }


warningsLength : QuestionnaireDetail -> Int
warningsLength =
    List.length << getWarnings


getWarnings : QuestionnaireDetail -> List QuestionnaireWarning
getWarnings questionnaire =
    let
        fn chapter currentPath question =
            let
                questionnaireWarning =
                    { chapter = chapter
                    , question = question
                    , path = pathToString currentPath
                    }

                checkValue regex value =
                    if not (String.isEmpty value) && not (Regex.contains regex value) then
                        [ questionnaireWarning
                        ]

                    else
                        []
            in
            case question of
                ValueQuestion _ questionData ->
                    case getReplyValue questionnaire (pathToString currentPath) of
                        Just replyValue ->
                            case replyValue of
                                StringReply value ->
                                    case questionData.valueType of
                                        DateQuestionValueType ->
                                            checkValue RegexPatterns.date value

                                        DateTimeQuestionValueType ->
                                            checkValue RegexPatterns.datetime value

                                        TimeQuestionValueType ->
                                            checkValue RegexPatterns.time value

                                        EmailQuestionValueType ->
                                            checkValue RegexPatterns.email value

                                        UrlQuestionValueType ->
                                            checkValue RegexPatterns.url value

                                        ColorQuestionValueType ->
                                            checkValue RegexPatterns.color value

                                        _ ->
                                            []

                                _ ->
                                    []

                        Nothing ->
                            []

                _ ->
                    []
    in
    concatMapVisibleQuestions fn questionnaire


concatMapVisibleQuestions : (Chapter -> List String -> Question -> List a) -> QuestionnaireDetail -> List a
concatMapVisibleQuestions fn questionnaire =
    List.concatMap
        (concatMapVisibleQuestionsChapters fn questionnaire)
        (KnowledgeModel.getChapters questionnaire.knowledgeModel)


concatMapVisibleQuestionsChapters : (Chapter -> List String -> Question -> List a) -> QuestionnaireDetail -> Chapter -> List a
concatMapVisibleQuestionsChapters fn questionnaire chapter =
    List.concatMap
        (mapVisibleQuestion fn questionnaire chapter [ chapter.uuid ])
        (KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel)


mapVisibleQuestion : (Chapter -> List String -> Question -> List a) -> QuestionnaireDetail -> Chapter -> List String -> Question -> List a
mapVisibleQuestion fn questionnaire chapter path question =
    let
        currentPath =
            path ++ [ Question.getUuid question ]

        results =
            fn chapter currentPath question

        childResults =
            case getReplyValue questionnaire (pathToString currentPath) of
                Just replyValue ->
                    let
                        km =
                            questionnaire.knowledgeModel
                    in
                    case question of
                        OptionsQuestion commonData _ ->
                            case List.find (.uuid >> (==) (ReplyValue.getAnswerUuid replyValue)) (KnowledgeModel.getQuestionAnswers commonData.uuid km) of
                                Just answer ->
                                    List.concatMap
                                        (mapVisibleQuestion fn questionnaire chapter (currentPath ++ [ answer.uuid ]))
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km)

                                Nothing ->
                                    []

                        ListQuestion commonData _ ->
                            let
                                mapVisibleItemQuestion itemUuid =
                                    List.concatMap
                                        (mapVisibleQuestion fn questionnaire chapter (currentPath ++ [ itemUuid ]))
                                        (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid km)
                            in
                            List.concatMap mapVisibleItemQuestion (ReplyValue.getItemUuids replyValue)

                        _ ->
                            []

                Nothing ->
                    []
    in
    results ++ childResults


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


isVersion : QuestionnaireDetail -> QuestionnaireEvent -> Bool
isVersion questionnaire event =
    List.any (.eventUuid >> (==) (QuestionnaireEvent.getUuid event)) questionnaire.versions


lastVisibleEvent : List QuestionnaireEvent -> Maybe QuestionnaireEvent
lastVisibleEvent =
    List.reverse
        >> List.dropWhile QuestionnaireEvent.isInvisible
        >> List.head


isCurrentVersion : List QuestionnaireEvent -> Uuid -> Bool
isCurrentVersion questionnaire eventUuid =
    Maybe.map QuestionnaireEvent.getUuid (lastVisibleEvent questionnaire) == Just eventUuid


getItemTitle : QuestionnaireDetail -> List String -> List Question -> Maybe String
getItemTitle questionnaire itemPath itemTemplateQuestions =
    let
        firstQuestionUuid =
            Maybe.unwrap "" Question.getUuid (List.head itemTemplateQuestions)

        titleFromMarkdown value =
            Markdown.toString value
                |> String.split "\n"
                |> List.find (not << String.isEmpty)
    in
    Dict.get (pathToString (itemPath ++ [ firstQuestionUuid ])) questionnaire.replies
        |> Maybe.andThen (.value >> ReplyValue.getStringReply >> titleFromMarkdown)
        |> Maybe.andThen String.toMaybe



-- Evaluations


calculatePhasesAnsweredIndications : QuestionnaireDetail -> AnsweredIndicationData
calculatePhasesAnsweredIndications questionnaire =
    let
        ( unaswered, total ) =
            KnowledgeModel.getChapters questionnaire.knowledgeModel
                |> List.map (evaluateChapter questionnaire)
                |> List.foldl Tuple.sum ( 0, 0 )
    in
    { answeredQuestions = total - unaswered
    , unansweredQuestions = unaswered
    }


calculateUnansweredQuestionsForChapter : QuestionnaireDetail -> Chapter -> Int
calculateUnansweredQuestionsForChapter questionnaire =
    Tuple.first << evaluateChapter questionnaire


evaluateChapter : QuestionnaireDetail -> Chapter -> ( Int, Int )
evaluateChapter questionnaire chapter =
    KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire [ chapter.uuid ])
        |> List.foldl Tuple.sum ( 0, 0 )


evaluateQuestion : QuestionnaireDetail -> List String -> Question -> ( Int, Int )
evaluateQuestion questionnaire path question =
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
                        |> Maybe.map (evaluateFollowups questionnaire currentPath)
                        |> Maybe.map (Tuple.sum ( 0, boolToInt requiredNow ))
                        |> Maybe.withDefault ( boolToInt requiredNow, boolToInt requiredNow )

                ListQuestion commonData _ ->
                    let
                        itemUuids =
                            ReplyValue.getItemUuids value
                    in
                    if not (List.isEmpty itemUuids) then
                        itemUuids
                            |> List.map (evaluateAnswerItem questionnaire currentPath (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid questionnaire.knowledgeModel))
                            |> List.foldl Tuple.sum ( 0, boolToInt requiredNow )

                    else
                        ( boolToInt requiredNow, boolToInt requiredNow )

                _ ->
                    if ReplyValue.isEmpty value then
                        ( boolToInt requiredNow, boolToInt requiredNow )

                    else
                        ( 0, boolToInt requiredNow )

        Nothing ->
            ( boolToInt requiredNow, boolToInt requiredNow )


evaluateFollowups : QuestionnaireDetail -> List String -> String -> ( Int, Int )
evaluateFollowups questionnaire path answerUuid =
    let
        currentPath =
            path ++ [ answerUuid ]
    in
    KnowledgeModel.getAnswerFollowupQuestions answerUuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire currentPath)
        |> List.foldl Tuple.sum ( 0, 0 )


evaluateAnswerItem : QuestionnaireDetail -> List String -> List Question -> String -> ( Int, Int )
evaluateAnswerItem questionnaire path questions uuid =
    let
        currentPath =
            path ++ [ uuid ]
    in
    questions
        |> List.map (evaluateQuestion questionnaire currentPath)
        |> List.foldl Tuple.sum ( 0, 0 )



-- Utils


updateContent : QuestionnaireDetail -> QuestionnaireContent -> QuestionnaireDetail
updateContent detail content =
    { detail
        | replies = content.replies
        , phaseUuid = content.phaseUuid
        , labels = content.labels
    }
