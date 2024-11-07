module Shared.Data.QuestionnaireQuestionnaire exposing
    ( QuestionCommentInfo
    , QuestionnaireQuestionnaire
    , QuestionnaireWarning
    , addCommentCount
    , addFile
    , addReopenedCommentThreadToCount
    , addResolvedCommentThreadToCount
    , calculateUnansweredQuestionsForChapter
    , clearReplyValue
    , commentsLength
    , createQuestionnaireDetail
    , decoder
    , generateReplies
    , getComments
    , getFile
    , getItemSelectQuestionValueLabel
    , getItemTitle
    , getTodos
    , getUnresolvedCommentCount
    , getWarnings
    , hasReply
    , isCurrentVersion
    , itemSelectQuestionItemMissing
    , itemSelectQuestionItemPath
    , removeCommentThreadFromCount
    , setLabels
    , setPhaseUuid
    , setReply
    , subCommentCount
    , todoUuid
    , todosLength
    , updateContent
    , updateWithQuestionnaireData
    , warningsLength
    )

import Dict exposing (Dict)
import Dict.Extra as Dict
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Regex
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Shared.Data.Package exposing (Package)
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing(..))
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireContent exposing (QuestionnaireContent)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent as QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.Reply as Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnaireFileSimple as QuestionnaireFileSimple exposing (QuestionnaireFileSimple)
import Shared.Data.WebSockets.QuestionnaireAction.SetQuestionnaireData exposing (SetQuestionnaireData)
import Shared.Markdown as Markdown
import Shared.RegexPatterns as RegexPatterns
import Shared.Utils exposing (boolToInt, getUuidString)
import String.Extra as String
import String.Format as String
import Time
import Tuple.Extra as Tuple
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)


type alias QuestionnaireQuestionnaire =
    { uuid : Uuid
    , name : String
    , isTemplate : Bool
    , packageId : String
    , knowledgeModel : KnowledgeModel
    , replies : Dict String Reply
    , phaseUuid : Maybe Uuid
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    , permissions : List Permission
    , labels : Dict String (List String)
    , migrationUuid : Maybe Uuid
    , unresolvedCommentCounts : Dict String (Dict String Int)
    , resolvedCommentCounts : Dict String (Dict String Int)
    , questionnaireActionsAvailable : Int
    , questionnaireImportersAvailable : Int
    , selectedQuestionTagUuids : List String
    , files : List QuestionnaireFileSimple
    }


decoder : Decoder QuestionnaireQuestionnaire
decoder =
    D.succeed QuestionnaireQuestionnaire
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "isTemplate" D.bool
        |> D.required "packageId" D.string
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "sharing" QuestionnaireSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "migrationUuid" (D.maybe Uuid.decoder)
        |> D.required "unresolvedCommentCounts" (D.dict (D.dict D.int))
        |> D.required "resolvedCommentCounts" (D.dict (D.dict D.int))
        |> D.required "questionnaireActionsAvailable" D.int
        |> D.required "questionnaireImportersAvailable" D.int
        |> D.required "selectedQuestionTagUuids" (D.list D.string)
        |> D.required "files" (D.list QuestionnaireFileSimple.decoder)


addCommentCount : String -> Uuid -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
addCommentCount path threadUuid questionnaire =
    let
        threadUuidString =
            Uuid.toString threadUuid

        questionDict =
            Dict.get path questionnaire.unresolvedCommentCounts
                |> Maybe.withDefault Dict.empty

        count =
            Dict.get threadUuidString questionDict
                |> Maybe.withDefault 0
    in
    { questionnaire | unresolvedCommentCounts = Dict.insert path (Dict.insert threadUuidString (count + 1) questionDict) questionnaire.unresolvedCommentCounts }


subCommentCount : String -> Uuid -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
subCommentCount path threadUuid questionnaire =
    let
        threadUuidString =
            Uuid.toString threadUuid

        questionDict =
            Dict.get path questionnaire.unresolvedCommentCounts
                |> Maybe.withDefault Dict.empty

        count =
            Dict.get threadUuidString questionDict
                |> Maybe.withDefault 0
    in
    { questionnaire | unresolvedCommentCounts = Dict.insert path (Dict.insert threadUuidString (max 0 (count - 1)) questionDict) questionnaire.unresolvedCommentCounts }


addReopenedCommentThreadToCount : String -> Uuid -> Int -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
addReopenedCommentThreadToCount path threadUuid count questionnaire =
    let
        threadUuidString =
            Uuid.toString threadUuid

        unresolvedQuestionDict =
            Dict.get path questionnaire.unresolvedCommentCounts
                |> Maybe.withDefault Dict.empty

        resolvedQuestionDict =
            Dict.get path questionnaire.resolvedCommentCounts
                |> Maybe.withDefault Dict.empty
    in
    { questionnaire
        | unresolvedCommentCounts = Dict.insert path (Dict.insert threadUuidString count unresolvedQuestionDict) questionnaire.unresolvedCommentCounts
        , resolvedCommentCounts = Dict.insert path (Dict.remove threadUuidString resolvedQuestionDict) questionnaire.resolvedCommentCounts
    }


addResolvedCommentThreadToCount : String -> Uuid -> Int -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
addResolvedCommentThreadToCount path threadUuid count questionnaire =
    let
        threadUuidString =
            Uuid.toString threadUuid

        unresolvedQuestionDict =
            Dict.get path questionnaire.unresolvedCommentCounts
                |> Maybe.withDefault Dict.empty

        resolvedQuestionDict =
            Dict.get path questionnaire.resolvedCommentCounts
                |> Maybe.withDefault Dict.empty
    in
    { questionnaire
        | unresolvedCommentCounts = Dict.insert path (Dict.remove threadUuidString unresolvedQuestionDict) questionnaire.unresolvedCommentCounts
        , resolvedCommentCounts = Dict.insert path (Dict.insert threadUuidString count resolvedQuestionDict) questionnaire.resolvedCommentCounts
    }


removeCommentThreadFromCount : String -> Uuid -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
removeCommentThreadFromCount path threadUuid questionnaire =
    let
        removeFromUnresolvedCommentCounts q =
            case Dict.get path q.unresolvedCommentCounts of
                Just questionDict ->
                    { q | unresolvedCommentCounts = Dict.insert path (Dict.remove (Uuid.toString threadUuid) questionDict) questionnaire.unresolvedCommentCounts }

                Nothing ->
                    q

        removeFromResolvedCommentCounts q =
            case Dict.get path q.resolvedCommentCounts of
                Just questionDict ->
                    { q | resolvedCommentCounts = Dict.insert path (Dict.remove (Uuid.toString threadUuid) questionDict) questionnaire.resolvedCommentCounts }

                Nothing ->
                    q
    in
    questionnaire
        |> removeFromUnresolvedCommentCounts
        |> removeFromResolvedCommentCounts


createQuestionnaireDetail : Package -> KnowledgeModel -> QuestionnaireQuestionnaire
createQuestionnaireDetail package km =
    { uuid = Uuid.nil
    , name = ""
    , isTemplate = False
    , visibility = PrivateQuestionnaire
    , sharing = RestrictedQuestionnaire
    , permissions = []
    , packageId = package.id
    , knowledgeModel = km
    , replies = Dict.empty
    , unresolvedCommentCounts = Dict.empty
    , resolvedCommentCounts = Dict.empty
    , phaseUuid = Maybe.andThen Uuid.fromString (List.head km.phaseUuids)
    , labels = Dict.empty
    , migrationUuid = Nothing
    , questionnaireActionsAvailable = 0
    , questionnaireImportersAvailable = 0
    , selectedQuestionTagUuids = []
    , files = []
    }


updateWithQuestionnaireData : SetQuestionnaireData -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
updateWithQuestionnaireData data detail =
    { detail
        | name = data.name
        , isTemplate = data.isTemplate
        , visibility = data.visibility
        , sharing = data.sharing
        , permissions = data.permissions
    }


setPhaseUuid : Maybe Uuid -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
setPhaseUuid phaseUuid questionnaire =
    { questionnaire | phaseUuid = phaseUuid }


setReply : String -> Reply -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
setReply path reply questionnaire =
    { questionnaire | replies = Dict.insert path reply questionnaire.replies }


clearReplyValue : String -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
clearReplyValue path questionnaire =
    { questionnaire | replies = Dict.remove path questionnaire.replies }


setLabels : String -> List String -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
setLabels path labels questionnaire =
    { questionnaire | labels = Dict.insert path labels questionnaire.labels }


getUnresolvedCommentCount : String -> QuestionnaireQuestionnaire -> Int
getUnresolvedCommentCount path questionnaire =
    Dict.get path questionnaire.unresolvedCommentCounts
        |> Maybe.unwrap [] Dict.values
        |> List.sum


getResolvedCommentCount : String -> QuestionnaireQuestionnaire -> Int
getResolvedCommentCount path questionnaire =
    Dict.get path questionnaire.resolvedCommentCounts
        |> Maybe.unwrap [] Dict.values
        |> List.sum


todosLength : QuestionnaireQuestionnaire -> Int
todosLength =
    List.length << getTodos


getTodos : QuestionnaireQuestionnaire -> List QuestionnaireTodo
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


commentsLength : QuestionnaireQuestionnaire -> Int
commentsLength =
    List.sum << List.map .unresolvedComments << getComments


type alias QuestionCommentInfo =
    { chapter : Chapter
    , question : Question
    , path : String
    , unresolvedComments : Int
    , resolvedComments : Int
    }


getComments : QuestionnaireQuestionnaire -> List QuestionCommentInfo
getComments questionnaire =
    let
        fn chapter currentPath question =
            let
                unresolvedCommentCount =
                    getUnresolvedCommentCount (pathToString currentPath) questionnaire

                resolvedCommentCount =
                    getResolvedCommentCount (pathToString currentPath) questionnaire
            in
            if unresolvedCommentCount > 0 || resolvedCommentCount > 0 then
                [ { chapter = chapter
                  , question = question
                  , path = pathToString currentPath
                  , unresolvedComments = getUnresolvedCommentCount (pathToString currentPath) questionnaire
                  , resolvedComments = getResolvedCommentCount (pathToString currentPath) questionnaire
                  }
                ]

            else
                []
    in
    concatMapVisibleQuestions fn questionnaire


itemSelectQuestionItemMissing : QuestionnaireQuestionnaire -> Maybe String -> String -> Bool
itemSelectQuestionItemMissing questionnaire itemSelectQuestionListQuestionUuid path =
    case getReplyValue questionnaire path of
        Just replyValue ->
            case replyValue of
                ItemSelectReply itemUuid ->
                    let
                        mbItemQuestionUuid =
                            itemSelectQuestionListQuestionUuid
                                |> Maybe.andThen (\uuid -> KnowledgeModel.getQuestion uuid questionnaire.knowledgeModel)
                                |> Maybe.map Question.getUuid
                    in
                    case mbItemQuestionUuid of
                        Just itemQuestionUuid ->
                            let
                                itemExists =
                                    questionnaire.replies
                                        |> Dict.filter (\key _ -> String.endsWith itemQuestionUuid key)
                                        |> Dict.values
                                        |> List.any (List.member itemUuid << ReplyValue.getItemUuids << .value)
                            in
                            not itemExists

                        Nothing ->
                            False

                _ ->
                    False

        Nothing ->
            False


itemSelectQuestionItemPath : QuestionnaireQuestionnaire -> Maybe String -> String -> Maybe String
itemSelectQuestionItemPath questionnaire itemSelectQuestionListQuestionUuid path =
    case getReplyValue questionnaire path of
        Just replyValue ->
            case replyValue of
                ItemSelectReply itemUuid ->
                    let
                        mbItemQuestionUuid =
                            itemSelectQuestionListQuestionUuid
                                |> Maybe.andThen (\uuid -> KnowledgeModel.getQuestion uuid questionnaire.knowledgeModel)
                                |> Maybe.map Question.getUuid
                    in
                    case mbItemQuestionUuid of
                        Just itemQuestionUuid ->
                            questionnaire.replies
                                |> Dict.filter (\key _ -> String.endsWith itemQuestionUuid key)
                                |> Dict.toList
                                |> List.find (List.member itemUuid << ReplyValue.getItemUuids << .value << Tuple.second)
                                |> Maybe.map (\( key, _ ) -> key ++ "." ++ itemUuid)

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing


getItemSelectQuestionValueLabel : AppState -> QuestionnaireQuestionnaire -> String -> String -> String
getItemSelectQuestionValueLabel appState questionnaire itemSelectQuestionUuid itemUuid =
    let
        mbItemSelectQuestion =
            KnowledgeModel.getQuestion itemSelectQuestionUuid questionnaire.knowledgeModel

        mbListQuestionUuid =
            mbItemSelectQuestion
                |> Maybe.andThen Question.getListQuestionUuid
                |> Maybe.andThen (\uuid -> KnowledgeModel.getQuestion uuid questionnaire.knowledgeModel)
                |> Maybe.map Question.getUuid

        fallbackItemName =
            gettext "Item" appState.locale
    in
    case mbListQuestionUuid of
        Just listQuestionUuid ->
            let
                itemTemplateQuestions =
                    KnowledgeModel.getQuestionItemTemplateQuestions listQuestionUuid questionnaire.knowledgeModel

                itemsToOptions ( itemQuestionPath, reply ) =
                    ReplyValue.getItemUuids reply.value
                        |> List.indexedMap
                            (\i iUuid ->
                                ( iUuid
                                , getItemTitle questionnaire (String.split "." itemQuestionPath ++ [ itemUuid ]) itemTemplateQuestions
                                    |> Maybe.withDefault (String.format (gettext "Item %s" appState.locale) [ String.fromInt (i + 1) ])
                                )
                            )
            in
            questionnaire.replies
                |> Dict.filter (\key _ -> String.endsWith listQuestionUuid key)
                |> Dict.find (\_ value -> List.member itemUuid (ReplyValue.getItemUuids value.value))
                |> Maybe.unwrap [] itemsToOptions
                |> List.find (\( iUuid, _ ) -> iUuid == itemUuid)
                |> Maybe.unwrap fallbackItemName Tuple.second

        Nothing ->
            fallbackItemName


type alias QuestionnaireWarning =
    { chapter : Chapter
    , question : Question
    , path : String
    }


warningsLength : QuestionnaireQuestionnaire -> Int
warningsLength =
    List.length << getWarnings


getWarnings : QuestionnaireQuestionnaire -> List QuestionnaireWarning
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

                ItemSelectQuestion _ questionData ->
                    if itemSelectQuestionItemMissing questionnaire questionData.listQuestionUuid (pathToString currentPath) then
                        [ questionnaireWarning ]

                    else
                        []

                FileQuestion _ _ ->
                    case getReplyValue questionnaire (pathToString currentPath) of
                        Just replyValue ->
                            case replyValue of
                                FileReply fileUuid ->
                                    if Maybe.isJust (getFile questionnaire fileUuid) then
                                        []

                                    else
                                        [ questionnaireWarning ]

                                _ ->
                                    []

                        Nothing ->
                            []

                _ ->
                    []
    in
    concatMapVisibleQuestions fn questionnaire


concatMapVisibleQuestions : (Chapter -> List String -> Question -> List a) -> QuestionnaireQuestionnaire -> List a
concatMapVisibleQuestions fn questionnaire =
    List.concatMap
        (concatMapVisibleQuestionsChapters fn questionnaire)
        (KnowledgeModel.getChapters questionnaire.knowledgeModel)


concatMapVisibleQuestionsChapters : (Chapter -> List String -> Question -> List a) -> QuestionnaireQuestionnaire -> Chapter -> List a
concatMapVisibleQuestionsChapters fn questionnaire chapter =
    List.concatMap
        (mapVisibleQuestion fn questionnaire chapter [ chapter.uuid ])
        (KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel)


mapVisibleQuestion : (Chapter -> List String -> Question -> List a) -> QuestionnaireQuestionnaire -> Chapter -> List String -> Question -> List a
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


getReplyValue : QuestionnaireQuestionnaire -> String -> Maybe ReplyValue
getReplyValue questionnaire path =
    Maybe.map .value <|
        Dict.get path questionnaire.replies


hasTodo : QuestionnaireQuestionnaire -> String -> Bool
hasTodo questionnaire path =
    Maybe.unwrap False (List.member todoUuid) (Dict.get path questionnaire.labels)


pathToString : List String -> String
pathToString =
    String.join "."


todoUuid : String
todoUuid =
    "615b9028-5e3f-414f-b245-12d2ae2eeb20"


hasReply : String -> QuestionnaireQuestionnaire -> Bool
hasReply path questionnaire =
    Maybe.unwrap False (not << ReplyValue.isEmpty) (getReplyValue questionnaire path)


lastVisibleEvent : List QuestionnaireEvent -> Maybe QuestionnaireEvent
lastVisibleEvent =
    List.reverse
        >> List.dropWhile QuestionnaireEvent.isInvisible
        >> List.head


isCurrentVersion : List QuestionnaireEvent -> Uuid -> Bool
isCurrentVersion questionnaire eventUuid =
    Maybe.map QuestionnaireEvent.getUuid (lastVisibleEvent questionnaire) == Just eventUuid


getItemTitle : QuestionnaireQuestionnaire -> List String -> List Question -> Maybe String
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


getFile : QuestionnaireQuestionnaire -> Uuid -> Maybe QuestionnaireFileSimple
getFile questionnaire fileUuid =
    List.find (\file -> file.uuid == fileUuid) questionnaire.files


addFile : QuestionnaireFileSimple -> QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire
addFile file questionnaire =
    { questionnaire | files = file :: questionnaire.files }



-- Evaluations


calculateUnansweredQuestionsForChapter : QuestionnaireQuestionnaire -> Chapter -> Int
calculateUnansweredQuestionsForChapter questionnaire =
    Tuple.first << evaluateChapter questionnaire


evaluateChapter : QuestionnaireQuestionnaire -> Chapter -> ( Int, Int )
evaluateChapter questionnaire chapter =
    KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire [ chapter.uuid ])
        |> List.foldl Tuple.sum ( 0, 0 )


evaluateQuestion : QuestionnaireQuestionnaire -> List String -> Question -> ( Int, Int )
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


evaluateFollowups : QuestionnaireQuestionnaire -> List String -> String -> ( Int, Int )
evaluateFollowups questionnaire path answerUuid =
    let
        currentPath =
            path ++ [ answerUuid ]
    in
    KnowledgeModel.getAnswerFollowupQuestions answerUuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire currentPath)
        |> List.foldl Tuple.sum ( 0, 0 )


evaluateAnswerItem : QuestionnaireQuestionnaire -> List String -> List Question -> String -> ( Int, Int )
evaluateAnswerItem questionnaire path questions uuid =
    let
        currentPath =
            path ++ [ uuid ]
    in
    questions
        |> List.map (evaluateQuestion questionnaire currentPath)
        |> List.foldl Tuple.sum ( 0, 0 )



-- Generating Replies


generateReplies : Time.Posix -> Seed -> String -> KnowledgeModel -> QuestionnaireQuestionnaire -> ( Seed, Maybe String, QuestionnaireQuestionnaire )
generateReplies currentTime seed questionUuid km questionnaireDetail =
    let
        parentMap =
            KnowledgeModel.createParentMap km

        ( newSeed, mbChapterUuid, replies ) =
            foldReplies currentTime km parentMap seed questionUuid Dict.empty
    in
    ( newSeed
    , mbChapterUuid
    , { questionnaireDetail | replies = replies }
    )


foldReplies : Time.Posix -> KnowledgeModel -> KnowledgeModel.ParentMap -> Seed -> String -> Dict String Reply -> ( Seed, Maybe String, Dict String Reply )
foldReplies currentTime km parentMap seed questionUuid replies =
    let
        parentUuid =
            KnowledgeModel.getParent parentMap questionUuid

        prefixPaths prefix repliesDict =
            Dict.mapKeys (\k -> prefix ++ "." ++ k) repliesDict

        foldReplies_ =
            foldReplies currentTime km parentMap
    in
    case
        ( KnowledgeModel.getChapter parentUuid km
        , KnowledgeModel.getQuestion parentUuid km
        , KnowledgeModel.getAnswer parentUuid km
        )
    of
        ( Just chapter, Nothing, Nothing ) ->
            -- just prefix replies with chapter uuid
            ( seed, Just chapter.uuid, prefixPaths chapter.uuid replies )

        ( Nothing, Just question, Nothing ) ->
            -- add item to question, get parent question and continue
            let
                ( itemUuid, newSeed ) =
                    getUuidString seed

                reply =
                    { value = ReplyValue.ItemListReply [ itemUuid ]
                    , createdAt = currentTime
                    , createdBy = Nothing
                    }

                listQuestionUuid =
                    Question.getUuid question
            in
            foldReplies_ newSeed
                listQuestionUuid
                (Dict.insert listQuestionUuid reply (prefixPaths listQuestionUuid (prefixPaths itemUuid replies)))

        ( Nothing, Nothing, Just answer ) ->
            -- select answer, get parent question and continue
            let
                answerParentQuestionUuid =
                    KnowledgeModel.getParent parentMap answer.uuid
            in
            case KnowledgeModel.getQuestion answerParentQuestionUuid km of
                Just question ->
                    let
                        reply =
                            { value = ReplyValue.AnswerReply answer.uuid
                            , createdAt = currentTime
                            , createdBy = Nothing
                            }

                        answerQuestionUuid =
                            Question.getUuid question
                    in
                    foldReplies_ seed
                        answerQuestionUuid
                        (Dict.insert answerQuestionUuid reply (prefixPaths answerQuestionUuid (prefixPaths answer.uuid replies)))

                Nothing ->
                    -- should not happen
                    ( seed, Nothing, replies )

        _ ->
            -- should not happen
            ( seed, Nothing, replies )



-- Utils


updateContent : QuestionnaireQuestionnaire -> QuestionnaireContent -> QuestionnaireQuestionnaire
updateContent detail content =
    { detail
        | replies = content.replies
        , phaseUuid = content.phaseUuid
        , labels = content.labels
    }
