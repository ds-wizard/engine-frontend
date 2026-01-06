module Wizard.Api.Models.ProjectQuestionnaire exposing
    ( ProjectQuestionnaire
    , QuestionCommentInfo
    , QuestionnaireWarning
    , addCommentCount
    , addFile
    , addReopenedCommentThreadToCount
    , addResolvedCommentThreadToCount
    , calculateUnansweredQuestionsForChapter
    , clearReplyValue
    , commentsLength
    , concatMapVisibleQuestions
    , createQuestionnaireDetail
    , decoder
    , generateReplies
    , getClosestQuestionParentPath
    , getComments
    , getCurrentPhaseIndex
    , getFile
    , getItemSelectQuestionValueLabel
    , getItemTitle
    , getItemUsageInItemSelectQuestions
    , getTodos
    , getUnresolvedCommentCount
    , getWarnings
    , hasReply
    , isCurrentVersion
    , isPathVisible
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

import Common.Utils.Bool as Bool
import Common.Utils.Markdown as Markdown
import Common.Utils.RegexPatterns as RegexPatterns
import Dict exposing (Dict)
import Dict.Extra as Dict
import Flip exposing (flip)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Regex
import Result.Extra as Result
import String.Extra as String
import String.Format as String
import Time
import Tuple.Extensions as Tuple
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing(..))
import Wizard.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility(..))
import Wizard.Api.Models.ProjectContent exposing (ProjectContent)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.Reply as Reply exposing (Reply)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Wizard.Api.Models.ProjectFileSimple as ProjectFileSimple exposing (ProjectFileSimple)
import Wizard.Api.Models.WebSockets.ProjectMessage.SetProjectData exposing (SetProjectData)
import Wizard.Data.AppState exposing (AppState)


type alias ProjectQuestionnaire =
    { uuid : Uuid
    , name : String
    , isTemplate : Bool
    , knowledgeModelPackageId : String
    , knowledgeModel : KnowledgeModel
    , replies : Dict String Reply
    , phaseUuid : Maybe Uuid
    , visibility : ProjectVisibility
    , sharing : ProjectSharing
    , permissions : List Permission
    , labels : Dict String (List String)
    , migrationUuid : Maybe Uuid
    , unresolvedCommentCounts : Dict String (Dict String Int)
    , resolvedCommentCounts : Dict String (Dict String Int)
    , projectActionsAvailable : Int
    , projectImportersAvailable : Int
    , selectedQuestionTagUuids : List String
    , files : List ProjectFileSimple
    }


decoder : Decoder ProjectQuestionnaire
decoder =
    D.succeed ProjectQuestionnaire
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "isTemplate" D.bool
        |> D.required "knowledgeModelPackageId" D.string
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "replies" (D.dict Reply.decoder)
        |> D.required "phaseUuid" (D.maybe Uuid.decoder)
        |> D.required "visibility" ProjectVisibility.decoder
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "labels" (D.dict (D.list D.string))
        |> D.required "migrationUuid" (D.maybe Uuid.decoder)
        |> D.required "unresolvedCommentCounts" (D.dict (D.dict D.int))
        |> D.required "resolvedCommentCounts" (D.dict (D.dict D.int))
        |> D.required "projectActionsAvailable" D.int
        |> D.required "projectImportersAvailable" D.int
        |> D.required "selectedQuestionTagUuids" (D.list D.string)
        |> D.required "files" (D.list ProjectFileSimple.decoder)


addCommentCount : String -> Uuid -> ProjectQuestionnaire -> ProjectQuestionnaire
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


subCommentCount : String -> Uuid -> ProjectQuestionnaire -> ProjectQuestionnaire
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


addReopenedCommentThreadToCount : String -> Uuid -> Int -> ProjectQuestionnaire -> ProjectQuestionnaire
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


addResolvedCommentThreadToCount : String -> Uuid -> Int -> ProjectQuestionnaire -> ProjectQuestionnaire
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


removeCommentThreadFromCount : String -> Uuid -> ProjectQuestionnaire -> ProjectQuestionnaire
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


createQuestionnaireDetail : KnowledgeModelPackage -> KnowledgeModel -> ProjectQuestionnaire
createQuestionnaireDetail kmPackage km =
    { uuid = Uuid.nil
    , name = ""
    , isTemplate = False
    , visibility = Private
    , sharing = Restricted
    , permissions = []
    , knowledgeModelPackageId = kmPackage.id
    , knowledgeModel = km
    , replies = Dict.empty
    , unresolvedCommentCounts = Dict.empty
    , resolvedCommentCounts = Dict.empty
    , phaseUuid = Maybe.andThen Uuid.fromString (List.head km.phaseUuids)
    , labels = Dict.empty
    , migrationUuid = Nothing
    , projectActionsAvailable = 0
    , projectImportersAvailable = 0
    , selectedQuestionTagUuids = []
    , files = []
    }


updateWithQuestionnaireData : SetProjectData -> ProjectQuestionnaire -> ProjectQuestionnaire
updateWithQuestionnaireData data detail =
    { detail
        | name = data.name
        , isTemplate = data.isTemplate
        , visibility = data.visibility
        , sharing = data.sharing
        , permissions = data.permissions
        , labels = data.labels
        , unresolvedCommentCounts = data.unresolvedCommentCounts
        , resolvedCommentCounts = data.resolvedCommentCounts
    }


setPhaseUuid : Maybe Uuid -> ProjectQuestionnaire -> ProjectQuestionnaire
setPhaseUuid phaseUuid questionnaire =
    { questionnaire | phaseUuid = phaseUuid }


getCurrentPhaseIndex : ProjectQuestionnaire -> Int
getCurrentPhaseIndex questionnaire =
    case questionnaire.phaseUuid of
        Just phaseUuid ->
            KnowledgeModel.getPhases questionnaire.knowledgeModel
                |> List.map .uuid
                |> List.elemIndex (Uuid.toString phaseUuid)
                |> Maybe.withDefault 0

        Nothing ->
            0


setReply : String -> Reply -> ProjectQuestionnaire -> ProjectQuestionnaire
setReply path reply questionnaire =
    { questionnaire | replies = Dict.insert path reply questionnaire.replies }


clearReplyValue : String -> ProjectQuestionnaire -> ProjectQuestionnaire
clearReplyValue path questionnaire =
    { questionnaire | replies = Dict.remove path questionnaire.replies }


setLabels : String -> List String -> ProjectQuestionnaire -> ProjectQuestionnaire
setLabels path labels questionnaire =
    { questionnaire | labels = Dict.insert path labels questionnaire.labels }


getUnresolvedCommentCount : String -> ProjectQuestionnaire -> Int
getUnresolvedCommentCount path questionnaire =
    Dict.get path questionnaire.unresolvedCommentCounts
        |> Maybe.unwrap [] Dict.values
        |> List.sum


getResolvedCommentCount : String -> ProjectQuestionnaire -> Int
getResolvedCommentCount path questionnaire =
    Dict.get path questionnaire.resolvedCommentCounts
        |> Maybe.unwrap [] Dict.values
        |> List.sum


todosLength : ProjectQuestionnaire -> Int
todosLength =
    List.length << getTodos


getTodos : ProjectQuestionnaire -> List ProjectTodo
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


commentsLength : ProjectQuestionnaire -> Int
commentsLength =
    List.sum << List.map .unresolvedComments << getComments


type alias QuestionCommentInfo =
    { chapter : Chapter
    , question : Question
    , path : String
    , unresolvedComments : Int
    , resolvedComments : Int
    }


getComments : ProjectQuestionnaire -> List QuestionCommentInfo
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


getClosestQuestionParentPath : ProjectQuestionnaire -> KnowledgeModel.ParentMap -> String -> Maybe String
getClosestQuestionParentPath questionnaire parentMap questionUuid =
    let
        getQuestionReply qUuid =
            Dict.find (\key _ -> String.endsWith qUuid key) questionnaire.replies
    in
    case getQuestionReply questionUuid of
        Just ( questionReplyPath, _ ) ->
            Just questionReplyPath

        Nothing ->
            let
                parentUuid =
                    KnowledgeModel.getParent parentMap questionUuid

                getClosestQuestionParentPath_ =
                    getClosestQuestionParentPath questionnaire parentMap
            in
            case
                ( KnowledgeModel.getChapter parentUuid questionnaire.knowledgeModel
                , KnowledgeModel.getQuestion parentUuid questionnaire.knowledgeModel
                , KnowledgeModel.getAnswer parentUuid questionnaire.knowledgeModel
                )
            of
                -- Parent is chapter, path is straightforward
                ( Just chapter, _, _ ) ->
                    Just <| chapter.uuid ++ "." ++ questionUuid

                -- Parent is item question
                ( _, Just question, _ ) ->
                    case getQuestionReply (Question.getUuid question) of
                        -- If we have reply, we can try to get the first item to build the path
                        Just ( questionReplyPath, reply ) ->
                            case List.head (ReplyValue.getItemUuids reply.value) of
                                -- If the reply contains item uuids, we can use the first one to build the path
                                Just itemUuid ->
                                    Just <| questionReplyPath ++ "." ++ itemUuid ++ "." ++ questionUuid

                                -- Otherwise, best we can do is parent question path
                                Nothing ->
                                    Just <| questionReplyPath

                        Nothing ->
                            getClosestQuestionParentPath_ (Question.getUuid question)

                -- Parent is answer
                ( _, _, Just answer ) ->
                    let
                        answerParentQuestionUuid =
                            KnowledgeModel.getParent parentMap answer.uuid
                    in
                    case getQuestionReply answerParentQuestionUuid of
                        Just ( parentQuestinReplyPath, reply ) ->
                            -- If there is a reply for the answer's parent question and it matches the parent answer uuid, we can use it to build path
                            if ReplyValue.getAnswerUuid reply.value == answer.uuid then
                                Just <| parentQuestinReplyPath ++ "." ++ answer.uuid ++ "." ++ questionUuid
                                -- Otherwise, best we can do is parent question path

                            else
                                Just <| parentQuestinReplyPath

                        Nothing ->
                            getClosestQuestionParentPath_ answerParentQuestionUuid

                _ ->
                    Nothing


itemSelectQuestionItemMissing : ProjectQuestionnaire -> Maybe String -> String -> Bool
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


itemSelectQuestionItemPath : ProjectQuestionnaire -> Maybe String -> String -> Maybe String
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


getItemSelectQuestionValueLabel : AppState -> ProjectQuestionnaire -> String -> String -> String
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


isPathVisible : ProjectQuestionnaire -> String -> Bool
isPathVisible questionnaire path =
    case String.split "." path of
        chapterUuid :: rest ->
            isPathVisibleStep questionnaire chapterUuid rest

        [] ->
            False


isPathVisibleStep : ProjectQuestionnaire -> String -> List String -> Bool
isPathVisibleStep questionnaire prefix rest =
    case rest of
        [] ->
            -- This should never happen
            False

        _ :: [] ->
            -- End of the path reached, so far everything is visible
            True

        questionUuid :: nextUuid :: tail ->
            let
                currentPath =
                    prefix ++ "." ++ questionUuid
            in
            case getReplyValue questionnaire currentPath of
                Just (AnswerReply answerUuid) ->
                    if answerUuid == nextUuid then
                        isPathVisibleStep questionnaire (currentPath ++ "." ++ answerUuid) tail

                    else
                        False

                Just (ItemListReply itemUuids) ->
                    if List.member nextUuid itemUuids then
                        isPathVisibleStep questionnaire (currentPath ++ "." ++ nextUuid) tail

                    else
                        False

                _ ->
                    -- This should never happen
                    False


type alias QuestionnaireWarning =
    { chapter : Chapter
    , question : Question
    , path : String
    }


warningsLength : ProjectQuestionnaire -> Int
warningsLength =
    List.length << getWarnings


getWarnings : ProjectQuestionnaire -> List QuestionnaireWarning
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
                                    let
                                        typeWarnings =
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

                                        validate validation =
                                            QuestionValidation.validate
                                                { locale = Gettext.defaultLocale }
                                                validation
                                                value

                                        anyValidationWarning =
                                            questionData.validations
                                                |> List.map validate
                                                |> List.any Result.isErr

                                        validationWarnings =
                                            if List.isEmpty typeWarnings && anyValidationWarning then
                                                [ questionnaireWarning ]

                                            else
                                                []
                                    in
                                    typeWarnings ++ validationWarnings

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


concatMapVisibleQuestions : (Chapter -> List String -> Question -> List a) -> ProjectQuestionnaire -> List a
concatMapVisibleQuestions fn questionnaire =
    List.concatMap
        (concatMapVisibleQuestionsChapters fn questionnaire)
        (KnowledgeModel.getChapters questionnaire.knowledgeModel)


concatMapVisibleQuestionsChapters : (Chapter -> List String -> Question -> List a) -> ProjectQuestionnaire -> Chapter -> List a
concatMapVisibleQuestionsChapters fn questionnaire chapter =
    List.concatMap
        (mapVisibleQuestion fn questionnaire chapter [ chapter.uuid ])
        (KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel)


mapVisibleQuestion : (Chapter -> List String -> Question -> List a) -> ProjectQuestionnaire -> Chapter -> List String -> Question -> List a
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


getReplyValue : ProjectQuestionnaire -> String -> Maybe ReplyValue
getReplyValue questionnaire path =
    Maybe.map .value <|
        Dict.get path questionnaire.replies


hasTodo : ProjectQuestionnaire -> String -> Bool
hasTodo questionnaire path =
    Maybe.unwrap False (List.member todoUuid) (Dict.get path questionnaire.labels)


pathToString : List String -> String
pathToString =
    String.join "."


todoUuid : String
todoUuid =
    "615b9028-5e3f-414f-b245-12d2ae2eeb20"


hasReply : String -> ProjectQuestionnaire -> Bool
hasReply path questionnaire =
    Maybe.unwrap False (not << ReplyValue.isEmpty) (getReplyValue questionnaire path)


lastVisibleEvent : List ProjectEvent -> Maybe ProjectEvent
lastVisibleEvent =
    List.reverse
        >> List.dropWhile ProjectEvent.isInvisible
        >> List.head


isCurrentVersion : List ProjectEvent -> Uuid -> Bool
isCurrentVersion questionnaire eventUuid =
    Maybe.map ProjectEvent.getUuid (lastVisibleEvent questionnaire) == Just eventUuid


getItemTitle : ProjectQuestionnaire -> List String -> List Question -> Maybe String
getItemTitle =
    getItemTitleRecursive []


getItemTitleRecursive : List String -> ProjectQuestionnaire -> List String -> List Question -> Maybe String
getItemTitleRecursive itemUuids questionnaire itemPath itemTemplateQuestions =
    case List.head itemTemplateQuestions of
        Nothing ->
            Nothing

        Just itemTemplateQuestion ->
            let
                titleFromMarkdown value =
                    Markdown.toString value
                        |> String.split "\n"
                        |> List.find (not << String.isEmpty)

                getReply common =
                    Dict.get (pathToString (itemPath ++ [ common.uuid ])) questionnaire.replies

                getReplyValueString common =
                    getReply common
                        |> Maybe.andThen (titleFromMarkdown << ReplyValue.getStringReply << .value)
                        |> Maybe.andThen String.toMaybe
            in
            case itemTemplateQuestion of
                OptionsQuestion common _ ->
                    getReply common
                        |> Maybe.map (KnowledgeModel.getAnswerName questionnaire.knowledgeModel << ReplyValue.getAnswerUuid << .value)
                        |> Maybe.andThen String.toMaybe

                ListQuestion common _ ->
                    getReply common
                        |> Maybe.andThen (List.head << ReplyValue.getItemUuids << .value)
                        |> Maybe.andThen
                            (\firstItemUuid ->
                                let
                                    newItemPath =
                                        itemPath ++ [ common.uuid, firstItemUuid ]

                                    newItemTemplateQuestion =
                                        KnowledgeModel.getQuestionItemTemplateQuestions common.uuid questionnaire.knowledgeModel
                                in
                                getItemTitleRecursive itemUuids questionnaire newItemPath newItemTemplateQuestion
                            )

                ValueQuestion common _ ->
                    getReplyValueString common

                IntegrationQuestion common _ ->
                    getReplyValueString common

                MultiChoiceQuestion common _ ->
                    getReply common
                        |> Maybe.map (ReplyValue.getChoiceUuid << .value)
                        |> Maybe.andThen
                            (\uuids ->
                                if List.isEmpty uuids then
                                    Nothing

                                else
                                    List.map (KnowledgeModel.getChoiceName questionnaire.knowledgeModel) uuids
                                        |> List.filter (not << String.isEmpty)
                                        |> List.sort
                                        |> String.join ", "
                                        |> Just
                            )

                ItemSelectQuestion common itemSelectQuestion ->
                    getReply common
                        |> Maybe.map (ReplyValue.getSelectedItemUuid << .value)
                        |> Maybe.andThen
                            (\itemUuid ->
                                let
                                    findByPathAndItemUuid listQuestionUuid key value =
                                        String.endsWith listQuestionUuid key
                                            && List.member itemUuid (ReplyValue.getItemUuids value.value)

                                    createTargetItemPath ( key, _ ) =
                                        String.split "." key ++ [ itemUuid ]

                                    getTargetItemTitleByPath listQuestionUuid newItemPath =
                                        if List.member itemUuid itemUuids then
                                            Nothing

                                        else
                                            getItemTitleRecursive (itemUuid :: itemUuids) questionnaire newItemPath (KnowledgeModel.getQuestionItemTemplateQuestions listQuestionUuid questionnaire.knowledgeModel)

                                    getTargetItemTitle listQuestionUuid =
                                        questionnaire.replies
                                            |> Dict.find (findByPathAndItemUuid listQuestionUuid)
                                            |> Maybe.map createTargetItemPath
                                            |> Maybe.andThen (getTargetItemTitleByPath listQuestionUuid)
                                in
                                Maybe.andThen getTargetItemTitle itemSelectQuestion.listQuestionUuid
                            )

                FileQuestion common _ ->
                    getReply common
                        |> Maybe.andThen (ReplyValue.getFileUuid << .value)
                        |> Maybe.andThen (getFile questionnaire)
                        |> Maybe.map .fileName


getFile : ProjectQuestionnaire -> Uuid -> Maybe ProjectFileSimple
getFile questionnaire fileUuid =
    List.find (\file -> file.uuid == fileUuid) questionnaire.files


addFile : ProjectFileSimple -> ProjectQuestionnaire -> ProjectQuestionnaire
addFile file questionnaire =
    { questionnaire | files = file :: questionnaire.files }


getItemUsageInItemSelectQuestions : ProjectQuestionnaire -> String -> List ( String, String )
getItemUsageInItemSelectQuestions questionnaire itemUuid =
    let
        mapItem ( path, reply ) =
            case String.toMaybe (ReplyValue.getSelectedItemUuid reply.value) of
                Just selectedItemUuid ->
                    if selectedItemUuid == itemUuid then
                        String.split "." path
                            |> List.last
                            |> Maybe.andThen (flip KnowledgeModel.getQuestion questionnaire.knowledgeModel)
                            |> Maybe.map (\q -> ( path, Question.getTitle q ))

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    Dict.toList questionnaire.replies
        |> List.filterMap mapItem



-- Evaluations


calculateUnansweredQuestionsForChapter : ProjectQuestionnaire -> Chapter -> Int
calculateUnansweredQuestionsForChapter questionnaire =
    Tuple.first << evaluateChapter questionnaire


evaluateChapter : ProjectQuestionnaire -> Chapter -> ( Int, Int )
evaluateChapter questionnaire chapter =
    KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire [ chapter.uuid ])
        |> List.foldl Tuple.sum ( 0, 0 )


evaluateQuestion : ProjectQuestionnaire -> List String -> Question -> ( Int, Int )
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
                        |> Maybe.map (Tuple.sum ( 0, Bool.toInt requiredNow ))
                        |> Maybe.withDefault ( Bool.toInt requiredNow, Bool.toInt requiredNow )

                ListQuestion commonData _ ->
                    let
                        itemUuids =
                            ReplyValue.getItemUuids value
                    in
                    if not (List.isEmpty itemUuids) then
                        itemUuids
                            |> List.map (evaluateAnswerItem questionnaire currentPath (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid questionnaire.knowledgeModel))
                            |> List.foldl Tuple.sum ( 0, Bool.toInt requiredNow )

                    else
                        ( Bool.toInt requiredNow, Bool.toInt requiredNow )

                _ ->
                    if ReplyValue.isEmpty value then
                        ( Bool.toInt requiredNow, Bool.toInt requiredNow )

                    else
                        ( 0, Bool.toInt requiredNow )

        Nothing ->
            ( Bool.toInt requiredNow, Bool.toInt requiredNow )


evaluateFollowups : ProjectQuestionnaire -> List String -> String -> ( Int, Int )
evaluateFollowups questionnaire path answerUuid =
    let
        currentPath =
            path ++ [ answerUuid ]
    in
    KnowledgeModel.getAnswerFollowupQuestions answerUuid questionnaire.knowledgeModel
        |> List.map (evaluateQuestion questionnaire currentPath)
        |> List.foldl Tuple.sum ( 0, 0 )


evaluateAnswerItem : ProjectQuestionnaire -> List String -> List Question -> String -> ( Int, Int )
evaluateAnswerItem questionnaire path questions uuid =
    let
        currentPath =
            path ++ [ uuid ]
    in
    questions
        |> List.map (evaluateQuestion questionnaire currentPath)
        |> List.foldl Tuple.sum ( 0, 0 )



-- Generating Replies


generateReplies : Time.Posix -> Seed -> String -> KnowledgeModel -> ProjectQuestionnaire -> ( Seed, Maybe String, ProjectQuestionnaire )
generateReplies currentTime seed questionUuid km questionnaireDetail =
    let
        parentMap =
            KnowledgeModel.createParentMap km

        ( newSeed, mbChapterUuid, replies ) =
            foldReplies currentTime km parentMap seed questionUuid Dict.empty

        cleanedReplies =
            cleanReplies km questionnaireDetail.replies

        newReplies =
            Dict.union replies cleanedReplies
    in
    ( newSeed
    , mbChapterUuid
    , { questionnaireDetail | replies = newReplies }
    )


cleanReplies : KnowledgeModel -> Dict String Reply -> Dict String Reply
cleanReplies km replies =
    let
        processChapter chapterUuid =
            KnowledgeModel.getChapterQuestions chapterUuid km
                |> List.map (processQuestion [ chapterUuid ])
                |> List.foldl Dict.union Dict.empty

        processQuestion path question =
            let
                questionPathKey =
                    pathToString (path ++ [ Question.getUuid question ])
            in
            case question of
                OptionsQuestion commonData _ ->
                    case Dict.get questionPathKey replies of
                        Just reply ->
                            case reply.value of
                                AnswerReply answerUuid ->
                                    case KnowledgeModel.getAnswer answerUuid km of
                                        Just answer ->
                                            KnowledgeModel.getAnswerFollowupQuestions answerUuid km
                                                |> List.map (processQuestion (path ++ [ commonData.uuid, answer.uuid ]))
                                                |> List.foldl Dict.union Dict.empty
                                                |> Dict.insert questionPathKey reply

                                        Nothing ->
                                            Dict.empty

                                _ ->
                                    Dict.empty

                        _ ->
                            Dict.empty

                ListQuestion commonData _ ->
                    case Dict.get questionPathKey replies of
                        Just reply ->
                            case reply.value of
                                ItemListReply itemUuids ->
                                    let
                                        processItem itemUuid =
                                            KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid km
                                                |> List.map (processQuestion (path ++ [ commonData.uuid, itemUuid ]))
                                                |> List.foldl Dict.union Dict.empty
                                    in
                                    List.map processItem itemUuids
                                        |> List.foldl Dict.union Dict.empty
                                        |> Dict.insert questionPathKey reply

                                _ ->
                                    Dict.empty

                        _ ->
                            Dict.empty

                _ ->
                    Dict.get questionPathKey replies
                        |> Maybe.unwrap Dict.empty (Dict.singleton questionPathKey)
    in
    KnowledgeModel.getChapters km
        |> List.map (processChapter << .uuid)
        |> List.foldl Dict.union Dict.empty


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
                    Uuid.stepString seed

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


updateContent : ProjectQuestionnaire -> ProjectContent -> ProjectQuestionnaire
updateContent detail content =
    { detail
        | replies = content.replies
        , phaseUuid = content.phaseUuid
        , labels = content.labels
    }
