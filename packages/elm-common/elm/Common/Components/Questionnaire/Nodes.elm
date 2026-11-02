module Common.Components.Questionnaire.Nodes exposing
    ( ChapterLinksNodeData
    , ChapterNodeData
    , ContentNode(..)
    , Context
    , IntegrationAnswering(..)
    , IntegrationPlugin
    , IntegrationPluginData
    , IntegrationQuestionNodeData
    , ItemEmptyNodeData
    , ItemFooterNodeData
    , ItemHeaderNodeData
    , ItemSelectQuestionNodeData
    , ItemsEndNodeData
    , MultiChoiceQuestionNodeData
    , NestingType(..)
    , OptionsQuestionNodeData
    , QuestionExtraCrossReference
    , QuestionExtraData
    , QuestionExtraResourceCollection
    , QuestionExtraResourcePage
    , QuestionExtraUrlReference
    , QuestionNodeData
    , QuestionSpecificNodeData(..)
    , ValueQuestionNodeData
    , build
    , depth
    , key
    , level
    )

{-| One chapter of a questionnaire as a flat list of nodes — the shape the
questionnaire renders, one keyed, lazy node each.

**Why flat.** A questionnaire can hold thousands of questions, follow-ups under
answers, lists of items with questions of their own and lists inside those. Drawn
as a tree, a change deep inside it would rebuild the view of every ancestor. Drawn
as a flat, keyed list, each node is its own `Html.Lazy` call whose arguments are
stable references or primitives, and a change re-renders the one node it touches.
The nesting is only drawn: `NestingType` says how deep a node sits, the view wraps
it in that many indentation boxes, and `level` states it for assistive technology
(`aria-level`).

**Built once, kept in the model.** `build` does every knowledge-model lookup and
every localization — questions, answers, choices, tags, references, experts,
phases, cross-reference chapter numbers, and how an integration question is
answered (by hints, or by which plugin) — so that nothing in the view touches the
knowledge model or looks anything up. It is run only when the structure can have changed
(`Common.Components.Questionnaire.Rebuild.needsRebuild`): a new chapter, a collapse, a
view setting, the phase, an answer, the items of a list, a cleared reply. Every
node it returns is a new record, so every lazy node misses once after a rebuild —
which is why rebuilding on anything else (a keystroke, a comment, a highlight)
would undo the whole design.

A node does not record whether a plugin panel is open for it (that is a flag
bit, so opening a panel does not copy every node), and `level` and `key` live
here, next to the structure they describe.

-}

import CharIdentifier
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Answer as Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Chapter as Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Choice as Choice exposing (Choice)
import Common.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Common.Api.Models.KnowledgeModel.Integration exposing (Integration(..))
import Common.Api.Models.KnowledgeModel.Metric as Metric exposing (Metric)
import Common.Api.Models.KnowledgeModel.Phase as Phase exposing (Phase)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.KnowledgeModel.Question.QuestionValidation exposing (QuestionValidation)
import Common.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Common.Api.Models.KnowledgeModel.ResourceCollection as ResourceCollection
import Common.Api.Models.KnowledgeModel.ResourcePage as ResourcePage
import Common.Api.Models.KnowledgeModel.Tag as Tag exposing (Tag)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Components.Questionnaire.Path as Path
import Common.Components.Questionnaire.ViewSettings exposing (ViewSettings)
import Common.Data.BuiltInIntegration as BuiltInIntegration exposing (BuiltInIntegration)
import Common.Plugins.Plugin exposing (KnowledgeModelIntegrationConnector, Plugin)
import Common.Plugins.PluginElement exposing (PluginElement)
import Dict
import Dict.Extra as Dict
import Flip exposing (flip)
import Gettext exposing (gettext)
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import String.Format as String
import Uuid


type ContentNode
    = ChapterNode ChapterNodeData
    | ChapterEmptyNode
    | ChapterLinksNode ChapterLinksNodeData
    | QuestionNode QuestionNodeData
    | ItemHeaderNode ItemHeaderNodeData
    | ItemEmptyNode ItemEmptyNodeData
    | ItemFooterNode ItemFooterNodeData
    | ItemsEndNode ItemsEndNodeData


type alias ChapterNodeData =
    { chapter : Chapter
    , chapterNumber : String
    }


type alias ChapterLinksNodeData =
    { chapterUuid : String
    , previousChapter : Maybe Chapter
    , nextChapter : Maybe Chapter
    }


type alias QuestionNodeData =
    { humanIdentifier : List String
    , isDesirable : Bool
    , question : Question
    , questionExtraData : QuestionExtraData
    , questionPath : String
    , questionSpecificData : QuestionSpecificNodeData
    , nestingType : NestingType
    , tags : List Tag
    }


type alias QuestionExtraData =
    { resourceCollections : List QuestionExtraResourceCollection
    , urlReferences : List QuestionExtraUrlReference
    , crossReferences : List QuestionExtraCrossReference
    , experts : List Expert
    , requiredPhase : Maybe Phase
    }


type alias QuestionExtraResourceCollection =
    { title : String
    , resourcePages : List QuestionExtraResourcePage
    }


type alias QuestionExtraResourcePage =
    { title : String
    , url : String
    }


type alias QuestionExtraUrlReference =
    { label : String
    , url : String
    }


type alias QuestionExtraCrossReference =
    { targetQuestionUuid : String
    , targetQuestionTitle : String
    , description : String
    }


type QuestionSpecificNodeData
    = OptionsQuestionSpecificNodeData OptionsQuestionNodeData
    | MultiChoiceQuestionSpecificNodeData MultiChoiceQuestionNodeData
    | ItemSelectQuestionSpecificNodeData ItemSelectQuestionNodeData
    | IntegrationQuestionSpecificNodeData IntegrationQuestionNodeData
    | ValueQuestionSpecificNodeData ValueQuestionNodeData
    | NoQuestionNodeData


type alias OptionsQuestionNodeData =
    { answers : List Answer
    , followUpsCollapsed : Bool
    , followUpsCount : Int
    , metrics : List Metric
    }


type alias MultiChoiceQuestionNodeData =
    { choices : List Choice
    }


type alias ItemSelectQuestionNodeData =
    { itemTemplateQuestions : List Question
    }


type alias IntegrationQuestionNodeData =
    { integration : Maybe Integration
    , answering : IntegrationAnswering
    }


{-| How an integration question is answered, decided here so that the view only
branches on it.

  - **`AnsweredFromHints`** — an API integration, or a plugin integration the
    client has built in (`Common.Data.BuiltInIntegration`).
  - **`AnsweredByPlugin`** — a plugin's element, with what it is given that does
    not change while the chapter is shown. The project does, so the view gives
    it on its own.
  - **`PluginMissing`** — a plugin integration whose plugin is not on.
  - **`NotConfigured`** — the question has no integration.

-}
type IntegrationAnswering
    = AnsweredFromHints
    | AnsweredByPlugin IntegrationPluginData
    | PluginMissing
    | NotConfigured


type alias IntegrationPluginData =
    { pluginElement : PluginElement
    , rendersReply : Bool
    , integrationSettings : String
    , settings : String
    , userSettings : String
    , user : String
    }


{-| A plugin that renders integration questions, with what its element is given,
read once from the app state.
-}
type alias IntegrationPlugin =
    { plugin : Plugin
    , connector : KnowledgeModelIntegrationConnector
    , settings : String
    , userSettings : String
    , user : String
    }


type alias ValueQuestionNodeData =
    { validations : List QuestionValidation }


type alias ItemHeaderNodeData =
    { itemPath : String
    , itemIndex : Int
    , itemUuid : String
    , parentQuestionPath : String
    , parentQuestionUuid : String
    , nestingType : NestingType
    , isCollapsed : Bool
    }


type alias ItemFooterNodeData =
    { itemPath : String
    , nestingType : NestingType
    }


type alias ItemEmptyNodeData =
    { itemPath : String
    , nestingType : NestingType
    , hiddenByViewOptions : Bool
    }


type alias ItemsEndNodeData =
    { questionPath : String
    , nestingType : NestingType
    }


type NestingType
    = ContentNesting
    | FollowUpNesting NestingType
    | ItemNesting NestingType


{-| What a chapter is built from. `resourcePageUrl` is the caller's, because
a resource page opens in the package's own context (a project, a KM editor).
-}
type alias Context =
    { chapterUuid : String
    , questionnaire : ProjectQuestionnaire
    , collapsedPaths : Set String
    , resourcePageUrl : String -> String
    , viewSettings : ViewSettings
    , locale : Gettext.Locale
    , knowledgeModelParentMap : KnowledgeModel.ParentMap
    , integrationPlugins : List IntegrationPlugin
    , builtInIntegrations : List BuiltInIntegration
    }


{-| The chapter `ctx.chapterUuid` as nodes, in reading order: the chapter,
a notice when nothing in it is shown, its questions with their follow-ups and
items, and the links to the chapters either side.
-}
build : Context -> List ContentNode
build ctx =
    case KnowledgeModel.getChapter ctx.chapterUuid ctx.questionnaire.knowledgeModel of
        Just chapter ->
            let
                chapterIndex =
                    KnowledgeModel.getChapters ctx.questionnaire.knowledgeModel
                        |> List.findIndex (.uuid >> (==) chapter.uuid)
                        |> Maybe.withDefault 0

                chapterNumber =
                    String.fromInt (chapterIndex + 1)

                chapterNode =
                    ChapterNode
                        { chapter = Chapter.localize ctx.locale chapter
                        , chapterNumber = chapterNumber
                        }

                questionNodes =
                    List.indexedMap (buildQuestion ctx identity [ ctx.chapterUuid ] [ chapterNumber ]) chapter.questionUuids
                        |> List.concat

                emptyChapterNodes =
                    if List.isEmpty questionNodes then
                        [ ChapterEmptyNode ]

                    else
                        []

                previousChapter =
                    ctx.questionnaire.knowledgeModel.chapterUuids
                        |> List.getAt (chapterIndex - 1)
                        |> Maybe.andThen (flip KnowledgeModel.getChapter ctx.questionnaire.knowledgeModel)

                nextChapter =
                    ctx.questionnaire.knowledgeModel.chapterUuids
                        |> List.getAt (chapterIndex + 1)
                        |> Maybe.andThen (flip KnowledgeModel.getChapter ctx.questionnaire.knowledgeModel)

                chapterLinksNode =
                    ChapterLinksNode
                        { chapterUuid = chapter.uuid
                        , previousChapter = Maybe.map (Chapter.localize ctx.locale) previousChapter
                        , nextChapter = Maybe.map (Chapter.localize ctx.locale) nextChapter
                        }
            in
            chapterNode :: emptyChapterNodes ++ questionNodes ++ [ chapterLinksNode ]

        Nothing ->
            []


buildQuestion : Context -> (NestingType -> NestingType) -> List String -> List String -> Int -> String -> List ContentNode
buildQuestion ctx createNestingType path humanIdentifier order questionUuid =
    case KnowledgeModel.getQuestion questionUuid ctx.questionnaire.knowledgeModel of
        Just question ->
            let
                questionnairePhaseUuid =
                    Uuid.toString (Maybe.withDefault Uuid.nil ctx.questionnaire.phaseUuid)

                isDesirable =
                    Question.isDesirable ctx.questionnaire.knowledgeModel.phaseUuids questionnairePhaseUuid question
            in
            if ctx.viewSettings.nonDesirableQuestions || isDesirable then
                let
                    questionPath =
                        Path.toString (path ++ [ questionUuid ])

                    questionHumanIdentifier =
                        humanIdentifier ++ [ String.fromInt (order + 1) ]

                    tags =
                        Question.getTagUuids question
                            |> List.filterMap (flip KnowledgeModel.getTag ctx.questionnaire.knowledgeModel)
                            |> List.map (Tag.localize ctx.locale)
                            |> List.sortBy .name

                    questionNode specific =
                        QuestionNode
                            { humanIdentifier = questionHumanIdentifier
                            , isDesirable = isDesirable
                            , question = Question.localize ctx.locale question
                            , questionExtraData = createQuestionExtraData ctx question
                            , questionPath = questionPath
                            , nestingType = createNestingType ContentNesting
                            , questionSpecificData = specific
                            , tags = tags
                            }

                    ( questionSpecificData, questionTypeNodes ) =
                        case question of
                            Question.OptionsQuestion _ _ ->
                                let
                                    answers =
                                        KnowledgeModel.getQuestionAnswers questionUuid ctx.questionnaire.knowledgeModel
                                            |> List.map (cleanFollowUpUuids >> Answer.localize ctx.locale)

                                    followUpExists followUpUuid =
                                        Dict.get followUpUuid ctx.questionnaire.knowledgeModel.entities.questions
                                            |> Maybe.isJust

                                    cleanFollowUpUuids answer =
                                        { answer | followUpUuids = List.filter followUpExists answer.followUpUuids }

                                    mbSelectedAnswerUuid =
                                        Dict.get questionPath ctx.questionnaire.replies
                                            |> Maybe.map (ReplyValue.getAnswerUuid << .value)

                                    mbSelectedAnswer =
                                        List.find ((==) mbSelectedAnswerUuid << Just << .uuid) answers

                                    selectedAnswerPath =
                                        case mbSelectedAnswerUuid of
                                            Just selectedAnswerUuid ->
                                                questionPath ++ "." ++ selectedAnswerUuid

                                            Nothing ->
                                                ""

                                    ( followUpsCount, followUpQuestions ) =
                                        case mbSelectedAnswer of
                                            Just selectedAnswer ->
                                                let
                                                    answerPath =
                                                        path ++ [ questionUuid, selectedAnswer.uuid ]

                                                    count =
                                                        selectedAnswer.followUpUuids
                                                            |> List.length
                                                in
                                                if Path.isCollapsed (Path.toString answerPath) ctx then
                                                    ( count, [] )

                                                else
                                                    let
                                                        answerHumanIdentifier =
                                                            List.elemIndex selectedAnswer answers
                                                                |> Maybe.withDefault 1
                                                                |> CharIdentifier.fromInt
                                                                |> List.singleton
                                                                |> (++) questionHumanIdentifier
                                                    in
                                                    ( count
                                                    , List.indexedMap (buildQuestion ctx (createNestingType << FollowUpNesting) (path ++ [ questionUuid, selectedAnswer.uuid ]) answerHumanIdentifier) selectedAnswer.followUpUuids
                                                        |> List.concat
                                                    )

                                            Nothing ->
                                                ( 0, [] )
                                in
                                ( OptionsQuestionSpecificNodeData
                                    { answers = answers
                                    , followUpsCount = followUpsCount
                                    , followUpsCollapsed = Path.isCollapsed selectedAnswerPath ctx
                                    , metrics = List.map (Metric.localize ctx.locale) (KnowledgeModel.getMetrics ctx.questionnaire.knowledgeModel)
                                    }
                                , followUpQuestions
                                )

                            Question.ListQuestion _ _ ->
                                let
                                    itemUuids =
                                        Dict.get questionPath ctx.questionnaire.replies
                                            |> Maybe.unwrap [] (ReplyValue.getItemUuids << .value)

                                    items =
                                        List.indexedMap (buildItem ctx createNestingType (path ++ [ questionUuid ]) questionHumanIdentifier questionUuid) itemUuids
                                            |> List.concat

                                    itemAddNode =
                                        ItemsEndNode
                                            { questionPath = questionPath
                                            , nestingType = createNestingType ContentNesting
                                            }
                                in
                                ( NoQuestionNodeData
                                , items ++ [ itemAddNode ]
                                )

                            Question.MultiChoiceQuestion _ _ ->
                                let
                                    choices =
                                        KnowledgeModel.getQuestionChoices questionUuid ctx.questionnaire.knowledgeModel
                                            |> List.map (Choice.localize ctx.locale)
                                in
                                ( MultiChoiceQuestionSpecificNodeData { choices = choices }
                                , []
                                )

                            Question.ItemSelectQuestion _ itemSelectQuestionData ->
                                let
                                    itemTemplateQuestions =
                                        (case itemSelectQuestionData.listQuestionUuid of
                                            Just listQuestionUuid ->
                                                KnowledgeModel.getQuestionItemTemplateQuestions listQuestionUuid ctx.questionnaire.knowledgeModel

                                            Nothing ->
                                                []
                                        )
                                            |> List.map (Question.localize ctx.locale)
                                in
                                ( ItemSelectQuestionSpecificNodeData { itemTemplateQuestions = itemTemplateQuestions }
                                , []
                                )

                            Question.IntegrationQuestion _ integrationQuestionData ->
                                let
                                    integration =
                                        KnowledgeModel.getIntegration integrationQuestionData.integrationUuid ctx.questionnaire.knowledgeModel
                                in
                                ( IntegrationQuestionSpecificNodeData
                                    { integration = integration
                                    , answering = integrationAnswering ctx integration
                                    }
                                , []
                                )

                            Question.ValueQuestion _ _ ->
                                ( ValueQuestionSpecificNodeData { validations = Question.getAppliedValidations question }
                                , []
                                )

                            _ ->
                                ( NoQuestionNodeData, [] )
                in
                questionNode questionSpecificData :: questionTypeNodes

            else
                []

        Nothing ->
            []


createQuestionExtraData : Context -> Question -> QuestionExtraData
createQuestionExtraData ctx question =
    let
        ( resourcePageReferencesData, urlReferencesData, crossReferencesData ) =
            List.foldl
                (\r ( rpr, ur, cr ) ->
                    case r of
                        ResourcePageReference data ->
                            ( rpr ++ [ data ], ur, cr )

                        URLReference data ->
                            ( rpr, ur ++ [ data ], cr )

                        CrossReference data ->
                            ( rpr, ur, cr ++ [ data ] )
                )
                ( [], [], [] )
                (KnowledgeModel.getQuestionReferences (Question.getUuid question) ctx.questionnaire.knowledgeModel
                    |> List.map (Reference.localize ctx.locale)
                )

        toResourceCollection ( resourceCollectionUuid, collectionResourcePageReferences ) =
            let
                resourceCollection =
                    KnowledgeModel.getResourceCollection resourceCollectionUuid ctx.questionnaire.knowledgeModel
                        |> Maybe.map (ResourceCollection.localize ctx.locale)
            in
            case resourceCollection of
                Just rc ->
                    Just <|
                        { title = rc.title
                        , resourcePages =
                            List.filterMap
                                (\resourcePageReference ->
                                    let
                                        mbResourcePage =
                                            resourcePageReference.resourcePageUuid
                                                |> Maybe.andThen (flip KnowledgeModel.getResourcePage ctx.questionnaire.knowledgeModel)
                                                |> Maybe.map (ResourcePage.localize ctx.locale)
                                    in
                                    case mbResourcePage of
                                        Just resourcePage ->
                                            Just
                                                { title = resourcePage.title
                                                , url = ctx.resourcePageUrl resourcePage.uuid
                                                }

                                        Nothing ->
                                            Nothing
                                )
                                collectionResourcePageReferences
                        }

                Nothing ->
                    Nothing

        resource =
            Dict.filterGroupBy
                (Maybe.andThen (flip KnowledgeModel.getResourceCollectionUuidByResourcePageUuid ctx.questionnaire.knowledgeModel) << .resourcePageUuid)
                resourcePageReferencesData

        resourceCollections =
            Dict.toList resource
                |> List.filterMap toResourceCollection
                |> List.sortBy .title

        urlReferences =
            List.map
                (\data ->
                    { label = data.label
                    , url = data.url
                    }
                )
                urlReferencesData

        crossReferences =
            List.filterMap
                (\data ->
                    KnowledgeModel.getQuestion data.targetUuid ctx.questionnaire.knowledgeModel
                        |> Maybe.map
                            (\targetQuestion ->
                                { targetQuestionUuid = data.targetUuid
                                , targetQuestionTitle = crossReferenceTitle ctx data.targetUuid (Question.getTitle (Question.localize ctx.locale targetQuestion))
                                , description = data.description
                                }
                            )
                )
                crossReferencesData
    in
    { resourceCollections = resourceCollections
    , urlReferences = urlReferences
    , crossReferences = crossReferences
    , experts = KnowledgeModel.getQuestionExperts (Question.getUuid question) ctx.questionnaire.knowledgeModel
    , requiredPhase =
        Question.getRequiredPhaseUuid question
            |> Maybe.andThen (flip KnowledgeModel.getPhase ctx.questionnaire.knowledgeModel)
            |> Maybe.map (Phase.localize ctx.locale)
    }


{-| Prefix a cross-referenced question title with the number of the chapter it
belongs to, e.g. "Ch. 1: Question title".
-}
crossReferenceTitle : Context -> String -> String -> String
crossReferenceTitle ctx targetQuestionUuid title =
    case getQuestionChapterNumber ctx.knowledgeModelParentMap targetQuestionUuid ctx.questionnaire.knowledgeModel of
        Just chapterNumber ->
            String.format (gettext "Ch. %s: " ctx.locale) [ String.fromInt chapterNumber ] ++ title

        Nothing ->
            title


getQuestionChapterNumber : KnowledgeModel.ParentMap -> String -> KnowledgeModel -> Maybe Int
getQuestionChapterNumber parentMap questionUuid km =
    getQuestionChapter parentMap questionUuid km
        |> Maybe.andThen (\chapter -> List.findIndex ((==) chapter.uuid << .uuid) (KnowledgeModel.getChapters km))
        |> Maybe.map ((+) 1)


getQuestionChapter : KnowledgeModel.ParentMap -> String -> KnowledgeModel -> Maybe Chapter
getQuestionChapter parentMap questionUuid km =
    let
        parentUuid =
            KnowledgeModel.getParent parentMap questionUuid
    in
    if parentUuid == Uuid.toString Uuid.nil then
        Nothing

    else
        case KnowledgeModel.getChapter parentUuid km of
            Just chapter ->
                Just chapter

            Nothing ->
                getQuestionChapter parentMap parentUuid km


integrationAnswering : Context -> Maybe Integration -> IntegrationAnswering
integrationAnswering ctx mbIntegration =
    case mbIntegration of
        Just (ApiIntegration _) ->
            AnsweredFromHints

        Just (PluginIntegration data) ->
            if Maybe.isJust (BuiltInIntegration.find ctx.builtInIntegrations data) then
                AnsweredFromHints

            else
                case List.find (\p -> Uuid.toString p.plugin.uuid == data.pluginUuid && p.connector.integrationId == data.pluginIntegrationId) ctx.integrationPlugins of
                    Just plugin ->
                        AnsweredByPlugin
                            { pluginElement = plugin.connector.questionnaireElement
                            , rendersReply = plugin.connector.rendersReply
                            , integrationSettings = data.pluginIntegrationSettings
                            , settings = plugin.settings
                            , userSettings = plugin.userSettings
                            , user = plugin.user
                            }

                    Nothing ->
                        PluginMissing

        Nothing ->
            NotConfigured


buildItem : Context -> (NestingType -> NestingType) -> List String -> List String -> String -> Int -> String -> List ContentNode
buildItem ctx createNestingType path humanIdentifier parentQuestionUuid itemIndex itemUuid =
    let
        itemPath =
            Path.toString (path ++ [ itemUuid ])

        itemIsCollapsed =
            Path.isCollapsed itemPath ctx

        itemHeaderNode =
            ItemHeaderNode
                { itemPath = itemPath
                , itemUuid = itemUuid
                , itemIndex = itemIndex
                , parentQuestionPath = Path.toString path
                , parentQuestionUuid = parentQuestionUuid
                , nestingType = createNestingType ContentNesting
                , isCollapsed = itemIsCollapsed
                }
    in
    if itemIsCollapsed then
        [ itemHeaderNode ]

    else
        let
            itemHumanIdentifier =
                humanIdentifier ++ [ CharIdentifier.fromInt itemIndex ]

            itemFooterNode =
                ItemFooterNode
                    { itemPath = itemPath
                    , nestingType = createNestingType ContentNesting
                    }

            questions =
                KnowledgeModel.getQuestionItemTemplateQuestions parentQuestionUuid ctx.questionnaire.knowledgeModel
                    |> List.map Question.getUuid

            questionNodes =
                List.indexedMap (buildQuestion ctx (createNestingType << ItemNesting) (path ++ [ itemUuid ]) itemHumanIdentifier) questions
                    |> List.concat

            itemContentNodes =
                if List.isEmpty questionNodes then
                    -- The item shows no questions. It either genuinely has none (or
                    -- they were filtered out by question tags, which looks the same) or
                    -- every question is currently hidden by the view options. Surface a
                    -- flash so an empty item does not look broken.
                    [ ItemEmptyNode
                        { itemPath = itemPath
                        , nestingType = createNestingType (ItemNesting ContentNesting)
                        , hiddenByViewOptions = not (List.isEmpty questions)
                        }
                    ]

                else
                    questionNodes
        in
        itemHeaderNode :: itemContentNodes ++ [ itemFooterNode ]


{-| How many levels deep a nesting is: 1 in the chapter, one more under an
answer, two more inside an item — the item is a level of its own, and its
questions sit under it.
-}
depth : NestingType -> Int
depth nestingType =
    case nestingType of
        ContentNesting ->
            1

        FollowUpNesting parent ->
            depth parent + 1

        ItemNesting parent ->
            depth parent + 2


{-| The node's `aria-level` in the questionnaire's list, or `Nothing` for the
nodes that are not in the list — the chapter heading before it, the chapter's
links after it, and the empty-chapter notice.

A question is at its own depth. An item — its header, its footer — and the list's
"Add item" end are one level under the list question they belong to, the
item's level. The empty-item notice stands where the item's questions would, one
level further.

-}
level : ContentNode -> Maybe Int
level node =
    case node of
        QuestionNode data ->
            Just (depth data.nestingType)

        ItemHeaderNode data ->
            Just (depth data.nestingType + 1)

        ItemFooterNode data ->
            Just (depth data.nestingType + 1)

        ItemsEndNode data ->
            Just (depth data.nestingType + 1)

        ItemEmptyNode data ->
            Just (depth data.nestingType)

        ChapterNode _ ->
            Nothing

        ChapterEmptyNode ->
            Nothing

        ChapterLinksNode _ ->
            Nothing


{-| The node's key in the keyed list: unique across every kind of node, because
a question's path is also the prefix of its items' paths.
-}
key : ContentNode -> String
key node =
    case node of
        ChapterNode data ->
            data.chapter.uuid

        ChapterEmptyNode ->
            "chapter-empty"

        ChapterLinksNode data ->
            "chapter-links-" ++ data.chapterUuid

        QuestionNode data ->
            data.questionPath

        ItemHeaderNode data ->
            data.itemPath ++ "-header"

        ItemEmptyNode data ->
            data.itemPath ++ "-empty"

        ItemFooterNode data ->
            data.itemPath ++ "-footer"

        ItemsEndNode data ->
            data.questionPath ++ "-items-end"
