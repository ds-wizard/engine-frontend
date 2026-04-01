module Wizard.Components.Questionnaire2.QuestionnaireVirtualization exposing
    ( ChapterLinksNodeData
    , ChapterNodeData
    , ContentNode(..)
    , IntegrationQuestionNodeData
    , ItemFooterNodeData
    , ItemHeaderNodeData
    , ItemSelectQuestionNodedata
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
    , VirtualizeContext
    , clearPluginOpen
    , needVirtualization
    , setPluginOpen
    , virtualizeChapter
    )

import CharIdentifier
import Dict
import Dict.Extra as Dict
import Flip exposing (flip)
import List.Extra as List
import Maybe.Extra as Maybe
import Roman
import Set exposing (Set)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValidation exposing (QuestionValidation)
import Wizard.Api.Models.KnowledgeModel.Reference exposing (Reference(..))
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Components.Questionnaire2.QuestionnaireUtils exposing (isPathCollapsed, pathToString)
import Wizard.Plugins.Plugin exposing (ProjectQuestionActionConnector)
import Wizard.Routes
import Wizard.Routing as Routing


type ContentNode
    = ChapterNode ChapterNodeData
    | ChapterEmptyNode
    | ChapterLinksNode ChapterLinksNodeData
    | QuestionNode QuestionNodeData
    | ItemHeaderNode ItemHeaderNodeData
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
    , pluginOpen : Maybe ( Uuid, ProjectQuestionActionConnector )
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
    | ItemSelectQuestionSpecificNodeData ItemSelectQuestionNodedata
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


type alias ItemSelectQuestionNodedata =
    { itemTemplateQuestions : List Question
    }


type alias IntegrationQuestionNodeData =
    { integration : Maybe Integration
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


type alias ItemsEndNodeData =
    { questionPath : String
    , nestingType : NestingType
    }


type NestingType
    = ContentNesting
    | FollowUpNesting NestingType
    | ItemNesting NestingType


type alias VirtualizeContext =
    { chapterUuid : String
    , questionnaire : ProjectQuestionnaire
    , collapsedPaths : Set String
    , resourcePageToUrl : String -> Wizard.Routes.Route
    , viewSettings : QuestionnaireViewSettings
    }


setPluginOpen : String -> ( Uuid, ProjectQuestionActionConnector ) -> List ContentNode -> List ContentNode
setPluginOpen questionUuid pluginOpenValue =
    List.map
        (\contentNode ->
            case contentNode of
                QuestionNode questionNodeData ->
                    if Question.getUuid questionNodeData.question == questionUuid then
                        QuestionNode { questionNodeData | pluginOpen = Just pluginOpenValue }

                    else
                        QuestionNode { questionNodeData | pluginOpen = Nothing }

                _ ->
                    contentNode
        )


clearPluginOpen : List ContentNode -> List ContentNode
clearPluginOpen =
    List.map
        (\contentNode ->
            case contentNode of
                QuestionNode questionNodeData ->
                    QuestionNode { questionNodeData | pluginOpen = Nothing }

                _ ->
                    contentNode
        )


virtualizeChapter : VirtualizeContext -> List ContentNode
virtualizeChapter ctx =
    case KnowledgeModel.getChapter ctx.chapterUuid ctx.questionnaire.knowledgeModel of
        Just chapter ->
            let
                chapterIndex =
                    KnowledgeModel.getChapters ctx.questionnaire.knowledgeModel
                        |> List.findIndex (.uuid >> (==) chapter.uuid)
                        |> Maybe.withDefault 0

                chapterNumber =
                    Roman.toRomanNumber (chapterIndex + 1)

                chapterNode =
                    ChapterNode
                        { chapter = chapter
                        , chapterNumber = chapterNumber
                        }

                questionNodes =
                    List.indexedMap (virtualizeQuestion ctx identity [ ctx.chapterUuid ] [ chapterNumber ]) chapter.questionUuids
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
                        , previousChapter = previousChapter
                        , nextChapter = nextChapter
                        }
            in
            chapterNode :: emptyChapterNodes ++ questionNodes ++ [ chapterLinksNode ]

        Nothing ->
            []


virtualizeQuestion : VirtualizeContext -> (NestingType -> NestingType) -> List String -> List String -> Int -> String -> List ContentNode
virtualizeQuestion ctx createNestingType path humanIdentifier order questionUuid =
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
                        pathToString (path ++ [ questionUuid ])

                    questionHumanIdentifier =
                        humanIdentifier ++ [ String.fromInt (order + 1) ]

                    tags =
                        Question.getTagUuids question
                            |> List.filterMap (flip KnowledgeModel.getTag ctx.questionnaire.knowledgeModel)
                            |> List.sortBy .name

                    questionNode specific =
                        QuestionNode
                            { humanIdentifier = questionHumanIdentifier
                            , isDesirable = isDesirable
                            , pluginOpen = Nothing
                            , question = question
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
                                            |> List.map cleanFollowUpUuids

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
                                                if isPathCollapsed (pathToString answerPath) ctx then
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
                                                    , List.indexedMap (virtualizeQuestion ctx (createNestingType << FollowUpNesting) (path ++ [ questionUuid, selectedAnswer.uuid ]) answerHumanIdentifier) selectedAnswer.followUpUuids
                                                        |> List.concat
                                                    )

                                            Nothing ->
                                                ( 0, [] )
                                in
                                ( OptionsQuestionSpecificNodeData
                                    { answers = answers
                                    , followUpsCount = followUpsCount
                                    , followUpsCollapsed = isPathCollapsed selectedAnswerPath ctx
                                    , metrics = KnowledgeModel.getMetrics ctx.questionnaire.knowledgeModel
                                    }
                                , followUpQuestions
                                )

                            Question.ListQuestion _ _ ->
                                let
                                    itemUuids =
                                        Dict.get questionPath ctx.questionnaire.replies
                                            |> Maybe.unwrap [] (ReplyValue.getItemUuids << .value)

                                    items =
                                        List.indexedMap (virtualizeItem ctx createNestingType (path ++ [ questionUuid ]) questionHumanIdentifier questionUuid) itemUuids
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
                                in
                                ( MultiChoiceQuestionSpecificNodeData { choices = choices }
                                , []
                                )

                            Question.ItemSelectQuestion _ itemSelectQuestionData ->
                                let
                                    itemTemplateQuestions =
                                        case itemSelectQuestionData.listQuestionUuid of
                                            Just listQuestionUuid ->
                                                KnowledgeModel.getQuestionItemTemplateQuestions listQuestionUuid ctx.questionnaire.knowledgeModel

                                            Nothing ->
                                                []
                                in
                                ( ItemSelectQuestionSpecificNodeData { itemTemplateQuestions = itemTemplateQuestions }
                                , []
                                )

                            Question.IntegrationQuestion _ integrationQuestionData ->
                                let
                                    integration =
                                        KnowledgeModel.getIntegration integrationQuestionData.integrationUuid ctx.questionnaire.knowledgeModel
                                in
                                ( IntegrationQuestionSpecificNodeData { integration = integration }
                                , []
                                )

                            Question.ValueQuestion _ valueQuestionData ->
                                ( ValueQuestionSpecificNodeData { validations = valueQuestionData.validations }
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


createQuestionExtraData : VirtualizeContext -> Question -> QuestionExtraData
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
                (KnowledgeModel.getQuestionReferences (Question.getUuid question) ctx.questionnaire.knowledgeModel)

        toResourceCollection ( resourceCollectionUuid, collectionResourcePageReferences ) =
            let
                resourceCollection =
                    KnowledgeModel.getResourceCollection resourceCollectionUuid ctx.questionnaire.knowledgeModel
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
                                    in
                                    case mbResourcePage of
                                        Just resourcePage ->
                                            Just
                                                { title = resourcePage.title
                                                , url = Routing.toUrl (ctx.resourcePageToUrl resourcePage.uuid)
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
            List.map
                (\data ->
                    { targetQuestionUuid = data.targetUuid
                    , targetQuestionTitle =
                        KnowledgeModel.getQuestion data.targetUuid ctx.questionnaire.knowledgeModel
                            |> Maybe.unwrap "" Question.getTitle
                    , description = data.description
                    }
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
    }


virtualizeItem : VirtualizeContext -> (NestingType -> NestingType) -> List String -> List String -> String -> Int -> String -> List ContentNode
virtualizeItem ctx createNestingType path humanIdentifier parentQuestionUuid itemIndex itemUuid =
    let
        itemPath =
            pathToString (path ++ [ itemUuid ])

        itemIsCollapsed =
            isPathCollapsed itemPath ctx

        itemHeaderNode =
            ItemHeaderNode
                { itemPath = itemPath
                , itemUuid = itemUuid
                , itemIndex = itemIndex
                , parentQuestionPath = pathToString path
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
                List.indexedMap (virtualizeQuestion ctx (createNestingType << ItemNesting) (path ++ [ itemUuid ]) itemHumanIdentifier) questions
                    |> List.concat
        in
        itemHeaderNode :: questionNodes ++ [ itemFooterNode ]


needVirtualization : ProjectEvent -> Bool
needVirtualization projectEvent =
    case projectEvent of
        ProjectEvent.SetReply reply ->
            case reply.value of
                ReplyValue.AnswerReply _ ->
                    True

                ReplyValue.ItemListReply _ ->
                    True

                _ ->
                    False

        ProjectEvent.ClearReply _ ->
            True

        _ ->
            False
