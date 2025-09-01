module Wizard.Pages.KMEditor.Migration.View.DiffTree exposing (view)

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Shared.Components.FontAwesome exposing (faKmAnswer, faKmChapter, faKmChoice, faKmExpert, faKmIntegration, faKmKnowledgeModel, faKmMetric, faKmPhase, faKmQuestion, faKmReference, faKmResourceCollection, faKmResourcePage, faKmTag)
import Wizard.Api.Models.Event as Event exposing (Event(..))
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.KnowledgeModel.Reference as Reference


view : String -> KnowledgeModel -> Event -> Html msg
view kmName km event =
    div [ class "diff-tree" ]
        [ ul [] [ viewEvent kmName km event ] ]


viewEvent : String -> KnowledgeModel -> Event -> Html msg
viewEvent kmName km event =
    let
        parentMap =
            KnowledgeModel.createParentMap km

        getParent =
            KnowledgeModel.getParent parentMap

        eventEntityName =
            eventEntityNameOrDefault Nothing

        eventEntityNameOrDefault value =
            Event.getEntityVisibleName event
                |> Maybe.orElse value

        getChapterTitle commonData =
            Maybe.map .title <| KnowledgeModel.getChapter commonData.entityUuid km

        getMetricTitle commonData =
            Maybe.map .title <| KnowledgeModel.getMetric commonData.entityUuid km

        getPhaseTitle commonData =
            Maybe.map .title <| KnowledgeModel.getPhase commonData.entityUuid km

        getTagName commonData =
            Maybe.map .name <| KnowledgeModel.getTag commonData.entityUuid km

        getIntegrationName commonData =
            Maybe.map Integration.getVisibleName <| KnowledgeModel.getIntegration commonData.entityUuid km

        getQuestionTitle commonData =
            Maybe.map Question.getTitle <| KnowledgeModel.getQuestion commonData.entityUuid km

        getAnswerLabel commonData =
            Maybe.map .label <| KnowledgeModel.getAnswer commonData.entityUuid km

        getChoiceLabel commonData =
            Maybe.map .label <| KnowledgeModel.getChoice commonData.entityUuid km

        getReferenceName commonData =
            Maybe.map (Reference.getVisibleName (KnowledgeModel.getAllResourcePages km)) <| KnowledgeModel.getReference commonData.entityUuid km

        getExpertName commonData =
            Maybe.map .name <| KnowledgeModel.getExpert commonData.entityUuid km

        getResourceCollectionName commonData =
            Maybe.map .title <| KnowledgeModel.getResourceCollection commonData.entityUuid km

        getResourcePageName commonData =
            Maybe.map .title <| KnowledgeModel.getResourcePage commonData.entityUuid km

        viewKnolwedgeModelNode_ =
            viewKnowledgeModelNode Nothing

        viewChapterNode_ =
            viewChapterNode kmName Nothing

        viewMetricNode_ =
            viewMetricNode kmName

        viewPhaseNode_ =
            viewPhaseNode kmName

        viewTagNode_ =
            viewTagNode kmName

        viewIntegrationNode_ =
            viewIntegrationNode kmName

        viewQuestionNode_ =
            viewQuestionNode kmName km getParent Nothing

        viewAnswerNode_ =
            viewAnswerNode kmName km getParent Nothing

        viewChoiceNode_ =
            viewChoiceNode kmName km getParent

        viewReferenceNode_ =
            viewReferenceNode kmName km getParent

        viewExpertNode_ =
            viewExpertNode kmName km getParent

        viewResourceCollectionNode_ =
            viewResourceCollectionNode kmName Nothing

        viewResourcePageNode_ =
            viewResourcePageNode kmName km
    in
    case event of
        AddKnowledgeModelEvent _ _ ->
            viewKnolwedgeModelNode_ stateClass.added eventEntityName

        EditKnowledgeModelEvent _ _ ->
            viewKnolwedgeModelNode_ stateClass.edited (eventEntityNameOrDefault (Just kmName))

        AddChapterEvent _ _ ->
            viewChapterNode_ stateClass.added eventEntityName

        EditChapterEvent _ commonData ->
            viewChapterNode_ stateClass.edited (eventEntityNameOrDefault (getChapterTitle commonData))

        DeleteChapterEvent commonData ->
            viewChapterNode_ stateClass.deleted (getChapterTitle commonData)

        AddMetricEvent _ _ ->
            viewMetricNode_ stateClass.added eventEntityName

        EditMetricEvent _ commonData ->
            viewMetricNode_ stateClass.edited (eventEntityNameOrDefault (getMetricTitle commonData))

        DeleteMetricEvent commonData ->
            viewMetricNode_ stateClass.deleted (getMetricTitle commonData)

        AddPhaseEvent _ _ ->
            viewPhaseNode_ stateClass.added eventEntityName

        EditPhaseEvent _ commonData ->
            viewMetricNode_ stateClass.edited (eventEntityNameOrDefault (getPhaseTitle commonData))

        DeletePhaseEvent commonData ->
            viewPhaseNode_ stateClass.deleted (getPhaseTitle commonData)

        AddTagEvent _ _ ->
            viewTagNode_ stateClass.added eventEntityName

        EditTagEvent _ commonData ->
            viewTagNode_ stateClass.edited (eventEntityNameOrDefault (getTagName commonData))

        DeleteTagEvent commonData ->
            viewTagNode_ stateClass.deleted (getTagName commonData)

        AddIntegrationEvent _ _ ->
            viewIntegrationNode_ stateClass.added eventEntityName

        EditIntegrationEvent _ commonData ->
            viewIntegrationNode_ stateClass.edited (eventEntityNameOrDefault (getIntegrationName commonData))

        DeleteIntegrationEvent commonData ->
            viewIntegrationNode_ stateClass.deleted (getIntegrationName commonData)

        AddQuestionEvent _ commonData ->
            viewQuestionNode_ stateClass.added eventEntityName commonData.parentUuid

        EditQuestionEvent _ commonData ->
            viewQuestionNode_ stateClass.edited (eventEntityNameOrDefault (getQuestionTitle commonData)) commonData.parentUuid

        DeleteQuestionEvent commonData ->
            viewQuestionNode_ stateClass.deleted (getQuestionTitle commonData) commonData.parentUuid

        AddAnswerEvent _ commonData ->
            viewAnswerNode_ stateClass.added eventEntityName commonData.parentUuid

        EditAnswerEvent _ commonData ->
            viewAnswerNode_ stateClass.edited (eventEntityNameOrDefault (getAnswerLabel commonData)) commonData.parentUuid

        DeleteAnswerEvent commonData ->
            viewAnswerNode_ stateClass.deleted (getAnswerLabel commonData) commonData.parentUuid

        AddChoiceEvent _ commonData ->
            viewChoiceNode_ stateClass.added eventEntityName commonData.parentUuid

        EditChoiceEvent _ commonData ->
            viewChoiceNode_ stateClass.edited (eventEntityNameOrDefault (getChoiceLabel commonData)) commonData.parentUuid

        DeleteChoiceEvent commonData ->
            viewChoiceNode_ stateClass.deleted (getChoiceLabel commonData) commonData.parentUuid

        AddReferenceEvent _ commonData ->
            viewReferenceNode_ stateClass.added eventEntityName commonData.parentUuid

        EditReferenceEvent _ commonData ->
            viewReferenceNode_ stateClass.edited (eventEntityNameOrDefault (getReferenceName commonData)) commonData.parentUuid

        DeleteReferenceEvent commonData ->
            viewReferenceNode_ stateClass.deleted (getReferenceName commonData) commonData.parentUuid

        AddExpertEvent _ commonData ->
            viewExpertNode_ stateClass.added eventEntityName commonData.parentUuid

        EditExpertEvent _ commonData ->
            viewExpertNode_ stateClass.edited (eventEntityNameOrDefault (getExpertName commonData)) commonData.parentUuid

        DeleteExpertEvent commonData ->
            viewExpertNode_ stateClass.deleted (getExpertName commonData) commonData.parentUuid

        AddResourceCollectionEvent _ _ ->
            viewResourceCollectionNode_ stateClass.added eventEntityName

        EditResourceCollectionEvent _ _ ->
            viewResourceCollectionNode_ stateClass.edited eventEntityName

        DeleteResourceCollectionEvent commonData ->
            viewResourceCollectionNode_ stateClass.deleted (getResourceCollectionName commonData)

        AddResourcePageEvent _ commonData ->
            viewResourcePageNode_ stateClass.added eventEntityName commonData.parentUuid

        EditResourcePageEvent _ commonData ->
            viewResourcePageNode_ stateClass.edited (eventEntityNameOrDefault (getResourcePageName commonData)) commonData.parentUuid

        DeleteResourcePageEvent commonData ->
            viewResourcePageNode_ stateClass.deleted (getResourcePageName commonData) commonData.parentUuid

        MoveQuestionEvent eventData commonData ->
            div []
                [ viewQuestionNode_ stateClass.deleted (getQuestionTitle commonData) commonData.parentUuid
                , viewQuestionNode_ stateClass.added (getQuestionTitle commonData) eventData.targetUuid
                ]

        MoveAnswerEvent eventData commonData ->
            div []
                [ viewAnswerNode_ stateClass.deleted (getAnswerLabel commonData) commonData.parentUuid
                , viewAnswerNode_ stateClass.added (getAnswerLabel commonData) eventData.targetUuid
                ]

        MoveChoiceEvent eventData commonData ->
            div []
                [ viewChoiceNode_ stateClass.deleted (getChoiceLabel commonData) commonData.parentUuid
                , viewChoiceNode_ stateClass.added (getChoiceLabel commonData) eventData.targetUuid
                ]

        MoveReferenceEvent eventData commonData ->
            div []
                [ viewReferenceNode_ stateClass.deleted (getReferenceName commonData) commonData.parentUuid
                , viewReferenceNode_ stateClass.added (getReferenceName commonData) eventData.targetUuid
                ]

        MoveExpertEvent eventData commonData ->
            div []
                [ viewExpertNode_ stateClass.deleted (getExpertName commonData) commonData.parentUuid
                , viewExpertNode_ stateClass.added (getExpertName commonData) eventData.targetUuid
                ]



-- Node views


viewKnowledgeModelNode :
    Maybe (Html msg)
    -> String
    -> Maybe String
    -> Html msg
viewKnowledgeModelNode mbChildNode cssClass mbTitle =
    viewNode faKmKnowledgeModel cssClass mbTitle mbChildNode


viewChapterNode :
    String
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> Html msg
viewChapterNode kmName mbChildNode cssClass mbTitle =
    let
        chapterNode =
            viewNode faKmChapter cssClass mbTitle mbChildNode
    in
    viewKnowledgeModelNode (Just chapterNode) stateClass.none (Just kmName)


viewMetricNode :
    String
    -> String
    -> Maybe String
    -> Html msg
viewMetricNode kmName cssClass mbTitle =
    let
        metricNode =
            viewNode faKmMetric cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode (Just metricNode) stateClass.none (Just kmName)


viewPhaseNode :
    String
    -> String
    -> Maybe String
    -> Html msg
viewPhaseNode kmName cssClass mbTitle =
    let
        phaseNode =
            viewNode faKmPhase cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode (Just phaseNode) stateClass.none (Just kmName)


viewTagNode :
    String
    -> String
    -> Maybe String
    -> Html msg
viewTagNode kmName cssClass mbTitle =
    let
        tagNode =
            viewNode faKmTag cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode (Just tagNode) stateClass.none (Just kmName)


viewIntegrationNode :
    String
    -> String
    -> Maybe String
    -> Html msg
viewIntegrationNode kmName cssClass mbTitle =
    let
        integrationNode =
            viewNode faKmIntegration cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode (Just integrationNode) stateClass.none (Just kmName)


viewQuestionNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewQuestionNode kmName km getParent mbChildNode cssClass mbTitle parentUuid =
    let
        questionNode =
            viewNode faKmQuestion cssClass mbTitle mbChildNode

        parentChapter =
            getParentChapterNode kmName km parentUuid questionNode

        parentQuestion =
            getParentQuestionNode kmName km getParent parentUuid questionNode

        parentAnswer =
            getParentAnswerNode kmName km getParent parentUuid questionNode
    in
    parentChapter
        |> Maybe.orElseLazy (\_ -> parentQuestion)
        |> Maybe.orElseLazy (\_ -> parentAnswer)
        |> Maybe.withDefault questionNode


viewAnswerNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewAnswerNode kmName km getParent mbChildNode cssClass mbTitle parentUuid =
    let
        answerNode =
            viewNode faKmAnswer cssClass mbTitle mbChildNode

        parentQuestion =
            getParentQuestionNode kmName km getParent parentUuid answerNode
    in
    parentQuestion
        |> Maybe.withDefault answerNode


viewChoiceNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewChoiceNode kmName km getParent cssClass mbTitle parentUuid =
    let
        choiceNode =
            viewNode faKmChoice cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode kmName km getParent parentUuid choiceNode
    in
    parentQuestion
        |> Maybe.withDefault choiceNode


viewReferenceNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewReferenceNode kmName km getParent cssClass mbTitle parentUuid =
    let
        referenceNode =
            viewNode faKmReference cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode kmName km getParent parentUuid referenceNode
    in
    parentQuestion
        |> Maybe.withDefault referenceNode


viewExpertNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewExpertNode kmName km getParent cssClass mbTitle parentUuid =
    let
        expertNode =
            viewNode faKmExpert cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode kmName km getParent parentUuid expertNode
    in
    parentQuestion
        |> Maybe.withDefault expertNode


viewResourceCollectionNode :
    String
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> Html msg
viewResourceCollectionNode kmName mbChildNode cssClass mbTitle =
    let
        resourceCollectionNode =
            viewNode faKmResourceCollection cssClass mbTitle mbChildNode
    in
    viewKnowledgeModelNode (Just resourceCollectionNode) stateClass.none (Just kmName)


viewResourcePageNode :
    String
    -> KnowledgeModel
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewResourcePageNode kmName km cssClass mbTitle parentUuid =
    let
        resourcePageNode =
            viewNode faKmResourcePage cssClass mbTitle Nothing

        parentQuestion =
            getParentResourceCollectionNode kmName km parentUuid resourcePageNode
    in
    parentQuestion
        |> Maybe.withDefault resourcePageNode


viewNode : Html msg -> String -> Maybe String -> Maybe (Html msg) -> Html msg
viewNode icon cssClass mbTitle mbChildNode =
    let
        children =
            mbChildNode
                |> Maybe.map (List.singleton >> ul [])
                |> Maybe.withDefault Html.nothing
    in
    li [ dataCy ("km-migration_diff-tree-node_" ++ cssClass) ]
        [ span [ class cssClass ]
            [ icon
            , text <| Maybe.withDefault "" mbTitle
            ]
        , children
        ]



-- Helpers


stateClass : { edited : String, added : String, deleted : String, none : String }
stateClass =
    { edited = "edited"
    , added = "ins"
    , deleted = "del"
    , none = ""
    }


getParentChapterNode :
    String
    -> KnowledgeModel
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentChapterNode kmName km chapterUuid node =
    KnowledgeModel.getChapter chapterUuid km
        |> Maybe.map
            (\chapter ->
                viewChapterNode
                    kmName
                    (Just node)
                    stateClass.none
                    (Just chapter.title)
            )


getParentQuestionNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentQuestionNode kmName km getParent questionUuid node =
    KnowledgeModel.getQuestion questionUuid km
        |> Maybe.map
            (\question ->
                viewQuestionNode
                    kmName
                    km
                    getParent
                    (Just node)
                    stateClass.none
                    (Just <| Question.getTitle question)
                    (getParent questionUuid)
            )


getParentAnswerNode :
    String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentAnswerNode kmName km getParent answerUuid node =
    KnowledgeModel.getAnswer answerUuid km
        |> Maybe.map
            (\answer ->
                viewAnswerNode
                    kmName
                    km
                    getParent
                    (Just node)
                    stateClass.none
                    (Just answer.label)
                    (getParent answerUuid)
            )


getParentResourceCollectionNode :
    String
    -> KnowledgeModel
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentResourceCollectionNode kmName km resourceCollectionUuid node =
    KnowledgeModel.getResourceCollection resourceCollectionUuid km
        |> Maybe.map
            (\resourceCollection ->
                viewResourceCollectionNode
                    kmName
                    (Just node)
                    stateClass.none
                    (Just resourceCollection.title)
            )
