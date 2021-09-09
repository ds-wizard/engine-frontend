module Wizard.KMEditor.Migration.View.DiffTree exposing (view)

import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Data.Event as Event exposing (Event(..))
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.KnowledgeModel.Reference as Reference
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


view : AppState -> String -> KnowledgeModel -> Event -> Html msg
view appState kmName km event =
    div [ class "diff-tree" ]
        [ ul [] [ viewEvent appState kmName km event ] ]


viewEvent : AppState -> String -> KnowledgeModel -> Event -> Html msg
viewEvent appState kmName km event =
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
            Maybe.map .name <| KnowledgeModel.getIntegration commonData.entityUuid km

        getQuestionTitle commonData =
            Maybe.map Question.getTitle <| KnowledgeModel.getQuestion commonData.entityUuid km

        getAnswerLabel commonData =
            Maybe.map .label <| KnowledgeModel.getAnswer commonData.entityUuid km

        getChoiceLabel commonData =
            Maybe.map .label <| KnowledgeModel.getChoice commonData.entityUuid km

        getReferenceName commonData =
            Maybe.map Reference.getVisibleName <| KnowledgeModel.getReference commonData.entityUuid km

        getExpertName commonData =
            Maybe.map .name <| KnowledgeModel.getExpert commonData.entityUuid km

        viewKnolwedgeModelNode_ =
            viewKnowledgeModelNode appState Nothing

        viewChapterNode_ =
            viewChapterNode appState kmName Nothing

        viewMetricNode_ =
            viewMetricNode appState kmName

        viewPhaseNode_ =
            viewPhaseNode appState kmName

        viewTagNode_ =
            viewTagNode appState kmName

        viewIntegrationNode_ =
            viewIntegrationNode appState kmName

        viewQuestionNode_ =
            viewQuestionNode appState kmName km getParent Nothing

        viewAnswerNode_ =
            viewAnswerNode appState kmName km getParent Nothing

        viewChoiceNode_ =
            viewChoiceNode appState kmName km getParent

        viewReferenceNode_ =
            viewReferenceNode appState kmName km getParent

        viewExpertNode_ =
            viewExpertNode appState kmName km getParent
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
    AppState
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> Html msg
viewKnowledgeModelNode appState mbChildNode cssClass mbTitle =
    viewNode (faSet "km.knowledgeModel" appState) cssClass mbTitle mbChildNode


viewChapterNode :
    AppState
    -> String
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> Html msg
viewChapterNode appState kmName mbChildNode cssClass mbTitle =
    let
        chapterNode =
            viewNode (faSet "km.chapter" appState) cssClass mbTitle mbChildNode
    in
    viewKnowledgeModelNode appState (Just chapterNode) stateClass.none (Just kmName)


viewMetricNode :
    AppState
    -> String
    -> String
    -> Maybe String
    -> Html msg
viewMetricNode appState kmName cssClass mbTitle =
    let
        metricNode =
            viewNode (faSet "km.metric" appState) cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode appState (Just metricNode) stateClass.none (Just kmName)


viewPhaseNode :
    AppState
    -> String
    -> String
    -> Maybe String
    -> Html msg
viewPhaseNode appState kmName cssClass mbTitle =
    let
        phaseNode =
            viewNode (faSet "km.phase" appState) cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode appState (Just phaseNode) stateClass.none (Just kmName)


viewTagNode :
    AppState
    -> String
    -> String
    -> Maybe String
    -> Html msg
viewTagNode appState kmName cssClass mbTitle =
    let
        tagNode =
            viewNode (faSet "km.tag" appState) cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode appState (Just tagNode) stateClass.none (Just kmName)


viewIntegrationNode :
    AppState
    -> String
    -> String
    -> Maybe String
    -> Html msg
viewIntegrationNode appState kmName cssClass mbTitle =
    let
        integrationNode =
            viewNode (faSet "km.integration" appState) cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode appState (Just integrationNode) stateClass.none (Just kmName)


viewQuestionNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewQuestionNode appState kmName km getParent mbChildNode cssClass mbTitle parentUuid =
    let
        questionNode =
            viewNode (faSet "km.question" appState) cssClass mbTitle mbChildNode

        parentChapter =
            getParentChapterNode appState kmName km parentUuid questionNode

        parentQuestion =
            getParentQuestionNode appState kmName km getParent parentUuid questionNode

        parentAnswer =
            getParentAnswerNode appState kmName km getParent parentUuid questionNode
    in
    parentChapter
        |> Maybe.orElseLazy (\_ -> parentQuestion)
        |> Maybe.orElseLazy (\_ -> parentAnswer)
        |> Maybe.withDefault questionNode


viewAnswerNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewAnswerNode appState kmName km getParent mbChildNode cssClass mbTitle parentUuid =
    let
        answerNode =
            viewNode (faSet "km.answer" appState) cssClass mbTitle mbChildNode

        parentQuestion =
            getParentQuestionNode appState kmName km getParent parentUuid answerNode
    in
    parentQuestion
        |> Maybe.withDefault answerNode


viewChoiceNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewChoiceNode appState kmName km getParent cssClass mbTitle parentUuid =
    let
        choiceNode =
            viewNode (faSet "km.choice" appState) cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode appState kmName km getParent parentUuid choiceNode
    in
    parentQuestion
        |> Maybe.withDefault choiceNode


viewReferenceNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewReferenceNode appState kmName km getParent cssClass mbTitle parentUuid =
    let
        referenceNode =
            viewNode (faSet "km.reference" appState) cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode appState kmName km getParent parentUuid referenceNode
    in
    parentQuestion
        |> Maybe.withDefault referenceNode


viewExpertNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewExpertNode appState kmName km getParent cssClass mbTitle parentUuid =
    let
        expertNode =
            viewNode (faSet "km.expert" appState) cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode appState kmName km getParent parentUuid expertNode
    in
    parentQuestion
        |> Maybe.withDefault expertNode


viewNode : Html msg -> String -> Maybe String -> Maybe (Html msg) -> Html msg
viewNode icon cssClass mbTitle mbChildNode =
    let
        children =
            mbChildNode
                |> Maybe.map (List.singleton >> ul [])
                |> Maybe.withDefault emptyNode
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
    { edited = "state-edited"
    , added = "state-added"
    , deleted = "state-deleted"
    , none = ""
    }


getParentChapterNode :
    AppState
    -> String
    -> KnowledgeModel
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentChapterNode appState kmName km chapterUuid node =
    KnowledgeModel.getChapter chapterUuid km
        |> Maybe.map
            (\chapter ->
                viewChapterNode
                    appState
                    kmName
                    (Just node)
                    stateClass.none
                    (Just chapter.title)
            )


getParentQuestionNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentQuestionNode appState kmName km getParent questionUuid node =
    KnowledgeModel.getQuestion questionUuid km
        |> Maybe.map
            (\question ->
                viewQuestionNode
                    appState
                    kmName
                    km
                    getParent
                    (Just node)
                    stateClass.none
                    (Just <| Question.getTitle question)
                    (getParent questionUuid)
            )


getParentAnswerNode :
    AppState
    -> String
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentAnswerNode appState kmName km getParent answerUuid node =
    KnowledgeModel.getAnswer answerUuid km
        |> Maybe.map
            (\answer ->
                viewAnswerNode
                    appState
                    kmName
                    km
                    getParent
                    (Just node)
                    stateClass.none
                    (Just answer.label)
                    (getParent answerUuid)
            )
