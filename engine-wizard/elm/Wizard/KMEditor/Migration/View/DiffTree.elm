module Wizard.KMEditor.Migration.View.DiffTree exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Shared.Data.Event as Event exposing (Event(..))
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel, ParentMap)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference)
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Common.AppState exposing (AppState)


view : AppState -> KnowledgeModel -> Event -> Html msg
view appState km event =
    div [ class "diff-tree" ]
        [ ul [] [ viewEvent appState km event ] ]


viewEvent : AppState -> KnowledgeModel -> Event -> Html msg
viewEvent appState km event =
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
            viewChapterNode appState km Nothing

        viewTagNode_ =
            viewTagNode appState km

        viewIntegrationNode_ =
            viewIntegrationNode appState km

        viewQuestionNode_ =
            viewQuestionNode appState km getParent Nothing

        viewAnswerNode_ =
            viewAnswerNode appState km getParent Nothing

        viewChoiceNode_ =
            viewChoiceNode appState km getParent

        viewReferenceNode_ =
            viewReferenceNode appState km getParent

        viewExpertNode_ =
            viewExpertNode appState km getParent
    in
    case event of
        AddKnowledgeModelEvent _ _ ->
            viewKnolwedgeModelNode_ stateClass.added eventEntityName

        EditKnowledgeModelEvent _ _ ->
            viewKnolwedgeModelNode_ stateClass.edited (eventEntityNameOrDefault (Just km.name))

        AddChapterEvent _ _ ->
            viewChapterNode_ stateClass.added eventEntityName

        EditChapterEvent _ commonData ->
            viewChapterNode_ stateClass.edited (eventEntityNameOrDefault (getChapterTitle commonData))

        DeleteChapterEvent commonData ->
            viewChapterNode_ stateClass.deleted (getChapterTitle commonData)

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
    -> KnowledgeModel
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> Html msg
viewChapterNode appState km mbChildNode cssClass mbTitle =
    let
        chapterNode =
            viewNode (faSet "km.chapter" appState) cssClass mbTitle mbChildNode
    in
    viewKnowledgeModelNode appState (Just chapterNode) stateClass.none (Just km.name)


viewTagNode :
    AppState
    -> KnowledgeModel
    -> String
    -> Maybe String
    -> Html msg
viewTagNode appState km cssClass mbTitle =
    let
        tagNode =
            viewNode (faSet "km.tag" appState) cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode appState (Just tagNode) stateClass.none (Just km.name)


viewIntegrationNode :
    AppState
    -> KnowledgeModel
    -> String
    -> Maybe String
    -> Html msg
viewIntegrationNode appState km cssClass mbTitle =
    let
        integrationNode =
            viewNode (faSet "km.integration" appState) cssClass mbTitle Nothing
    in
    viewKnowledgeModelNode appState (Just integrationNode) stateClass.none (Just km.name)


viewQuestionNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewQuestionNode appState km getParent mbChildNode cssClass mbTitle parentUuid =
    let
        questionNode =
            viewNode (faSet "km.question" appState) cssClass mbTitle mbChildNode

        parentChapter =
            getParentChapterNode appState km parentUuid questionNode

        parentQuestion =
            getParentQuestionNode appState km getParent parentUuid questionNode

        parentAnswer =
            getParentAnswerNode appState km getParent parentUuid questionNode
    in
    parentChapter
        |> Maybe.orElseLazy (\_ -> parentQuestion)
        |> Maybe.orElseLazy (\_ -> parentAnswer)
        |> Maybe.withDefault questionNode


viewAnswerNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> Maybe (Html msg)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewAnswerNode appState km getParent mbChildNode cssClass mbTitle parentUuid =
    let
        answerNode =
            viewNode (faSet "km.answer" appState) cssClass mbTitle mbChildNode

        parentQuestion =
            getParentQuestionNode appState km getParent parentUuid answerNode
    in
    parentQuestion
        |> Maybe.withDefault answerNode


viewChoiceNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewChoiceNode appState km getParent cssClass mbTitle parentUuid =
    let
        choiceNode =
            viewNode (faSet "km.choice" appState) cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode appState km getParent parentUuid choiceNode
    in
    parentQuestion
        |> Maybe.withDefault choiceNode


viewReferenceNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewReferenceNode appState km getParent cssClass mbTitle parentUuid =
    let
        referenceNode =
            viewNode (faSet "km.reference" appState) cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode appState km getParent parentUuid referenceNode
    in
    parentQuestion
        |> Maybe.withDefault referenceNode


viewExpertNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Maybe String
    -> String
    -> Html msg
viewExpertNode appState km getParent cssClass mbTitle parentUuid =
    let
        expertNode =
            viewNode (faSet "km.expert" appState) cssClass mbTitle Nothing

        parentQuestion =
            getParentQuestionNode appState km getParent parentUuid expertNode
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
    li []
        [ span [ class cssClass ]
            [ icon
            , text <| Maybe.withDefault "" mbTitle
            ]
        , children
        ]



-- Helpers


stateClass =
    { edited = "state-edited"
    , added = "state-added"
    , deleted = "state-deleted"
    , none = ""
    }


getParentChapterNode :
    AppState
    -> KnowledgeModel
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentChapterNode appState km chapterUuid node =
    KnowledgeModel.getChapter chapterUuid km
        |> Maybe.map
            (\chapter ->
                viewChapterNode
                    appState
                    km
                    (Just node)
                    stateClass.none
                    (Just chapter.title)
            )


getParentQuestionNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentQuestionNode appState km getParent questionUuid node =
    KnowledgeModel.getQuestion questionUuid km
        |> Maybe.map
            (\question ->
                viewQuestionNode
                    appState
                    km
                    getParent
                    (Just node)
                    stateClass.none
                    (Just <| Question.getTitle question)
                    (getParent questionUuid)
            )


getParentAnswerNode :
    AppState
    -> KnowledgeModel
    -> (String -> String)
    -> String
    -> Html msg
    -> Maybe (Html msg)
getParentAnswerNode appState km getParent answerUuid node =
    KnowledgeModel.getAnswer answerUuid km
        |> Maybe.map
            (\answer ->
                viewAnswerNode
                    appState
                    km
                    getParent
                    (Just node)
                    stateClass.none
                    (Just answer.label)
                    (getParent answerUuid)
            )
