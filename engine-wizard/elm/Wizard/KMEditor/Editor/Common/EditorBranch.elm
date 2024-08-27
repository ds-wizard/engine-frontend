module Wizard.KMEditor.Editor.Common.EditorBranch exposing
    ( EditorBranch
    , EditorBranchWarning
    , applyEvent
    , filterDeleted
    , filterDeletedWith
    , getActiveQuestionUuid
    , getAllUuids
    , getChapterUuid
    , getEditUuid
    , getEditorName
    , getFilteredKM
    , getParentUuid
    , init
    , isActive
    , isAdded
    , isDeleted
    , isEdited
    , isEmptyIntegrationEditorUuid
    , isReachable
    , setActiveEditor
    , sortDeleted
    , treeCollapseAll
    , treeExpandAll
    , treeIsNodeOpen
    , treeSetNodeOpen
    )

import Dict
import Gettext exposing (gettext)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import Set exposing (Set)
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Data.Event exposing (Event(..))
import Shared.Data.Event.AddAnswerEventData as AddAnswerEventData
import Shared.Data.Event.AddChapterEventData as AddChapterEventData
import Shared.Data.Event.AddChoiceEventData as AddChoiceEventData
import Shared.Data.Event.AddExpertEventData as AddExpertEventData
import Shared.Data.Event.AddIntegrationEventData as AddIntegrationEventData
import Shared.Data.Event.AddMetricEventData as AddMetricEventData
import Shared.Data.Event.AddPhaseEventData as AddPhaseEventData
import Shared.Data.Event.AddQuestionEventData as AddQuestionEventData
import Shared.Data.Event.AddReferenceEventData as AddReferenceEventData
import Shared.Data.Event.AddResourceCollectionEventData as AddResourceCollectionEventData
import Shared.Data.Event.AddResourcePageEventData as AddResourcePageEventData
import Shared.Data.Event.AddTagEventData as AddTagEventData
import Shared.Data.Event.CommonEventData exposing (CommonEventData)
import Shared.Data.Event.EditAnswerEventData as EditAnswerEventData
import Shared.Data.Event.EditChapterEventData as EditChapterEventData
import Shared.Data.Event.EditChoiceEventData as EditChoiceEventData
import Shared.Data.Event.EditExpertEventData as EditExpertEventData
import Shared.Data.Event.EditIntegrationEventData as EditIntegrationEvent
import Shared.Data.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData
import Shared.Data.Event.EditMetricEventData as EditMetricEventData
import Shared.Data.Event.EditPhaseEventData as EditPhaseEventData
import Shared.Data.Event.EditQuestionEventData as EditQuestionEventData
import Shared.Data.Event.EditReferenceEventData as EditReferenceEventData
import Shared.Data.Event.EditResourceCollectionEventData as EditResourceCollectionEventData
import Shared.Data.Event.EditResourcePageEventData as EditResourcePageEventData
import Shared.Data.Event.EditTagEventData as EditTagEventData
import Shared.Data.Event.MoveEventData exposing (MoveEventData)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert as Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference)
import Shared.Data.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Shared.Data.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.RegexPatterns as RegexPatterns
import Shared.Utils exposing (flip)
import String.Extra as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)


type alias EditorBranch =
    { branch : BranchDetail
    , parentMap : KnowledgeModel.ParentMap
    , activeUuid : String
    , openNodeUuids : List String
    , addedUuids : List String
    , editedUuids : List String
    , deletedUuids : List String
    , emptyIntegrationEditorUuids : Set String
    , warnings : List EditorBranchWarning
    }


type alias EditorBranchWarning =
    { editorUuid : String
    , message : String
    }


init : AppState -> BranchDetail -> Maybe Uuid -> EditorBranch
init appState branch mbEditorUuid =
    let
        kmUuid =
            Uuid.toString branch.knowledgeModel.uuid

        editorBranch =
            { branch = branch
            , parentMap = KnowledgeModel.createParentMap branch.knowledgeModel
            , activeUuid = kmUuid
            , openNodeUuids = [ kmUuid ]
            , addedUuids = []
            , editedUuids = []
            , deletedUuids = []
            , emptyIntegrationEditorUuids = Set.empty
            , warnings = []
            }
    in
    List.foldl (applyEvent appState False) editorBranch editorBranch.branch.events
        |> setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)
        |> computeWarnings appState


getEditUuid : String -> EditorBranch -> Maybe Uuid
getEditUuid entityUuidString editorBranch =
    let
        entityUuid =
            Uuid.fromUuidString entityUuidString
    in
    if entityUuid == editorBranch.branch.knowledgeModel.uuid then
        Nothing

    else
        Just entityUuid


getParentUuid : String -> EditorBranch -> String
getParentUuid uuid editorBranch =
    Maybe.withDefault "" (Dict.get uuid editorBranch.parentMap)


filterDeleted : EditorBranch -> List String -> List String
filterDeleted =
    filterDeletedWith identity


filterDeletedWith : (a -> String) -> EditorBranch -> List a -> List a
filterDeletedWith toUuid editorBranch =
    List.filter (not << flip isDeleted editorBranch << toUuid)


getFilteredKM : EditorBranch -> KnowledgeModel
getFilteredKM editorBranch =
    let
        knowledgeModel =
            editorBranch.branch.knowledgeModel

        knowledgeModelEntities =
            knowledgeModel.entities

        filterChapter _ chapter =
            { chapter | questionUuids = filterDeleted editorBranch chapter.questionUuids }

        filterQuestion _ question =
            let
                filterCommonData commonData =
                    { commonData
                        | tagUuids = filterDeleted editorBranch commonData.tagUuids
                        , referenceUuids = filterDeleted editorBranch commonData.referenceUuids
                        , expertUuids = filterDeleted editorBranch commonData.expertUuids
                    }
            in
            case question of
                OptionsQuestion commonData optionsData ->
                    OptionsQuestion (filterCommonData commonData)
                        { optionsData | answerUuids = filterDeleted editorBranch optionsData.answerUuids }

                ListQuestion commonData listData ->
                    ListQuestion (filterCommonData commonData)
                        { listData | itemTemplateQuestionUuids = filterDeleted editorBranch listData.itemTemplateQuestionUuids }

                ValueQuestion commonData valueData ->
                    ValueQuestion (filterCommonData commonData) valueData

                IntegrationQuestion commonData integrationData ->
                    IntegrationQuestion (filterCommonData commonData) integrationData

                MultiChoiceQuestion commonData multichoiceData ->
                    MultiChoiceQuestion (filterCommonData commonData)
                        { multichoiceData | choiceUuids = filterDeleted editorBranch multichoiceData.choiceUuids }

                ItemSelectQuestion commonData itemSelectData ->
                    ItemSelectQuestion (filterCommonData commonData) itemSelectData

        filterAnswer _ answer =
            { answer | followUpUuids = filterDeleted editorBranch answer.followUpUuids }

        filterResourceCollection _ resourceCollection =
            { resourceCollection | resourcePageUuids = filterDeleted editorBranch resourceCollection.resourcePageUuids }

        entities =
            { knowledgeModelEntities
                | chapters = Dict.map filterChapter knowledgeModelEntities.chapters
                , questions = Dict.map filterQuestion knowledgeModelEntities.questions
                , answers = Dict.map filterAnswer knowledgeModelEntities.answers
                , resourceCollections = Dict.map filterResourceCollection knowledgeModelEntities.resourceCollections
            }
    in
    { knowledgeModel
        | chapterUuids = filterDeleted editorBranch knowledgeModel.chapterUuids
        , tagUuids = filterDeleted editorBranch knowledgeModel.tagUuids
        , integrationUuids = filterDeleted editorBranch knowledgeModel.integrationUuids
        , metricUuids = filterDeleted editorBranch knowledgeModel.metricUuids
        , phaseUuids = filterDeleted editorBranch knowledgeModel.phaseUuids
        , resourceCollectionUuids = filterDeleted editorBranch knowledgeModel.resourceCollectionUuids
        , entities = entities
    }


sortDeleted : (a -> String) -> EditorBranch -> List a -> List a
sortDeleted toUuid editorBranch items =
    let
        ( currentItems, deletedItems ) =
            List.partition (not << flip isDeleted editorBranch << toUuid) items
    in
    currentItems ++ deletedItems


setParent : String -> String -> EditorBranch -> EditorBranch
setParent entityUuid parentUuid editorBranch =
    { editorBranch | parentMap = Dict.insert entityUuid parentUuid editorBranch.parentMap }


setKnowledgeModel : KnowledgeModel -> EditorBranch -> EditorBranch
setKnowledgeModel km editorBranch =
    let
        branch =
            editorBranch.branch
    in
    { editorBranch | branch = { branch | knowledgeModel = km } }


isReachable : EditorBranch -> String -> Bool
isReachable editorBranch entityUuid =
    let
        parentUuid =
            getParentUuid entityUuid editorBranch

        getEntity getter isEntityReachable =
            getter editorBranch.branch.knowledgeModel.entities
                |> Dict.get parentUuid
                |> Maybe.map isEntityReachable

        isInChapterReachable : Chapter -> Bool
        isInChapterReachable chapter =
            List.member entityUuid chapter.questionUuids && isReachable editorBranch chapter.uuid

        isInQuestionReachable : Question -> Bool
        isInQuestionReachable question =
            case question of
                OptionsQuestion _ data ->
                    List.member entityUuid data.answerUuids && isReachable editorBranch parentUuid

                ListQuestion _ data ->
                    List.member entityUuid data.itemTemplateQuestionUuids && isReachable editorBranch parentUuid

                MultiChoiceQuestion _ data ->
                    List.member entityUuid data.choiceUuids && isReachable editorBranch parentUuid

                _ ->
                    False

        isInAnswerReachable : Answer -> Bool
        isInAnswerReachable answer =
            List.member entityUuid answer.followUpUuids && isReachable editorBranch parentUuid
    in
    if isDeleted entityUuid editorBranch then
        False

    else if parentUuid == Uuid.toString editorBranch.branch.knowledgeModel.uuid then
        True

    else
        getEntity .chapters isInChapterReachable
            |> Maybe.orElse (getEntity .questions isInQuestionReachable)
            |> Maybe.orElse (getEntity .answers isInAnswerReachable)
            |> Maybe.withDefault False


getEditorName : AppState -> String -> EditorBranch -> String
getEditorName appState uuid editorBranch =
    let
        getEditorName_ getEntityName getEntity =
            Maybe.map getEntityName (getEntity uuid editorBranch.branch.knowledgeModel)

        getKnowledgeModelName =
            if uuid == Uuid.toString editorBranch.branch.knowledgeModel.uuid then
                Just editorBranch.branch.name

            else
                Nothing

        getChapterName =
            getEditorName_ (String.withDefault (gettext "Untitled chapter" appState.locale) << .title) KnowledgeModel.getChapter

        getQuestionName =
            getEditorName_ (String.withDefault (gettext "Untitled question" appState.locale) << Question.getTitle) KnowledgeModel.getQuestion

        getMetricName =
            getEditorName_ (String.withDefault (gettext "Untitled metric" appState.locale) << .title) KnowledgeModel.getMetric

        getPhaseName =
            getEditorName_ (String.withDefault (gettext "Untitled phase" appState.locale) << .title) KnowledgeModel.getPhase

        getTagName =
            getEditorName_ (String.withDefault (gettext "Untitled tag" appState.locale) << .name) KnowledgeModel.getTag

        getIntegrationName =
            getEditorName_ (String.withDefault (gettext "Untitled integration" appState.locale) << Integration.getVisibleName) KnowledgeModel.getIntegration

        getAnswerName =
            getEditorName_ (String.withDefault (gettext "Untitled answer" appState.locale) << .label) KnowledgeModel.getAnswer

        getChoiceName =
            getEditorName_ (String.withDefault (gettext "Untitled choice" appState.locale) << .label) KnowledgeModel.getChoice

        getReferenceName =
            getEditorName_ (String.withDefault (gettext "Untitled reference" appState.locale) << Reference.getVisibleName (KnowledgeModel.getAllResourcePages editorBranch.branch.knowledgeModel)) KnowledgeModel.getReference

        getExpertName =
            getEditorName_ (String.withDefault (gettext "Untitled expert" appState.locale) << Expert.getVisibleName) KnowledgeModel.getExpert
    in
    getKnowledgeModelName
        |> Maybe.orElse getChapterName
        |> Maybe.orElse getQuestionName
        |> Maybe.orElse getMetricName
        |> Maybe.orElse getPhaseName
        |> Maybe.orElse getTagName
        |> Maybe.orElse getIntegrationName
        |> Maybe.orElse getAnswerName
        |> Maybe.orElse getChoiceName
        |> Maybe.orElse getReferenceName
        |> Maybe.orElse getExpertName
        |> Maybe.withDefault ""


setActiveEditor : Maybe String -> EditorBranch -> EditorBranch
setActiveEditor mbEditorUuid editorBranch =
    let
        kmUuid =
            Uuid.toString editorBranch.branch.knowledgeModel.uuid

        activeUuid =
            Maybe.withDefault kmUuid mbEditorUuid

        getParents childUuid =
            case Dict.get childUuid editorBranch.parentMap of
                Just parent ->
                    childUuid :: getParents parent

                Nothing ->
                    [ childUuid ]
    in
    { editorBranch
        | activeUuid = activeUuid
        , openNodeUuids = List.unique (List.drop 1 (getParents activeUuid) ++ editorBranch.openNodeUuids)
    }


updateActiveEditor : EditorBranch -> EditorBranch
updateActiveEditor editorBranch =
    let
        editorIsDeleted uuid =
            isDeleted uuid editorBranch

        editorHasChild parentUuid childUuid =
            case KnowledgeModel.getQuestion parentUuid editorBranch.branch.knowledgeModel of
                Just question ->
                    isQuestionChild question childUuid

                Nothing ->
                    True

        isQuestionChild question childUuid =
            let
                childUuids =
                    []
                        |> (++) (Question.getTagUuids question)
                        |> (++) (Question.getExpertUuids question)
                        |> (++) (Question.getReferenceUuids question)
                        |> (++) (Question.getAnswerUuids question)
                        |> (++) (Question.getChoiceUuids question)
                        |> (++) (Question.getItemTemplateQuestionUuids question)
            in
            List.member childUuid childUuids

        nil =
            Uuid.toString Uuid.nil

        getActiveEditor currentUuid activeEditorUuid =
            if currentUuid == nil then
                if activeEditorUuid == nil then
                    Nothing

                else
                    Just activeEditorUuid

            else
                let
                    parentUuid =
                        Maybe.withDefault (Uuid.toString Uuid.nil) (Dict.get currentUuid editorBranch.parentMap)
                in
                if editorIsDeleted currentUuid || not (editorHasChild parentUuid currentUuid) then
                    getActiveEditor parentUuid parentUuid

                else
                    getActiveEditor parentUuid activeEditorUuid

        newActiveEditorUuid =
            getActiveEditor editorBranch.activeUuid editorBranch.activeUuid
    in
    setActiveEditor newActiveEditorUuid editorBranch


getActiveQuestionUuid : EditorBranch -> String
getActiveQuestionUuid editorBranch =
    let
        isQuestionEditor uuid =
            Dict.member uuid editorBranch.branch.knowledgeModel.entities.questions

        getParentQuestion uuid =
            if String.isEmpty uuid || isQuestionEditor uuid then
                uuid

            else
                getParentQuestion (getParentUuid uuid editorBranch)
    in
    getParentQuestion editorBranch.activeUuid


getChapterUuid : String -> EditorBranch -> String
getChapterUuid entityUuid editorBranch =
    let
        isChapter uuid =
            List.member uuid editorBranch.branch.knowledgeModel.chapterUuids

        getParent uuid =
            if String.isEmpty uuid || isChapter uuid then
                uuid

            else
                getParent (getParentUuid uuid editorBranch)
    in
    getParent entityUuid


treeSetNodeOpen : String -> Bool -> EditorBranch -> EditorBranch
treeSetNodeOpen entityUuid open editorBranch =
    let
        openUuids =
            if open then
                entityUuid :: editorBranch.openNodeUuids

            else
                List.filter ((/=) entityUuid) editorBranch.openNodeUuids
    in
    { editorBranch | openNodeUuids = openUuids }


treeIsNodeOpen : String -> EditorBranch -> Bool
treeIsNodeOpen entityUuid editorBranch =
    List.member entityUuid editorBranch.openNodeUuids


getAllUuids : EditorBranch -> List String
getAllUuids editorBranch =
    Uuid.toString editorBranch.branch.knowledgeModel.uuid
        :: Dict.keys editorBranch.branch.knowledgeModel.entities.chapters
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.questions
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.answers
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.choices
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.experts
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.references
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.integrations
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.resourceCollections
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.tags
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.metrics
        ++ Dict.keys editorBranch.branch.knowledgeModel.entities.phases


treeExpandAll : EditorBranch -> EditorBranch
treeExpandAll editorBranch =
    { editorBranch | openNodeUuids = getAllUuids editorBranch }


treeCollapseAll : EditorBranch -> EditorBranch
treeCollapseAll editorBranch =
    { editorBranch | openNodeUuids = [] }


isActive : String -> EditorBranch -> Bool
isActive activeEditor editorBranch =
    editorBranch.activeUuid == activeEditor


setEdited : String -> EditorBranch -> EditorBranch
setEdited uuid editorBranch =
    if List.member uuid editorBranch.editedUuids then
        editorBranch

    else
        { editorBranch | editedUuids = uuid :: editorBranch.editedUuids }


isEdited : String -> EditorBranch -> Bool
isEdited uuid editorBranch =
    List.member uuid editorBranch.editedUuids && not (isAdded uuid editorBranch) && not (isDeleted uuid editorBranch)


setDeleted : String -> EditorBranch -> EditorBranch
setDeleted uuid editorBranch =
    { editorBranch | deletedUuids = uuid :: editorBranch.deletedUuids }


isDeleted : String -> EditorBranch -> Bool
isDeleted uuid editorBranch =
    List.member uuid editorBranch.deletedUuids


setAdded : String -> EditorBranch -> EditorBranch
setAdded uuid editorBranch =
    { editorBranch | addedUuids = uuid :: editorBranch.addedUuids }


isAdded : String -> EditorBranch -> Bool
isAdded uuid editorBranch =
    List.member uuid editorBranch.addedUuids && not (isDeleted uuid editorBranch)


addEmptyIntegrationEditorUuid : String -> EditorBranch -> EditorBranch
addEmptyIntegrationEditorUuid uuid editorBranch =
    { editorBranch | emptyIntegrationEditorUuids = Set.insert uuid editorBranch.emptyIntegrationEditorUuids }


removeEmptyIntegrationEditorUuid : String -> EditorBranch -> EditorBranch
removeEmptyIntegrationEditorUuid uuid editorBranch =
    { editorBranch | emptyIntegrationEditorUuids = Set.remove uuid editorBranch.emptyIntegrationEditorUuids }


isEmptyIntegrationEditorUuid : String -> EditorBranch -> Bool
isEmptyIntegrationEditorUuid uuid editorBranch =
    Set.member uuid editorBranch.emptyIntegrationEditorUuids


applyEvent : AppState -> Bool -> Event -> EditorBranch -> EditorBranch
applyEvent appState local event originalEditorBranch =
    let
        branch =
            originalEditorBranch.branch

        knowledgeModel =
            branch.knowledgeModel

        editorBranch =
            { originalEditorBranch | branch = { branch | events = branch.events ++ [ event ] } }
    in
    computeWarnings appState <|
        case event of
            AddKnowledgeModelEvent _ _ ->
                editorBranch

            AddAnswerEvent eventData commonData ->
                let
                    answer =
                        AddAnswerEventData.toAnswer commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertAnswer answer commonData editorBranch

            AddChapterEvent eventData commonData ->
                let
                    chapter =
                        AddChapterEventData.toChapter commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertChapter chapter commonData editorBranch

            AddChoiceEvent eventData commonData ->
                let
                    choice =
                        AddChoiceEventData.toChoice commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertChoice choice commonData editorBranch

            AddExpertEvent eventData commonData ->
                let
                    expert =
                        AddExpertEventData.toExpert commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertExpert expert commonData editorBranch

            AddIntegrationEvent eventData commonData ->
                let
                    integration =
                        AddIntegrationEventData.toIntegration commonData.entityUuid eventData

                    updatedEditorBranch =
                        addEmptyIntegrationEditorUuid (Integration.getUuid integration) editorBranch
                in
                applyAdd local KnowledgeModel.insertIntegration integration commonData updatedEditorBranch

            AddMetricEvent eventData commonData ->
                let
                    metric =
                        AddMetricEventData.toMetric commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertMetric metric commonData editorBranch

            AddPhaseEvent eventData commonData ->
                let
                    phase =
                        AddPhaseEventData.toPhase commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertPhase phase commonData editorBranch

            AddQuestionEvent eventData commonData ->
                let
                    question =
                        AddQuestionEventData.toQuestion commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertQuestion question commonData editorBranch

            AddReferenceEvent eventData commonData ->
                let
                    reference =
                        AddReferenceEventData.toReference commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertReference reference commonData editorBranch

            AddResourceCollectionEvent eventData commonData ->
                let
                    resourceCollection =
                        AddResourceCollectionEventData.toResourceCollection commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertResourceCollection resourceCollection commonData editorBranch

            AddResourcePageEvent eventData commonData ->
                let
                    resourcePage =
                        AddResourcePageEventData.toResourcePage commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertResourcePage resourcePage commonData editorBranch

            AddTagEvent eventData commonData ->
                let
                    tag =
                        AddTagEventData.toTag commonData.entityUuid eventData
                in
                applyAdd local KnowledgeModel.insertTag tag commonData editorBranch

            EditAnswerEvent eventData commonData ->
                let
                    mbAnswer =
                        KnowledgeModel.getAnswer commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditAnswerEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateAnswer mbAnswer commonData editorBranch

            EditChapterEvent eventData commonData ->
                let
                    mbChapter =
                        KnowledgeModel.getChapter commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditChapterEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateChapter mbChapter commonData editorBranch

            EditChoiceEvent eventData commonData ->
                let
                    mbChoice =
                        KnowledgeModel.getChoice commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditChoiceEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateChoice mbChoice commonData editorBranch

            EditExpertEvent eventData commonData ->
                let
                    mbExpert =
                        KnowledgeModel.getExpert commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditExpertEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateExpert mbExpert commonData editorBranch

            EditIntegrationEvent eventData commonData ->
                let
                    mbIntegration =
                        KnowledgeModel.getIntegration commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditIntegrationEvent.apply eventData)

                    updatedEditorBranch =
                        removeEmptyIntegrationEditorUuid (Maybe.unwrap "" Integration.getUuid mbIntegration) editorBranch
                in
                applyEdit KnowledgeModel.updateIntegration mbIntegration commonData updatedEditorBranch

            EditKnowledgeModelEvent eventData _ ->
                let
                    newKnowledgeModel =
                        EditKnowledgeModelEventData.apply eventData knowledgeModel
                in
                setKnowledgeModel newKnowledgeModel editorBranch
                    |> setEdited (Uuid.toString knowledgeModel.uuid)

            EditMetricEvent eventData commonData ->
                let
                    mbMetric =
                        KnowledgeModel.getMetric commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditMetricEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateMetric mbMetric commonData editorBranch

            EditPhaseEvent eventData commonData ->
                let
                    mbPhase =
                        KnowledgeModel.getPhase commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditPhaseEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updatePhase mbPhase commonData editorBranch

            EditQuestionEvent eventData commonData ->
                let
                    mbQuestion =
                        KnowledgeModel.getQuestion commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditQuestionEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateQuestion mbQuestion commonData editorBranch

            EditReferenceEvent eventData commonData ->
                let
                    mbReference =
                        KnowledgeModel.getReference commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditReferenceEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateReference mbReference commonData editorBranch

            EditResourceCollectionEvent eventData commonData ->
                let
                    mbResourceCollection =
                        KnowledgeModel.getResourceCollection commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditResourceCollectionEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateResourceCollection mbResourceCollection commonData editorBranch

            EditResourcePageEvent eventData commonData ->
                let
                    mbResourcePage =
                        KnowledgeModel.getResourcePage commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditResourcePageEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateResourcePage mbResourcePage commonData editorBranch

            EditTagEvent eventData commonData ->
                let
                    mbTag =
                        KnowledgeModel.getTag commonData.entityUuid knowledgeModel
                            |> Maybe.map (EditTagEventData.apply eventData)
                in
                applyEdit KnowledgeModel.updateTag mbTag commonData editorBranch

            DeleteAnswerEvent commonData ->
                applyDelete commonData editorBranch

            DeleteChapterEvent commonData ->
                applyDelete commonData editorBranch

            DeleteChoiceEvent commonData ->
                applyDelete commonData editorBranch

            DeleteExpertEvent commonData ->
                applyDelete commonData editorBranch

            DeleteIntegrationEvent commonData ->
                applyDelete commonData editorBranch

            DeleteMetricEvent commonData ->
                applyDelete commonData editorBranch

            DeletePhaseEvent commonData ->
                applyDelete commonData editorBranch

            DeleteReferenceEvent commonData ->
                applyDelete commonData editorBranch

            DeleteResourceCollectionEvent commonData ->
                applyDelete commonData editorBranch

            DeleteResourcePageEvent commonData ->
                applyDelete commonData editorBranch

            DeleteQuestionEvent commonData ->
                applyDelete commonData editorBranch

            DeleteTagEvent commonData ->
                applyDelete commonData editorBranch

            MoveQuestionEvent moveData commonData ->
                applyMove KnowledgeModel.moveQuestion KnowledgeModel.getQuestion commonData moveData editorBranch

            MoveAnswerEvent moveData commonData ->
                applyMove KnowledgeModel.moveAnswer KnowledgeModel.getAnswer commonData moveData editorBranch

            MoveChoiceEvent moveData commonData ->
                applyMove KnowledgeModel.moveChoice KnowledgeModel.getChoice commonData moveData editorBranch

            MoveReferenceEvent moveData commonData ->
                applyMove KnowledgeModel.moveReference KnowledgeModel.getReference commonData moveData editorBranch

            MoveExpertEvent moveData commonData ->
                applyMove KnowledgeModel.moveExpert KnowledgeModel.getExpert commonData moveData editorBranch


applyAdd : Bool -> (a -> String -> KnowledgeModel -> KnowledgeModel) -> a -> CommonEventData -> EditorBranch -> EditorBranch
applyAdd local updateKm entity { entityUuid, parentUuid } editorBranch =
    let
        openAddedEditor uuid eb =
            if local then
                setActiveEditor (Just uuid) eb

            else
                eb

        newKnowledgeModel =
            updateKm entity parentUuid editorBranch.branch.knowledgeModel
    in
    editorBranch
        |> setKnowledgeModel newKnowledgeModel
        |> setParent entityUuid parentUuid
        |> setAdded entityUuid
        |> openAddedEditor entityUuid


applyEdit : (a -> KnowledgeModel -> KnowledgeModel) -> Maybe a -> CommonEventData -> EditorBranch -> EditorBranch
applyEdit updateKm mbEntity { entityUuid } editorBranch =
    case mbEntity of
        Just entity ->
            editorBranch
                |> setKnowledgeModel (updateKm entity editorBranch.branch.knowledgeModel)
                |> setEdited entityUuid
                |> updateActiveEditor

        Nothing ->
            editorBranch


applyDelete : CommonEventData -> EditorBranch -> EditorBranch
applyDelete { entityUuid } editorBranch =
    editorBranch
        |> setDeleted entityUuid
        |> updateActiveEditor


applyMove : (a -> String -> String -> KnowledgeModel -> KnowledgeModel) -> (String -> KnowledgeModel -> Maybe a) -> CommonEventData -> MoveEventData -> EditorBranch -> EditorBranch
applyMove updateKm getEntity { entityUuid, parentUuid } { targetUuid } editorBranch =
    case getEntity entityUuid editorBranch.branch.knowledgeModel of
        Just entity ->
            editorBranch
                |> setKnowledgeModel (updateKm entity parentUuid targetUuid editorBranch.branch.knowledgeModel)
                |> setParent entityUuid targetUuid
                |> setEdited entityUuid
                |> setActiveEditor (Just entityUuid)

        Nothing ->
            editorBranch


computeWarnings : AppState -> EditorBranch -> EditorBranch
computeWarnings appState editorBranch =
    let
        filteredKM =
            getFilteredKM editorBranch

        warnings =
            List.concatMap (computeChapterWarnings appState editorBranch filteredKM) (KnowledgeModel.getChapters filteredKM)
                |> flip (++) (List.concatMap (computeMetricWarnings appState) (KnowledgeModel.getMetrics filteredKM))
                |> flip (++) (List.concatMap (computePhaseWarnings appState) (KnowledgeModel.getPhases filteredKM))
                |> flip (++) (List.concatMap (computeTagWarnings appState) (KnowledgeModel.getTags filteredKM))
                |> flip (++) (List.concatMap (computeIntegrationWarnings appState) (KnowledgeModel.getIntegrations filteredKM))
                |> flip (++) (List.concatMap (computeResourceCollectionWarnings appState filteredKM) (KnowledgeModel.getResourceCollections filteredKM))
    in
    { editorBranch | warnings = warnings }


computeChapterWarnings : AppState -> EditorBranch -> KnowledgeModel -> Chapter -> List EditorBranchWarning
computeChapterWarnings appState editorBranch km chapter =
    let
        titleWarning =
            if String.isEmpty chapter.title then
                [ { editorUuid = chapter.uuid
                  , message = gettext "Empty title for chapter" appState.locale
                  }
                ]

            else
                []

        questionWarnings =
            List.concatMap
                (computeQuestionWarnings appState editorBranch km)
                (KnowledgeModel.getChapterQuestions chapter.uuid km)
    in
    titleWarning ++ questionWarnings


computeQuestionWarnings : AppState -> EditorBranch -> KnowledgeModel -> Question -> List EditorBranchWarning
computeQuestionWarnings appState editorBranch km question =
    let
        questionUuid =
            Question.getUuid question

        createError message =
            [ { editorUuid = questionUuid
              , message = message
              }
            ]

        titleWarning =
            if String.isEmpty (Question.getTitle question) then
                createError (gettext "Empty title for question" appState.locale)

            else
                []

        typeWarnings =
            case question of
                Question.OptionsQuestion _ data ->
                    if List.isEmpty data.answerUuids then
                        createError (gettext "No answers for options question" appState.locale)

                    else
                        List.concatMap
                            (computeAnswerWarnings appState editorBranch km)
                            (KnowledgeModel.getQuestionAnswers questionUuid km)

                Question.ListQuestion _ data ->
                    if List.isEmpty data.itemTemplateQuestionUuids then
                        createError (gettext "No item questions for list question" appState.locale)

                    else
                        List.concatMap
                            (computeQuestionWarnings appState editorBranch km)
                            (KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km)

                Question.IntegrationQuestion _ data ->
                    if data.integrationUuid == Uuid.toString Uuid.nil then
                        createError (gettext "No integration selected for integration question" appState.locale)

                    else
                        []

                Question.MultiChoiceQuestion _ data ->
                    if List.isEmpty data.choiceUuids then
                        createError (gettext "No choices for multi-choice question" appState.locale)

                    else
                        List.concatMap
                            (computeChoiceWarnings appState)
                            (KnowledgeModel.getQuestionChoices questionUuid km)

                Question.ItemSelectQuestion _ data ->
                    let
                        listQuestionNotSelected =
                            case data.listQuestionUuid of
                                Just listQuestionUuid ->
                                    isDeleted listQuestionUuid editorBranch

                                Nothing ->
                                    True
                    in
                    if listQuestionNotSelected then
                        createError (gettext "No list question selected for item select question" appState.locale)

                    else
                        []

                _ ->
                    []

        referencesWarnings =
            List.concatMap
                (computeReferenceWarnings appState)
                (KnowledgeModel.getQuestionReferences questionUuid km)

        expertWarnings =
            List.concatMap
                (computeExpertWarnings appState)
                (KnowledgeModel.getQuestionExperts questionUuid km)
    in
    titleWarning ++ typeWarnings ++ referencesWarnings ++ expertWarnings


computeAnswerWarnings : AppState -> EditorBranch -> KnowledgeModel -> Answer -> List EditorBranchWarning
computeAnswerWarnings appState editorBranch km answer =
    let
        labelWarning =
            if String.isEmpty answer.label then
                [ { editorUuid = answer.uuid
                  , message = gettext "Empty label for answer" appState.locale
                  }
                ]

            else
                []

        followUpQuestionsWarnings =
            List.concatMap
                (computeQuestionWarnings appState editorBranch km)
                (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km)
    in
    labelWarning ++ followUpQuestionsWarnings


computeChoiceWarnings : AppState -> Choice -> List EditorBranchWarning
computeChoiceWarnings appState choice =
    if String.isEmpty choice.label then
        [ { editorUuid = choice.uuid
          , message = gettext "Empty label for choice" appState.locale
          }
        ]

    else
        []


computeReferenceWarnings : AppState -> Reference -> List EditorBranchWarning
computeReferenceWarnings appState reference =
    let
        createError message =
            [ { editorUuid = Reference.getUuid reference
              , message = message
              }
            ]
    in
    case reference of
        Reference.ResourcePageReference data ->
            if Maybe.isNothing data.resourcePageUuid then
                createError (gettext "No resource page selected for resource page reference" appState.locale)

            else
                []

        Reference.URLReference data ->
            if String.isEmpty data.url then
                createError (gettext "Empty URL for URL reference" appState.locale)

            else if not (Regex.contains RegexPatterns.url data.url) then
                createError (gettext "Invalid URL for URL reference" appState.locale)

            else
                []

        _ ->
            []


computeExpertWarnings : AppState -> Expert -> List EditorBranchWarning
computeExpertWarnings appState expert =
    let
        createError message =
            [ { editorUuid = expert.uuid
              , message = message
              }
            ]
    in
    if String.isEmpty expert.email then
        createError (gettext "Empty email for expert" appState.locale)

    else if not (Regex.contains RegexPatterns.email expert.email) then
        createError (gettext "Invalid email for expert" appState.locale)

    else
        []


computeMetricWarnings : AppState -> Metric -> List EditorBranchWarning
computeMetricWarnings appState metric =
    if String.isEmpty metric.title then
        [ { editorUuid = metric.uuid
          , message = gettext "Empty title for metric" appState.locale
          }
        ]

    else
        []


computePhaseWarnings : AppState -> Phase -> List EditorBranchWarning
computePhaseWarnings appState phase =
    if String.isEmpty phase.title then
        [ { editorUuid = phase.uuid
          , message = gettext "Empty title for phase" appState.locale
          }
        ]

    else
        []


computeTagWarnings : AppState -> Tag -> List EditorBranchWarning
computeTagWarnings appState tag =
    if String.isEmpty tag.name then
        [ { editorUuid = tag.uuid
          , message = gettext "Empty name for tag" appState.locale
          }
        ]

    else
        []


computeIntegrationWarnings : AppState -> Integration -> List EditorBranchWarning
computeIntegrationWarnings appState integration =
    let
        createError message =
            [ { editorUuid = Integration.getUuid integration
              , message = message
              }
            ]

        idWarning =
            if String.isEmpty (Integration.getId integration) then
                createError (gettext "Empty ID for integration" appState.locale)

            else
                []

        typeWarnings =
            case integration of
                Integration.ApiIntegration _ data ->
                    let
                        urlError =
                            if String.isEmpty data.requestUrl then
                                createError (gettext "Empty request URL for integration" appState.locale)

                            else
                                []

                        requestMethod =
                            if String.isEmpty data.requestMethod then
                                createError (gettext "Empty request HTTP method for integration" appState.locale)

                            else
                                []

                        responseItemTemplate =
                            if String.isEmpty data.responseItemTemplate then
                                createError (gettext "Empty response item template for integration" appState.locale)

                            else
                                []
                    in
                    urlError ++ requestMethod ++ responseItemTemplate

                Integration.WidgetIntegration _ data ->
                    if String.isEmpty data.widgetUrl then
                        createError (gettext "Empty widget URL for integration" appState.locale)

                    else
                        []
    in
    idWarning ++ typeWarnings


computeResourceCollectionWarnings : AppState -> KnowledgeModel -> ResourceCollection -> List EditorBranchWarning
computeResourceCollectionWarnings appState km resourceCollection =
    let
        titleWarning =
            if String.isEmpty resourceCollection.title then
                [ { editorUuid = resourceCollection.uuid
                  , message = gettext "Empty title for resource collection" appState.locale
                  }
                ]

            else
                []

        resourcePagesWarnings =
            List.concatMap
                (computeResourcePageWarnings appState)
                (KnowledgeModel.getResourceCollectionResourcePages resourceCollection.uuid km)
    in
    titleWarning ++ resourcePagesWarnings


computeResourcePageWarnings : AppState -> ResourcePage -> List EditorBranchWarning
computeResourcePageWarnings appState resourcePage =
    let
        titleWarning =
            if String.isEmpty resourcePage.title then
                [ { editorUuid = resourcePage.uuid
                  , message = gettext "Empty title for resource page" appState.locale
                  }
                ]

            else
                []

        contentWarning =
            if String.isEmpty resourcePage.content then
                [ { editorUuid = resourcePage.uuid
                  , message = gettext "Empty content for resource page" appState.locale
                  }
                ]

            else
                []
    in
    titleWarning ++ contentWarning
