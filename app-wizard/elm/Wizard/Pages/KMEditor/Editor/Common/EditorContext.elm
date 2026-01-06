module Wizard.Pages.KMEditor.Editor.Common.EditorContext exposing
    ( EditorContext
    , EditorContextWarning
    , applyEvent
    , computeWarnings
    , editorRoute
    , filterDeleted
    , filterDeletedWith
    , filterExistingAnswers
    , filterExistingChapters
    , filterExistingChoices
    , filterExistingExperts
    , filterExistingIntegrations
    , filterExistingMetrics
    , filterExistingPhases
    , filterExistingQuestions
    , filterExistingReferences
    , filterExistingResourceCollections
    , filterExistingResourcePages
    , filterExistingTags
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
    , isQuestionDeletedInHierarchy
    , isReachable
    , setActiveEditor
    , setReplies
    , sortDeleted
    , treeCollapseAll
    , treeExpandAll
    , treeIsNodeOpen
    , treeSetNodeOpen
    )

import Common.Utils.JinjaUtils as JinjaUtils
import Common.Utils.RegexPatterns as RegexPatterns
import Dict exposing (Dict)
import Flip exposing (flip)
import Gettext exposing (gettext)
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import Set exposing (Set)
import String.Extra as String
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event exposing (Event)
import Wizard.Api.Models.Event.AddAnswerEventData as AddAnswerEventData
import Wizard.Api.Models.Event.AddChapterEventData as AddChapterEventData
import Wizard.Api.Models.Event.AddChoiceEventData as AddChoiceEventData
import Wizard.Api.Models.Event.AddExpertEventData as AddExpertEventData
import Wizard.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Wizard.Api.Models.Event.AddMetricEventData as AddMetricEventData
import Wizard.Api.Models.Event.AddPhaseEventData as AddPhaseEventData
import Wizard.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Wizard.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Wizard.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData
import Wizard.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData
import Wizard.Api.Models.Event.AddTagEventData as AddTagEventData
import Wizard.Api.Models.Event.EditAnswerEventData as EditAnswerEventData
import Wizard.Api.Models.Event.EditChapterEventData as EditChapterEventData
import Wizard.Api.Models.Event.EditChoiceEventData as EditChoiceEventData
import Wizard.Api.Models.Event.EditExpertEventData as EditExpertEventData
import Wizard.Api.Models.Event.EditIntegrationEventData as EditIntegrationEvent
import Wizard.Api.Models.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData
import Wizard.Api.Models.Event.EditMetricEventData as EditMetricEventData
import Wizard.Api.Models.Event.EditPhaseEventData as EditPhaseEventData
import Wizard.Api.Models.Event.EditQuestionEventData as EditQuestionEventData
import Wizard.Api.Models.Event.EditReferenceEventData as EditReferenceEventData
import Wizard.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData
import Wizard.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData
import Wizard.Api.Models.Event.EditTagEventData as EditTagEventData
import Wizard.Api.Models.Event.EventContent exposing (EventContent(..))
import Wizard.Api.Models.Event.MoveEventData exposing (MoveEventData)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert as Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.ProjectDetail.Reply exposing (Reply)
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes


type alias EditorContext =
    { kmEditor : KnowledgeModelEditorDetail
    , parentMap : KnowledgeModel.ParentMap
    , activeUuid : String
    , openNodeUuids : List String
    , addedUuids : List String
    , editedUuids : List String
    , deletedUuids : List String
    , emptyIntegrationEditorUuids : Set String
    , warnings : List EditorContextWarning
    }


type alias EditorContextWarning =
    { editorUuid : String
    , message : String
    }


init : AppState -> List String -> KnowledgeModelEditorDetail -> Maybe Uuid -> EditorContext
init appState secrets kmEditor mbEditorUuid =
    let
        kmUuid =
            Uuid.toString kmEditor.knowledgeModel.uuid

        editorContext =
            { kmEditor = kmEditor
            , parentMap = KnowledgeModel.createParentMap kmEditor.knowledgeModel
            , activeUuid = kmUuid
            , openNodeUuids = [ kmUuid ]
            , addedUuids = []
            , editedUuids = []
            , deletedUuids = []
            , emptyIntegrationEditorUuids = Set.empty
            , warnings = []
            }
    in
    List.foldl (applyEvent False) editorContext editorContext.kmEditor.events
        |> setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)
        |> computeWarnings appState secrets


setReplies : Dict String Reply -> EditorContext -> EditorContext
setReplies replies editorContext =
    let
        kmEditor =
            editorContext.kmEditor
    in
    { editorContext | kmEditor = { kmEditor | replies = replies } }


getEditUuid : String -> EditorContext -> Maybe Uuid
getEditUuid entityUuidString editorContext =
    let
        entityUuid =
            Uuid.fromUuidString entityUuidString
    in
    if entityUuid == editorContext.kmEditor.knowledgeModel.uuid then
        Nothing

    else
        Just entityUuid


getParentUuid : String -> EditorContext -> String
getParentUuid uuid editorContext =
    Maybe.withDefault "" (Dict.get uuid editorContext.parentMap)


editorRoute : EditorContext -> String -> Routes.Route
editorRoute editorContext entityUuidString =
    Routes.kmEditorEditor editorContext.kmEditor.uuid (getEditUuid entityUuidString editorContext)


filterDeleted : EditorContext -> List String -> List String
filterDeleted =
    filterDeletedWith identity


filterDeletedWith : (a -> String) -> EditorContext -> List a -> List a
filterDeletedWith toUuid editorContext =
    let
        allUuids =
            getAllUuids editorContext

        isGood item =
            let
                uuid =
                    toUuid item
            in
            List.member uuid allUuids && not (isDeleted uuid editorContext)
    in
    List.filter isGood


filterExistingChapters : EditorContext -> List String -> List String
filterExistingChapters editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.chapters


filterExistingQuestions : EditorContext -> List String -> List String
filterExistingQuestions editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.questions


filterExistingAnswers : EditorContext -> List String -> List String
filterExistingAnswers editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.answers


filterExistingChoices : EditorContext -> List String -> List String
filterExistingChoices editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.choices


filterExistingExperts : EditorContext -> List String -> List String
filterExistingExperts editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.experts


filterExistingReferences : EditorContext -> List String -> List String
filterExistingReferences editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.references


filterExistingIntegrations : EditorContext -> List String -> List String
filterExistingIntegrations editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.integrations


filterExistingResourceCollections : EditorContext -> List String -> List String
filterExistingResourceCollections editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.resourceCollections


filterExistingResourcePages : EditorContext -> List String -> List String
filterExistingResourcePages editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.resourcePages


filterExistingTags : EditorContext -> List String -> List String
filterExistingTags editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.tags


filterExistingMetrics : EditorContext -> List String -> List String
filterExistingMetrics editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.metrics


filterExistingPhases : EditorContext -> List String -> List String
filterExistingPhases editorContext =
    filterExisting editorContext.kmEditor.knowledgeModel.entities.phases


filterExisting : Dict String a -> List String -> List String
filterExisting entitiesDict =
    List.filter (\entityUuid -> Dict.member entityUuid entitiesDict)


isQuestionDeletedInHierarchy : String -> EditorContext -> Bool
isQuestionDeletedInHierarchy questionUuid editorContext =
    if isDeleted questionUuid editorContext then
        True

    else
        let
            parentUuid =
                getParentUuid questionUuid editorContext
        in
        if parentUuid == Uuid.toString editorContext.kmEditor.knowledgeModel.uuid then
            False

        else if Maybe.isJust (KnowledgeModel.getQuestion parentUuid editorContext.kmEditor.knowledgeModel) then
            isQuestionDeletedInHierarchy parentUuid editorContext

        else if Maybe.isJust (KnowledgeModel.getChapter parentUuid editorContext.kmEditor.knowledgeModel) then
            isDeleted parentUuid editorContext

        else if Maybe.isJust (KnowledgeModel.getAnswer parentUuid editorContext.kmEditor.knowledgeModel) then
            if isDeleted parentUuid editorContext then
                True

            else
                isQuestionDeletedInHierarchy (getParentUuid parentUuid editorContext) editorContext

        else
            True


getFilteredKM : EditorContext -> KnowledgeModel
getFilteredKM editorContext =
    let
        knowledgeModel =
            editorContext.kmEditor.knowledgeModel

        knowledgeModelEntities =
            knowledgeModel.entities

        filterChapter _ chapter =
            { chapter | questionUuids = filterDeleted editorContext chapter.questionUuids }

        filterQuestion _ question =
            let
                filterCommonData commonData =
                    { commonData
                        | tagUuids = filterDeleted editorContext commonData.tagUuids
                        , referenceUuids = filterDeleted editorContext commonData.referenceUuids
                        , expertUuids = filterDeleted editorContext commonData.expertUuids
                    }
            in
            case question of
                OptionsQuestion commonData optionsData ->
                    OptionsQuestion (filterCommonData commonData)
                        { optionsData | answerUuids = filterDeleted editorContext optionsData.answerUuids }

                ListQuestion commonData listData ->
                    ListQuestion (filterCommonData commonData)
                        { listData | itemTemplateQuestionUuids = filterDeleted editorContext listData.itemTemplateQuestionUuids }

                ValueQuestion commonData valueData ->
                    ValueQuestion (filterCommonData commonData) valueData

                IntegrationQuestion commonData integrationData ->
                    IntegrationQuestion (filterCommonData commonData) integrationData

                MultiChoiceQuestion commonData multichoiceData ->
                    MultiChoiceQuestion (filterCommonData commonData)
                        { multichoiceData | choiceUuids = filterDeleted editorContext multichoiceData.choiceUuids }

                ItemSelectQuestion commonData itemSelectData ->
                    ItemSelectQuestion (filterCommonData commonData) itemSelectData

                FileQuestion commonData fileData ->
                    FileQuestion (filterCommonData commonData) fileData

        filterAnswer _ answer =
            { answer | followUpUuids = filterDeleted editorContext answer.followUpUuids }

        filterResourceCollection _ resourceCollection =
            { resourceCollection | resourcePageUuids = filterDeleted editorContext resourceCollection.resourcePageUuids }

        entities =
            { knowledgeModelEntities
                | chapters = Dict.map filterChapter knowledgeModelEntities.chapters
                , questions = Dict.map filterQuestion knowledgeModelEntities.questions
                , answers = Dict.map filterAnswer knowledgeModelEntities.answers
                , resourceCollections = Dict.map filterResourceCollection knowledgeModelEntities.resourceCollections
            }
    in
    { knowledgeModel
        | chapterUuids = filterDeleted editorContext knowledgeModel.chapterUuids
        , tagUuids = filterDeleted editorContext knowledgeModel.tagUuids
        , integrationUuids = filterDeleted editorContext knowledgeModel.integrationUuids
        , metricUuids = filterDeleted editorContext knowledgeModel.metricUuids
        , phaseUuids = filterDeleted editorContext knowledgeModel.phaseUuids
        , resourceCollectionUuids = filterDeleted editorContext knowledgeModel.resourceCollectionUuids
        , entities = entities
    }


sortDeleted : (a -> String) -> EditorContext -> List a -> List a
sortDeleted toUuid editorContext items =
    let
        ( currentItems, deletedItems ) =
            List.partition (not << flip isDeleted editorContext << toUuid) items
    in
    currentItems ++ deletedItems


setParent : String -> String -> EditorContext -> EditorContext
setParent entityUuid parentUuid editorContext =
    { editorContext | parentMap = Dict.insert entityUuid parentUuid editorContext.parentMap }


setKnowledgeModel : KnowledgeModel -> EditorContext -> EditorContext
setKnowledgeModel km editorContext =
    let
        kmEditor =
            editorContext.kmEditor
    in
    { editorContext | kmEditor = { kmEditor | knowledgeModel = km } }


isReachable : EditorContext -> String -> Bool
isReachable editorContext entityUuid =
    let
        parentUuid =
            getParentUuid entityUuid editorContext

        getEntity getter isEntityReachable =
            getter editorContext.kmEditor.knowledgeModel.entities
                |> Dict.get parentUuid
                |> Maybe.map isEntityReachable

        isInChapterReachable : Chapter -> Bool
        isInChapterReachable chapter =
            List.member entityUuid chapter.questionUuids && isReachable editorContext chapter.uuid

        isInQuestionReachable : Question -> Bool
        isInQuestionReachable question =
            case question of
                OptionsQuestion _ data ->
                    List.member entityUuid data.answerUuids && isReachable editorContext parentUuid

                ListQuestion _ data ->
                    List.member entityUuid data.itemTemplateQuestionUuids && isReachable editorContext parentUuid

                MultiChoiceQuestion _ data ->
                    List.member entityUuid data.choiceUuids && isReachable editorContext parentUuid

                _ ->
                    False

        isInAnswerReachable : Answer -> Bool
        isInAnswerReachable answer =
            List.member entityUuid answer.followUpUuids && isReachable editorContext parentUuid
    in
    if isDeleted entityUuid editorContext then
        False

    else if parentUuid == Uuid.toString editorContext.kmEditor.knowledgeModel.uuid then
        True

    else
        getEntity .chapters isInChapterReachable
            |> Maybe.orElse (getEntity .questions isInQuestionReachable)
            |> Maybe.orElse (getEntity .answers isInAnswerReachable)
            |> Maybe.withDefault False


getEditorName : AppState -> String -> EditorContext -> String
getEditorName appState uuid editorContext =
    let
        getEditorName_ getEntityName getEntity =
            Maybe.map getEntityName (getEntity uuid editorContext.kmEditor.knowledgeModel)

        getKnowledgeModelName =
            if uuid == Uuid.toString editorContext.kmEditor.knowledgeModel.uuid then
                Just editorContext.kmEditor.name

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
            getEditorName_ (String.withDefault (gettext "Untitled reference" appState.locale) << Reference.getVisibleName (KnowledgeModel.getAllQuestions editorContext.kmEditor.knowledgeModel) (KnowledgeModel.getAllResourcePages editorContext.kmEditor.knowledgeModel)) KnowledgeModel.getReference

        getExpertName =
            getEditorName_ (String.withDefault (gettext "Untitled expert" appState.locale) << Expert.getVisibleName) KnowledgeModel.getExpert

        getResourceCollectionName =
            getEditorName_ (String.withDefault (gettext "Untitled resource collection" appState.locale) << .title) KnowledgeModel.getResourceCollection

        getResourcePageName =
            getEditorName_ (String.withDefault (gettext "Untitled resource page" appState.locale) << .title) KnowledgeModel.getResourcePage
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
        |> Maybe.orElse getResourceCollectionName
        |> Maybe.orElse getResourcePageName
        |> Maybe.withDefault ""


setActiveEditor : Maybe String -> EditorContext -> EditorContext
setActiveEditor mbEditorUuid editorContext =
    let
        kmUuid =
            Uuid.toString editorContext.kmEditor.knowledgeModel.uuid

        activeUuid =
            Maybe.withDefault kmUuid mbEditorUuid

        getParents childUuid =
            case Dict.get childUuid editorContext.parentMap of
                Just parent ->
                    childUuid :: getParents parent

                Nothing ->
                    [ childUuid ]
    in
    { editorContext
        | activeUuid = activeUuid
        , openNodeUuids = List.unique (List.drop 1 (getParents activeUuid) ++ editorContext.openNodeUuids)
    }


updateActiveEditor : EditorContext -> EditorContext
updateActiveEditor editorContext =
    let
        editorIsDeleted uuid =
            isDeleted uuid editorContext

        editorHasChild parentUuid childUuid =
            case KnowledgeModel.getQuestion parentUuid editorContext.kmEditor.knowledgeModel of
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
                        Maybe.withDefault (Uuid.toString Uuid.nil) (Dict.get currentUuid editorContext.parentMap)
                in
                if editorIsDeleted currentUuid || not (editorHasChild parentUuid currentUuid) then
                    getActiveEditor parentUuid parentUuid

                else
                    getActiveEditor parentUuid activeEditorUuid

        newActiveEditorUuid =
            getActiveEditor editorContext.activeUuid editorContext.activeUuid
    in
    setActiveEditor newActiveEditorUuid editorContext


getActiveQuestionUuid : EditorContext -> String
getActiveQuestionUuid editorContext =
    let
        isQuestionEditor uuid =
            Dict.member uuid editorContext.kmEditor.knowledgeModel.entities.questions

        getParentQuestion uuid =
            if String.isEmpty uuid || isQuestionEditor uuid then
                uuid

            else
                getParentQuestion (getParentUuid uuid editorContext)
    in
    getParentQuestion editorContext.activeUuid


getChapterUuid : String -> EditorContext -> String
getChapterUuid entityUuid editorContext =
    let
        isChapter uuid =
            List.member uuid editorContext.kmEditor.knowledgeModel.chapterUuids

        getParent uuid =
            if String.isEmpty uuid || isChapter uuid then
                uuid

            else
                getParent (getParentUuid uuid editorContext)
    in
    getParent entityUuid


treeSetNodeOpen : String -> Bool -> EditorContext -> EditorContext
treeSetNodeOpen entityUuid open editorContext =
    let
        openUuids =
            if open then
                entityUuid :: editorContext.openNodeUuids

            else
                List.filter ((/=) entityUuid) editorContext.openNodeUuids
    in
    { editorContext | openNodeUuids = openUuids }


treeIsNodeOpen : String -> EditorContext -> Bool
treeIsNodeOpen entityUuid editorContext =
    List.member entityUuid editorContext.openNodeUuids


getAllUuids : EditorContext -> List String
getAllUuids editorContext =
    Uuid.toString editorContext.kmEditor.knowledgeModel.uuid
        :: Dict.keys editorContext.kmEditor.knowledgeModel.entities.chapters
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.questions
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.answers
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.choices
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.experts
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.references
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.integrations
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.resourceCollections
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.resourcePages
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.tags
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.metrics
        ++ Dict.keys editorContext.kmEditor.knowledgeModel.entities.phases


treeExpandAll : EditorContext -> EditorContext
treeExpandAll editorContext =
    { editorContext | openNodeUuids = getAllUuids editorContext }


treeCollapseAll : EditorContext -> EditorContext
treeCollapseAll editorContext =
    { editorContext | openNodeUuids = [] }


isActive : String -> EditorContext -> Bool
isActive activeEditor editorContext =
    editorContext.activeUuid == activeEditor


setEdited : String -> EditorContext -> EditorContext
setEdited uuid editorContext =
    if List.member uuid editorContext.editedUuids then
        editorContext

    else
        { editorContext | editedUuids = uuid :: editorContext.editedUuids }


isEdited : String -> EditorContext -> Bool
isEdited uuid editorContext =
    List.member uuid editorContext.editedUuids && not (isAdded uuid editorContext) && not (isDeleted uuid editorContext)


setDeleted : String -> EditorContext -> EditorContext
setDeleted uuid editorContext =
    { editorContext | deletedUuids = uuid :: editorContext.deletedUuids }


isDeleted : String -> EditorContext -> Bool
isDeleted uuid editorContext =
    List.member uuid editorContext.deletedUuids


setAdded : String -> EditorContext -> EditorContext
setAdded uuid editorContext =
    { editorContext | addedUuids = uuid :: editorContext.addedUuids }


isAdded : String -> EditorContext -> Bool
isAdded uuid editorContext =
    List.member uuid editorContext.addedUuids && not (isDeleted uuid editorContext)


addEmptyIntegrationEditorUuid : String -> EditorContext -> EditorContext
addEmptyIntegrationEditorUuid uuid editorContext =
    { editorContext | emptyIntegrationEditorUuids = Set.insert uuid editorContext.emptyIntegrationEditorUuids }


removeEmptyIntegrationEditorUuid : String -> EditorContext -> EditorContext
removeEmptyIntegrationEditorUuid uuid editorContext =
    { editorContext | emptyIntegrationEditorUuids = Set.remove uuid editorContext.emptyIntegrationEditorUuids }


isEmptyIntegrationEditorUuid : String -> EditorContext -> Bool
isEmptyIntegrationEditorUuid uuid editorContext =
    Set.member uuid editorContext.emptyIntegrationEditorUuids


applyEvent : Bool -> Event -> EditorContext -> EditorContext
applyEvent local event originalEditorContext =
    let
        kmEditor =
            originalEditorContext.kmEditor

        knowledgeModel =
            kmEditor.knowledgeModel

        editorContext =
            { originalEditorContext | kmEditor = { kmEditor | events = kmEditor.events ++ [ event ] } }
    in
    case event.content of
        AddKnowledgeModelEvent _ ->
            editorContext

        AddAnswerEvent eventData ->
            let
                answer =
                    AddAnswerEventData.toAnswer event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertAnswer answer event editorContext

        AddChapterEvent eventData ->
            let
                chapter =
                    AddChapterEventData.toChapter event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertChapter chapter event editorContext

        AddChoiceEvent eventData ->
            let
                choice =
                    AddChoiceEventData.toChoice event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertChoice choice event editorContext

        AddExpertEvent eventData ->
            let
                expert =
                    AddExpertEventData.toExpert event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertExpert expert event editorContext

        AddIntegrationEvent eventData ->
            let
                integration =
                    AddIntegrationEventData.toIntegration event.entityUuid eventData

                updatedEditorContext =
                    addEmptyIntegrationEditorUuid (Integration.getUuid integration) editorContext
            in
            applyAdd local KnowledgeModel.insertIntegration integration event updatedEditorContext

        AddMetricEvent eventData ->
            let
                metric =
                    AddMetricEventData.toMetric event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertMetric metric event editorContext

        AddPhaseEvent eventData ->
            let
                phase =
                    AddPhaseEventData.toPhase event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertPhase phase event editorContext

        AddQuestionEvent eventData ->
            let
                question =
                    AddQuestionEventData.toQuestion event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertQuestion question event editorContext

        AddReferenceEvent eventData ->
            let
                reference =
                    AddReferenceEventData.toReference event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertReference reference event editorContext

        AddResourceCollectionEvent eventData ->
            let
                resourceCollection =
                    AddResourceCollectionEventData.toResourceCollection event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertResourceCollection resourceCollection event editorContext

        AddResourcePageEvent eventData ->
            let
                resourcePage =
                    AddResourcePageEventData.toResourcePage event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertResourcePage resourcePage event editorContext

        AddTagEvent eventData ->
            let
                tag =
                    AddTagEventData.toTag event.entityUuid eventData
            in
            applyAdd local KnowledgeModel.insertTag tag event editorContext

        EditAnswerEvent eventData ->
            let
                mbAnswer =
                    KnowledgeModel.getAnswer event.entityUuid knowledgeModel
                        |> Maybe.map (EditAnswerEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateAnswer mbAnswer event editorContext

        EditChapterEvent eventData ->
            let
                mbChapter =
                    KnowledgeModel.getChapter event.entityUuid knowledgeModel
                        |> Maybe.map (EditChapterEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateChapter mbChapter event editorContext

        EditChoiceEvent eventData ->
            let
                mbChoice =
                    KnowledgeModel.getChoice event.entityUuid knowledgeModel
                        |> Maybe.map (EditChoiceEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateChoice mbChoice event editorContext

        EditExpertEvent eventData ->
            let
                mbExpert =
                    KnowledgeModel.getExpert event.entityUuid knowledgeModel
                        |> Maybe.map (EditExpertEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateExpert mbExpert event editorContext

        EditIntegrationEvent eventData ->
            let
                mbIntegration =
                    KnowledgeModel.getIntegration event.entityUuid knowledgeModel
                        |> Maybe.map (EditIntegrationEvent.apply eventData)

                updatedEditorContext =
                    removeEmptyIntegrationEditorUuid (Maybe.unwrap "" Integration.getUuid mbIntegration) editorContext
            in
            applyEdit KnowledgeModel.updateIntegration mbIntegration event updatedEditorContext

        EditKnowledgeModelEvent eventData ->
            let
                newKnowledgeModel =
                    EditKnowledgeModelEventData.apply eventData knowledgeModel
            in
            setKnowledgeModel newKnowledgeModel editorContext
                |> setEdited (Uuid.toString knowledgeModel.uuid)

        EditMetricEvent eventData ->
            let
                mbMetric =
                    KnowledgeModel.getMetric event.entityUuid knowledgeModel
                        |> Maybe.map (EditMetricEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateMetric mbMetric event editorContext

        EditPhaseEvent eventData ->
            let
                mbPhase =
                    KnowledgeModel.getPhase event.entityUuid knowledgeModel
                        |> Maybe.map (EditPhaseEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updatePhase mbPhase event editorContext

        EditQuestionEvent eventData ->
            let
                mbQuestion =
                    KnowledgeModel.getQuestion event.entityUuid knowledgeModel
                        |> Maybe.map (EditQuestionEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateQuestion mbQuestion event editorContext

        EditReferenceEvent eventData ->
            let
                mbReference =
                    KnowledgeModel.getReference event.entityUuid knowledgeModel
                        |> Maybe.map (EditReferenceEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateReference mbReference event editorContext

        EditResourceCollectionEvent eventData ->
            let
                mbResourceCollection =
                    KnowledgeModel.getResourceCollection event.entityUuid knowledgeModel
                        |> Maybe.map (EditResourceCollectionEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateResourceCollection mbResourceCollection event editorContext

        EditResourcePageEvent eventData ->
            let
                mbResourcePage =
                    KnowledgeModel.getResourcePage event.entityUuid knowledgeModel
                        |> Maybe.map (EditResourcePageEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateResourcePage mbResourcePage event editorContext

        EditTagEvent eventData ->
            let
                mbTag =
                    KnowledgeModel.getTag event.entityUuid knowledgeModel
                        |> Maybe.map (EditTagEventData.apply eventData)
            in
            applyEdit KnowledgeModel.updateTag mbTag event editorContext

        DeleteAnswerEvent ->
            applyDelete event editorContext

        DeleteChapterEvent ->
            applyDelete event editorContext

        DeleteChoiceEvent ->
            applyDelete event editorContext

        DeleteExpertEvent ->
            applyDelete event editorContext

        DeleteIntegrationEvent ->
            applyDelete event editorContext

        DeleteMetricEvent ->
            applyDelete event editorContext

        DeletePhaseEvent ->
            applyDelete event editorContext

        DeleteReferenceEvent ->
            applyDelete event editorContext

        DeleteResourceCollectionEvent ->
            applyDelete event editorContext

        DeleteResourcePageEvent ->
            applyDelete event editorContext

        DeleteQuestionEvent ->
            applyDelete event editorContext

        DeleteTagEvent ->
            applyDelete event editorContext

        MoveQuestionEvent moveData ->
            applyMove local KnowledgeModel.moveQuestion KnowledgeModel.getQuestion event moveData editorContext

        MoveAnswerEvent moveData ->
            applyMove local KnowledgeModel.moveAnswer KnowledgeModel.getAnswer event moveData editorContext

        MoveChoiceEvent moveData ->
            applyMove local KnowledgeModel.moveChoice KnowledgeModel.getChoice event moveData editorContext

        MoveReferenceEvent moveData ->
            applyMove local KnowledgeModel.moveReference KnowledgeModel.getReference event moveData editorContext

        MoveExpertEvent moveData ->
            applyMove local KnowledgeModel.moveExpert KnowledgeModel.getExpert event moveData editorContext


applyAdd : Bool -> (a -> String -> KnowledgeModel -> KnowledgeModel) -> a -> Event -> EditorContext -> EditorContext
applyAdd local updateKm entity { entityUuid, parentUuid } editorContext =
    let
        openAddedEditor uuid eb =
            if local then
                setActiveEditor (Just uuid) eb

            else
                eb

        newKnowledgeModel =
            updateKm entity parentUuid editorContext.kmEditor.knowledgeModel
    in
    editorContext
        |> setKnowledgeModel newKnowledgeModel
        |> setParent entityUuid parentUuid
        |> setAdded entityUuid
        |> openAddedEditor entityUuid


applyEdit : (a -> KnowledgeModel -> KnowledgeModel) -> Maybe a -> Event -> EditorContext -> EditorContext
applyEdit updateKm mbEntity { entityUuid } editorContext =
    case mbEntity of
        Just entity ->
            editorContext
                |> setKnowledgeModel (updateKm entity editorContext.kmEditor.knowledgeModel)
                |> setEdited entityUuid
                |> updateActiveEditor

        Nothing ->
            editorContext


applyDelete : Event -> EditorContext -> EditorContext
applyDelete { entityUuid } editorContext =
    editorContext
        |> setDeleted entityUuid
        |> updateActiveEditor


applyMove : Bool -> (a -> String -> String -> KnowledgeModel -> KnowledgeModel) -> (String -> KnowledgeModel -> Maybe a) -> Event -> MoveEventData -> EditorContext -> EditorContext
applyMove local updateKm getEntity { entityUuid, parentUuid } { targetUuid } editorContext =
    case getEntity entityUuid editorContext.kmEditor.knowledgeModel of
        Just entity ->
            let
                setActiveEditorIfLocal =
                    if local then
                        setActiveEditor (Just entityUuid)

                    else
                        identity
            in
            editorContext
                |> setKnowledgeModel (updateKm entity parentUuid targetUuid editorContext.kmEditor.knowledgeModel)
                |> setParent entityUuid targetUuid
                |> setEdited entityUuid
                |> setActiveEditorIfLocal

        Nothing ->
            editorContext


computeWarnings : AppState -> List String -> EditorContext -> EditorContext
computeWarnings appState secrets editorContext =
    let
        filteredKM =
            getFilteredKM editorContext

        warnings =
            List.concatMap (computeChapterWarnings appState editorContext filteredKM) (KnowledgeModel.getChapters filteredKM)
                |> flip (++) (List.concatMap (computeMetricWarnings appState) (KnowledgeModel.getMetrics filteredKM))
                |> flip (++) (List.concatMap (computePhaseWarnings appState) (KnowledgeModel.getPhases filteredKM))
                |> flip (++) (List.concatMap (computeTagWarnings appState) (KnowledgeModel.getTags filteredKM))
                |> flip (++) (List.concatMap (computeIntegrationWarnings appState secrets) (KnowledgeModel.getIntegrations filteredKM))
                |> flip (++) (List.concatMap (computeResourceCollectionWarnings appState filteredKM) (KnowledgeModel.getResourceCollections filteredKM))
    in
    { editorContext | warnings = warnings }


computeChapterWarnings : AppState -> EditorContext -> KnowledgeModel -> Chapter -> List EditorContextWarning
computeChapterWarnings appState editorContext km chapter =
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
                (computeQuestionWarnings appState editorContext km)
                (KnowledgeModel.getChapterQuestions chapter.uuid km)
    in
    titleWarning ++ questionWarnings


computeQuestionWarnings : AppState -> EditorContext -> KnowledgeModel -> Question -> List EditorContextWarning
computeQuestionWarnings appState editorContext km question =
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
                            (computeAnswerWarnings appState editorContext km)
                            (KnowledgeModel.getQuestionAnswers questionUuid km)

                Question.ListQuestion _ data ->
                    if List.isEmpty data.itemTemplateQuestionUuids then
                        createError (gettext "No item questions for list question" appState.locale)

                    else
                        List.concatMap
                            (computeQuestionWarnings appState editorContext km)
                            (KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km)

                Question.IntegrationQuestion _ data ->
                    let
                        integrationUuidWarning =
                            if data.integrationUuid == Uuid.toString Uuid.nil then
                                createError (gettext "No integration selected for integration question" appState.locale)

                            else
                                []

                        variablesWarning =
                            if data.integrationUuid /= Uuid.toString Uuid.nil then
                                case KnowledgeModel.getIntegration data.integrationUuid km of
                                    Just integration ->
                                        let
                                            missingVariables =
                                                List.any (Maybe.isNothing << flip Dict.get data.variables) (Integration.getVariables integration)
                                        in
                                        if missingVariables then
                                            createError (gettext "Missing variables for integration question" appState.locale)

                                        else
                                            []

                                    Nothing ->
                                        []

                            else
                                []
                    in
                    integrationUuidWarning ++ variablesWarning

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
                                    isDeleted listQuestionUuid editorContext

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


computeAnswerWarnings : AppState -> EditorContext -> KnowledgeModel -> Answer -> List EditorContextWarning
computeAnswerWarnings appState editorContext km answer =
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
                (computeQuestionWarnings appState editorContext km)
                (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km)
    in
    labelWarning ++ followUpQuestionsWarnings


computeChoiceWarnings : AppState -> Choice -> List EditorContextWarning
computeChoiceWarnings appState choice =
    if String.isEmpty choice.label then
        [ { editorUuid = choice.uuid
          , message = gettext "Empty label for choice" appState.locale
          }
        ]

    else
        []


computeReferenceWarnings : AppState -> Reference -> List EditorContextWarning
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


computeExpertWarnings : AppState -> Expert -> List EditorContextWarning
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


computeMetricWarnings : AppState -> Metric -> List EditorContextWarning
computeMetricWarnings appState metric =
    if String.isEmpty metric.title then
        [ { editorUuid = metric.uuid
          , message = gettext "Empty title for metric" appState.locale
          }
        ]

    else
        []


computePhaseWarnings : AppState -> Phase -> List EditorContextWarning
computePhaseWarnings appState phase =
    if String.isEmpty phase.title then
        [ { editorUuid = phase.uuid
          , message = gettext "Empty title for phase" appState.locale
          }
        ]

    else
        []


computeTagWarnings : AppState -> Tag -> List EditorContextWarning
computeTagWarnings appState tag =
    if String.isEmpty tag.name then
        [ { editorUuid = tag.uuid
          , message = gettext "Empty name for tag" appState.locale
          }
        ]

    else
        []


computeIntegrationWarnings : AppState -> List String -> Integration -> List EditorContextWarning
computeIntegrationWarnings appState secrets integration =
    let
        createError message =
            [ { editorUuid = Integration.getUuid integration
              , message = message
              }
            ]
    in
    case integration of
        Integration.ApiIntegration data ->
            let
                nameWarning =
                    if String.isEmpty (Integration.getName integration) then
                        createError (gettext "Empty name for integration" appState.locale)

                    else
                        []

                variablesWarning =
                    if List.any String.isEmpty data.variables then
                        createError (gettext "Empty variable name for integration" appState.locale)

                    else
                        []

                urlWarning =
                    if String.isEmpty data.requestUrl then
                        createError (gettext "Empty request URL for integration" appState.locale)

                    else
                        let
                            result =
                                JinjaUtils.parseJinja data.requestUrl

                            unknownVariables =
                                ApiIntegrationData.getUnknownVariables result secrets data

                            missingQWarning =
                                if not (List.member "q" result.properties) then
                                    createError (gettext "Missing {{ q }} in request URL for integration" appState.locale)

                                else
                                    []

                            unknownPropertyWarning =
                                if not (List.isEmpty unknownVariables.properties) then
                                    createError (String.format (gettext "Unknown Jinja property in request URL for integration: %s" appState.locale) [ String.join ", " unknownVariables.properties ])

                                else
                                    []

                            unknownVariableWarning =
                                if not (List.isEmpty unknownVariables.variables) then
                                    createError (String.format (gettext "Unknown variable in request URL for integration: %s" appState.locale) [ String.join ", " unknownVariables.variables ])

                                else
                                    []

                            unknownSecretWarning =
                                if not (List.isEmpty unknownVariables.secrets) then
                                    createError (String.format (gettext "Unknown secret in request URL for integration: %s" appState.locale) [ String.join ", " unknownVariables.secrets ])

                                else
                                    []
                        in
                        missingQWarning ++ unknownPropertyWarning ++ unknownVariableWarning ++ unknownSecretWarning

                ( testDataLoaded, testDataWarning ) =
                    case data.testResponse of
                        Nothing ->
                            ( False, createError (gettext "No test data loaded for integration" appState.locale) )

                        Just testData ->
                            case testData.response of
                                TypeHintTestResponse.SuccessTypeHintResponse response ->
                                    let
                                        responseListFieldWarning =
                                            case data.responseListField of
                                                Just responseListField ->
                                                    case TypeHintTestResponse.checkListField responseListField response of
                                                        TypeHintTestResponse.CheckListFieldResultNotFound ->
                                                            createError (gettext "Response list field not found in test data for integration" appState.locale)

                                                        TypeHintTestResponse.CheckListFieldResultNoList ->
                                                            createError (gettext "Response list field is not a list in test data for integration" appState.locale)

                                                        TypeHintTestResponse.CheckListFieldResultOk ->
                                                            []

                                                Nothing ->
                                                    []
                                    in
                                    ( True, responseListFieldWarning )

                                _ ->
                                    ( False, createError (gettext "No test data success response for integration" appState.locale) )

                itemTemplateWarning =
                    if testDataLoaded && String.isEmpty data.responseItemTemplate then
                        createError (gettext "Empty response item template for integration" appState.locale)

                    else
                        []
            in
            nameWarning ++ variablesWarning ++ urlWarning ++ testDataWarning ++ itemTemplateWarning

        Integration.ApiLegacyIntegration _ data ->
            let
                idWarning =
                    if String.isEmpty (Integration.getId integration) then
                        createError (gettext "Empty ID for integration" appState.locale)

                    else
                        []

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
            idWarning ++ urlError ++ requestMethod ++ responseItemTemplate

        Integration.WidgetIntegration _ data ->
            let
                idWarning =
                    if String.isEmpty (Integration.getId integration) then
                        createError (gettext "Empty ID for integration" appState.locale)

                    else
                        []

                widgetUrlWarning =
                    if String.isEmpty data.widgetUrl then
                        createError (gettext "Empty widget URL for integration" appState.locale)

                    else
                        []
            in
            idWarning ++ widgetUrlWarning


computeResourceCollectionWarnings : AppState -> KnowledgeModel -> ResourceCollection -> List EditorContextWarning
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


computeResourcePageWarnings : AppState -> ResourcePage -> List EditorContextWarning
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
