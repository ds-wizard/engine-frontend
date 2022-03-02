module Wizard.KMEditor.Editor.Common.EditorBranch exposing
    ( EditorBranch
    , applyEvent
    , filterDeleted
    , filterDeletedWith
    , getAllUuids
    , getEditUuid
    , getEditorName
    , getFilteredKM
    , getParentUuid
    , init
    , isActive
    , isAdded
    , isDeleted
    , isEdited
    , setActiveEditor
    , sortDeleted
    , treeCollapseAll
    , treeExpandAll
    , treeIsNodeOpen
    , treeSetNodeOpen
    )

import Dict
import List.Extra as List
import Maybe.Extra as Maybe
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
import Shared.Data.Event.EditTagEventData as EditTagEventData
import Shared.Data.Event.MoveEventData exposing (MoveEventData)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Integration as Integration
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Reference as Reference
import Shared.Locale exposing (lg)
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
    }


init : BranchDetail -> Maybe Uuid -> EditorBranch
init branch mbEditorUuid =
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
            }
    in
    setActiveEditor (Maybe.map Uuid.toString mbEditorUuid) <|
        List.foldl (applyEvent False) editorBranch editorBranch.branch.events


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

        filterAnswer _ answer =
            { answer | followUpUuids = filterDeleted editorBranch answer.followUpUuids }

        entities =
            { knowledgeModelEntities
                | chapters = Dict.map filterChapter knowledgeModelEntities.chapters
                , questions = Dict.map filterQuestion knowledgeModelEntities.questions
                , answers = Dict.map filterAnswer knowledgeModelEntities.answers
            }

        filteredKnowledgeModel =
            { knowledgeModel
                | chapterUuids = filterDeleted editorBranch knowledgeModel.chapterUuids
                , tagUuids = filterDeleted editorBranch knowledgeModel.tagUuids
                , integrationUuids = filterDeleted editorBranch knowledgeModel.integrationUuids
                , metricUuids = filterDeleted editorBranch knowledgeModel.metricUuids
                , phaseUuids = filterDeleted editorBranch knowledgeModel.phaseUuids
                , entities = entities
            }
    in
    filteredKnowledgeModel


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
            getEditorName_ (String.withDefault (lg "chapter.untitled" appState) << .title) KnowledgeModel.getChapter

        getQuestionName =
            getEditorName_ (String.withDefault (lg "question.untitled" appState) << Question.getTitle) KnowledgeModel.getQuestion

        getMetricName =
            getEditorName_ (String.withDefault (lg "metric.untitled" appState) << .title) KnowledgeModel.getMetric

        getPhaseName =
            getEditorName_ (String.withDefault (lg "phase.untitled" appState) << .title) KnowledgeModel.getPhase

        getTagName =
            getEditorName_ (String.withDefault (lg "tag.untitled" appState) << .name) KnowledgeModel.getTag

        getIntegrationName =
            getEditorName_ (String.withDefault (lg "integration.untitled" appState) << Integration.getName) KnowledgeModel.getIntegration

        getAnswerName =
            getEditorName_ (String.withDefault (lg "answer.untitled" appState) << .label) KnowledgeModel.getAnswer

        getChoiceName =
            getEditorName_ (String.withDefault (lg "choice.untitled" appState) << .label) KnowledgeModel.getChoice

        getReferenceName =
            getEditorName_ (String.withDefault (lg "reference.untitled" appState) << Reference.getVisibleName) KnowledgeModel.getReference

        getExpertName =
            getEditorName_ (String.withDefault (lg "expert.untitled" appState) << .name) KnowledgeModel.getExpert
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


applyEvent : Bool -> Event -> EditorBranch -> EditorBranch
applyEvent local event originalEditorBranch =
    let
        branch =
            originalEditorBranch.branch

        knowledgeModel =
            branch.knowledgeModel

        editorBranch =
            { originalEditorBranch | branch = { branch | events = branch.events ++ [ event ] } }
    in
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
            in
            applyAdd local KnowledgeModel.insertIntegration integration commonData editorBranch

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
            in
            applyEdit KnowledgeModel.updateIntegration mbIntegration commonData editorBranch

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
