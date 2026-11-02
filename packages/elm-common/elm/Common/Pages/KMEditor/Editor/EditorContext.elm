module Common.Pages.KMEditor.Editor.EditorContext exposing
    ( EditorContext
    , Warning
    , applyEvent
    , computeWarnings
    , filterDeleted
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
    , isAddedAndDeleted
    , isDeleted
    , isEdited
    , isEmptyIntegrationEditor
    , isNodeOpen
    , isReachable
    , removeEmptyIntegrationEditor
    , setActiveEditor
    , setNodeOpen
    , setReplies
    , sortDeleted
    , treeCollapseAll
    , treeExpandAll
    )

{-| The editor's memory: the knowledge model as it stands, and what has been done
to it since it was published.

The client is **event sourced.** The server sends the knowledge model plus the
history of events made against it, and this folds the history over the model —
the same fold that later applies each new event as it arrives, so an event just
made and an event replayed after a reload go through one code path.

**Delete is soft.** A deleted entity stays in the dictionaries and its uuid goes
on `deletedUuids`; every read path filters it out. That is what lets the tree
show it struck through, and what makes an entity that was added and then deleted
disappear entirely rather than leave a hole.

**`local` is the only thing that separates my edit from somebody else's.** It
decides whether the active editor jumps to the entity the event touched. An
event arriving from another editor must never steal the cursor.

There is no undo, no locking and no conflict resolution: last write wins, per
field. That is the server's protocol, and
inventing any of it here would be a bug.

-}

import Common.Api.Models.Event exposing (Event)
import Common.Api.Models.Event.AddAnswerEventData as AddAnswerEventData
import Common.Api.Models.Event.AddChapterEventData as AddChapterEventData
import Common.Api.Models.Event.AddChoiceEventData as AddChoiceEventData
import Common.Api.Models.Event.AddExpertEventData as AddExpertEventData
import Common.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Common.Api.Models.Event.AddMetricEventData as AddMetricEventData
import Common.Api.Models.Event.AddPhaseEventData as AddPhaseEventData
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Common.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData
import Common.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData
import Common.Api.Models.Event.AddTagEventData as AddTagEventData
import Common.Api.Models.Event.EditAnswerEventData as EditAnswerEventData
import Common.Api.Models.Event.EditChapterEventData as EditChapterEventData
import Common.Api.Models.Event.EditChoiceEventData as EditChoiceEventData
import Common.Api.Models.Event.EditExpertEventData as EditExpertEventData
import Common.Api.Models.Event.EditIntegrationEventData as EditIntegrationEventData
import Common.Api.Models.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData
import Common.Api.Models.Event.EditMetricEventData as EditMetricEventData
import Common.Api.Models.Event.EditPhaseEventData as EditPhaseEventData
import Common.Api.Models.Event.EditQuestionEventData as EditQuestionEventData
import Common.Api.Models.Event.EditReferenceEventData as EditReferenceEventData
import Common.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData
import Common.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData
import Common.Api.Models.Event.EditTagEventData as EditTagEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.Event.MoveEventData exposing (MoveEventData)
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Expert as Expert
import Common.Api.Models.KnowledgeModel.Integration as Integration
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Common.Api.Models.KnowledgeModel.Reference as Reference
import Common.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Data.BuiltInIntegration exposing (BuiltInIntegration)
import Common.Pages.KMEditor.Editor.Warnings as Warnings
import Dict
import Gettext
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Uuid exposing (Uuid)


type alias EditorContext =
    { kmEditor : KnowledgeModelEditorDetail
    , parentMap : KnowledgeModel.ParentMap
    , activeUuid : String
    , openNodeUuids : List String
    , addedUuids : List String
    , editedUuids : List String
    , deletedUuids : List String
    , emptyIntegrationEditorUuids : Set String
    , warnings : List Warning
    }


type alias Warning =
    Warnings.Warning


init : KnowledgeModelEditorDetail -> Maybe Uuid -> EditorContext
init kmEditor mbEditorUuid =
    let
        kmUuid =
            Uuid.toString kmEditor.knowledgeModel.uuid

        empty =
            { kmEditor = { kmEditor | events = [] }
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
    List.foldl (applyEvent False) empty kmEditor.events
        |> setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)


{-| The warnings, recomputed from the knowledge model as it stands. It is called
after every event — mine and everybody else's, which the wizard forgets to do,
so its count can go stale when somebody else fixes something.
-}
computeWarnings : { locale : Gettext.Locale, builtIns : List BuiltInIntegration, secrets : List String } -> EditorContext -> EditorContext
computeWarnings config editorContext =
    { editorContext
        | warnings =
            Warnings.compute
                { locale = config.locale
                , builtIns = config.builtIns
                , secrets = config.secrets
                , isDeleted = \uuid -> isDeleted uuid editorContext
                }
                (getFilteredKM editorContext)
    }


setReplies : Dict.Dict String Reply -> EditorContext -> EditorContext
setReplies replies editorContext =
    let
        kmEditor =
            editorContext.kmEditor
    in
    { editorContext | kmEditor = { kmEditor | replies = replies } }


{-| The uuid a route names, which is `Nothing` for the knowledge model itself —
the root is the editor's own page, not a child of it.
-}
getEditUuid : String -> EditorContext -> Maybe Uuid
getEditUuid entityUuid editorContext =
    if Uuid.fromUuidString entityUuid == editorContext.kmEditor.knowledgeModel.uuid then
        Nothing

    else
        Just (Uuid.fromUuidString entityUuid)


getParentUuid : String -> EditorContext -> String
getParentUuid uuid editorContext =
    Maybe.withDefault "" (Dict.get uuid editorContext.parentMap)


{-| What an entity is called, whatever kind it is — what the breadcrumbs read.
An entity nobody has named yet is "Untitled question" rather than blank, so the
trail never has a gap in it.
-}
getEditorName : Gettext.Locale -> String -> EditorContext -> String
getEditorName locale uuid editorContext =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        named untitled toName get =
            get uuid km
                |> Maybe.map toName
                |> Maybe.map
                    (\name ->
                        if String.isEmpty name then
                            Gettext.gettext untitled locale

                        else
                            name
                    )
    in
    (if uuid == Uuid.toString km.uuid then
        Just editorContext.kmEditor.name

     else
        Nothing
    )
        |> Maybe.orElse (named "Untitled chapter" .title KnowledgeModel.getChapter)
        |> Maybe.orElse (named "Untitled question" Question.getTitle KnowledgeModel.getQuestion)
        |> Maybe.orElse (named "Untitled answer" .label KnowledgeModel.getAnswer)
        |> Maybe.orElse (named "Untitled choice" .label KnowledgeModel.getChoice)
        |> Maybe.orElse (named "Untitled reference" (Reference.getVisibleName (KnowledgeModel.getAllQuestions km) (KnowledgeModel.getAllResourcePages km)) KnowledgeModel.getReference)
        |> Maybe.orElse (named "Untitled expert" Expert.getVisibleName KnowledgeModel.getExpert)
        |> Maybe.orElse (named "Untitled metric" .title KnowledgeModel.getMetric)
        |> Maybe.orElse (named "Untitled phase" .title KnowledgeModel.getPhase)
        |> Maybe.orElse (named "Untitled question tag" .name KnowledgeModel.getTag)
        |> Maybe.orElse (named "Untitled integration" Integration.getVisibleName KnowledgeModel.getIntegration)
        |> Maybe.orElse (named "Untitled resource collection" .title KnowledgeModel.getResourceCollection)
        |> Maybe.orElse (named "Untitled resource page" .title KnowledgeModel.getResourcePage)
        |> Maybe.withDefault ""


{-| The question the editor is open on, walking up from whatever is open until
something is a question — an answer, a reference or an expert is edited on a
question, and the Preview tab wants the question it belongs to.

Empty when the walk leaves the questions entirely, which is what happens on a
chapter, a metric or the knowledge model itself. Empty is a fine answer: the
preview opens on a chapter instead.

-}
getActiveQuestionUuid : EditorContext -> String
getActiveQuestionUuid editorContext =
    let
        questions =
            editorContext.kmEditor.knowledgeModel.entities.questions

        walk uuid =
            if String.isEmpty uuid || Dict.member uuid questions then
                uuid

            else
                walk (getParentUuid uuid editorContext)
    in
    walk editorContext.activeUuid


{-| The same walk, looking for the chapter something sits in.
-}
getChapterUuid : String -> EditorContext -> String
getChapterUuid entityUuid editorContext =
    let
        chapterUuids =
            editorContext.kmEditor.knowledgeModel.chapterUuids

        walk uuid =
            if String.isEmpty uuid || List.member uuid chapterUuids then
                uuid

            else
                walk (getParentUuid uuid editorContext)
    in
    walk entityUuid



-- WHAT IS STILL THERE


{-| Uuids that still name something and have not been deleted. Both halves
matter: an event may name an entity that is not in this knowledge model at all.
-}
filterDeleted : EditorContext -> List String -> List String
filterDeleted editorContext =
    let
        all =
            getAllUuids editorContext
    in
    List.filter (\uuid -> List.member uuid all && not (isDeleted uuid editorContext))


{-| Deleted entities last, so the tree reads as what is there followed by what
is going.
-}
sortDeleted : (a -> String) -> EditorContext -> List a -> List a
sortDeleted toUuid editorContext items =
    let
        ( kept, deleted ) =
            List.partition (\item -> not (isDeleted (toUuid item) editorContext)) items
    in
    kept ++ deleted


{-| The knowledge model with everything deleted taken out of it, which is what
the Preview tab and the warnings are computed from.
-}
getFilteredKM : EditorContext -> KnowledgeModel
getFilteredKM editorContext =
    let
        knowledgeModel =
            editorContext.kmEditor.knowledgeModel

        entities =
            knowledgeModel.entities

        keep =
            filterDeleted editorContext

        filterCommonData commonData =
            { commonData
                | tagUuids = keep commonData.tagUuids
                , referenceUuids = keep commonData.referenceUuids
                , expertUuids = keep commonData.expertUuids
            }

        filterQuestion _ question =
            case question of
                OptionsQuestion commonData data ->
                    OptionsQuestion (filterCommonData commonData) { data | answerUuids = keep data.answerUuids }

                ListQuestion commonData data ->
                    ListQuestion (filterCommonData commonData) { data | itemTemplateQuestionUuids = keep data.itemTemplateQuestionUuids }

                MultiChoiceQuestion commonData data ->
                    MultiChoiceQuestion (filterCommonData commonData) { data | choiceUuids = keep data.choiceUuids }

                ValueQuestion commonData data ->
                    ValueQuestion (filterCommonData commonData) data

                IntegrationQuestion commonData data ->
                    IntegrationQuestion (filterCommonData commonData) data

                ItemSelectQuestion commonData data ->
                    ItemSelectQuestion (filterCommonData commonData) data

                FileQuestion commonData data ->
                    FileQuestion (filterCommonData commonData) data
    in
    { knowledgeModel
        | chapterUuids = keep knowledgeModel.chapterUuids
        , tagUuids = keep knowledgeModel.tagUuids
        , integrationUuids = keep knowledgeModel.integrationUuids
        , metricUuids = keep knowledgeModel.metricUuids
        , phaseUuids = keep knowledgeModel.phaseUuids
        , resourceCollectionUuids = keep knowledgeModel.resourceCollectionUuids
        , entities =
            { entities
                | chapters = Dict.map (\_ chapter -> { chapter | questionUuids = keep chapter.questionUuids }) entities.chapters
                , questions = Dict.map filterQuestion entities.questions
                , answers = Dict.map (\_ answer -> { answer | followUpUuids = keep answer.followUpUuids }) entities.answers
                , resourceCollections =
                    Dict.map (\_ collection -> { collection | resourcePageUuids = keep collection.resourcePageUuids })
                        entities.resourceCollections
            }
    }


{-| Whether an entity can still be walked to from the root: it is not deleted,
and every parent between it and the root still lists it.
-}
isReachable : EditorContext -> String -> Bool
isReachable editorContext entityUuid =
    let
        parentUuid =
            getParentUuid entityUuid editorContext

        parentEntity getter isEntityReachable =
            Dict.get parentUuid (getter editorContext.kmEditor.knowledgeModel.entities)
                |> Maybe.map isEntityReachable

        inChapter chapter =
            List.member entityUuid chapter.questionUuids && isReachable editorContext chapter.uuid

        inQuestion question =
            case question of
                OptionsQuestion _ data ->
                    List.member entityUuid data.answerUuids && isReachable editorContext parentUuid

                ListQuestion _ data ->
                    List.member entityUuid data.itemTemplateQuestionUuids && isReachable editorContext parentUuid

                MultiChoiceQuestion _ data ->
                    List.member entityUuid data.choiceUuids && isReachable editorContext parentUuid

                _ ->
                    False

        inAnswer answer =
            List.member entityUuid answer.followUpUuids && isReachable editorContext parentUuid
    in
    if isDeleted entityUuid editorContext then
        False

    else if parentUuid == Uuid.toString editorContext.kmEditor.knowledgeModel.uuid then
        True

    else
        parentEntity .chapters inChapter
            |> Maybe.orElse (parentEntity .questions inQuestion)
            |> Maybe.orElse (parentEntity .answers inAnswer)
            |> Maybe.withDefault False


getAllUuids : EditorContext -> List String
getAllUuids editorContext =
    let
        entities =
            editorContext.kmEditor.knowledgeModel.entities
    in
    Uuid.toString editorContext.kmEditor.knowledgeModel.uuid
        :: Dict.keys entities.chapters
        ++ Dict.keys entities.questions
        ++ Dict.keys entities.answers
        ++ Dict.keys entities.choices
        ++ Dict.keys entities.experts
        ++ Dict.keys entities.references
        ++ Dict.keys entities.integrations
        ++ Dict.keys entities.resourceCollections
        ++ Dict.keys entities.resourcePages
        ++ Dict.keys entities.tags
        ++ Dict.keys entities.metrics
        ++ Dict.keys entities.phases



-- WHERE THE EDITOR IS


{-| Move to an entity, opening every ancestor on the way so the tree shows it.
-}
setActiveEditor : Maybe String -> EditorContext -> EditorContext
setActiveEditor mbEditorUuid editorContext =
    let
        activeUuid =
            Maybe.withDefault (Uuid.toString editorContext.kmEditor.knowledgeModel.uuid) mbEditorUuid

        ancestors childUuid =
            case Dict.get childUuid editorContext.parentMap of
                Just parent ->
                    childUuid :: ancestors parent

                Nothing ->
                    [ childUuid ]
    in
    { editorContext
        | activeUuid = activeUuid
        , openNodeUuids = List.unique (List.drop 1 (ancestors activeUuid) ++ editorContext.openNodeUuids)
    }


{-| The active entity after something was deleted or moved out from under it:
walk up until a parent that still holds its child, and stop there.
-}
updateActiveEditor : EditorContext -> EditorContext
updateActiveEditor editorContext =
    let
        nil =
            Uuid.toString Uuid.nil

        parentHasChild parentUuid childUuid =
            case KnowledgeModel.getQuestion parentUuid editorContext.kmEditor.knowledgeModel of
                Just question ->
                    List.member childUuid (questionChildren question)

                Nothing ->
                    True

        questionChildren question =
            List.concat
                [ Question.getTagUuids question
                , Question.getExpertUuids question
                , Question.getReferenceUuids question
                , Question.getAnswerUuids question
                , Question.getChoiceUuids question
                , Question.getItemTemplateQuestionUuids question
                ]

        walk currentUuid candidate =
            if currentUuid == nil then
                if candidate == nil then
                    Nothing

                else
                    Just candidate

            else
                let
                    parentUuid =
                        Maybe.withDefault nil (Dict.get currentUuid editorContext.parentMap)
                in
                if isDeleted currentUuid editorContext || not (parentHasChild parentUuid currentUuid) then
                    walk parentUuid parentUuid

                else
                    walk parentUuid candidate
    in
    setActiveEditor (walk editorContext.activeUuid editorContext.activeUuid) editorContext


isActive : String -> EditorContext -> Bool
isActive uuid editorContext =
    editorContext.activeUuid == uuid


setNodeOpen : String -> Bool -> EditorContext -> EditorContext
setNodeOpen entityUuid open editorContext =
    { editorContext
        | openNodeUuids =
            if open then
                entityUuid :: editorContext.openNodeUuids

            else
                List.filter ((/=) entityUuid) editorContext.openNodeUuids
    }


isNodeOpen : String -> EditorContext -> Bool
isNodeOpen entityUuid editorContext =
    List.member entityUuid editorContext.openNodeUuids


treeExpandAll : EditorContext -> EditorContext
treeExpandAll editorContext =
    { editorContext | openNodeUuids = getAllUuids editorContext }


treeCollapseAll : EditorContext -> EditorContext
treeCollapseAll editorContext =
    { editorContext | openNodeUuids = [] }



-- WHAT HAS BEEN DONE TO EACH ENTITY


setEdited : String -> EditorContext -> EditorContext
setEdited uuid editorContext =
    if List.member uuid editorContext.editedUuids then
        editorContext

    else
        { editorContext | editedUuids = uuid :: editorContext.editedUuids }


isEdited : String -> EditorContext -> Bool
isEdited uuid editorContext =
    List.member uuid editorContext.editedUuids
        && not (isAdded uuid editorContext)
        && not (isDeleted uuid editorContext)


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


{-| Added and deleted again before publishing: it was never part of the
knowledge model, so there is no reason to show it anywhere.
-}
isAddedAndDeleted : String -> EditorContext -> Bool
isAddedAndDeleted uuid editorContext =
    List.member uuid editorContext.addedUuids && isDeleted uuid editorContext


{-| An integration that has been added but not yet given a type. Its form asks
which kind it is instead of editing one.
-}
isEmptyIntegrationEditor : String -> EditorContext -> Bool
isEmptyIntegrationEditor uuid editorContext =
    Set.member uuid editorContext.emptyIntegrationEditorUuids


removeEmptyIntegrationEditor : String -> EditorContext -> EditorContext
removeEmptyIntegrationEditor uuid editorContext =
    { editorContext | emptyIntegrationEditorUuids = Set.remove uuid editorContext.emptyIntegrationEditorUuids }



-- APPLYING AN EVENT


{-| `local` is true for an event this client made, false for one replayed from
the history or arriving from somebody else. It decides one thing: whether the
active editor moves to the entity the event touched.
-}
applyEvent : Bool -> Event -> EditorContext -> EditorContext
applyEvent local event originalContext =
    let
        kmEditor =
            originalContext.kmEditor

        knowledgeModel =
            kmEditor.knowledgeModel

        editorContext =
            { originalContext | kmEditor = { kmEditor | events = kmEditor.events ++ [ event ] } }

        edit getEntity apply updateKm =
            applyEdit updateKm (Maybe.map apply (getEntity event.entityUuid knowledgeModel)) event editorContext
    in
    case event.content of
        AddKnowledgeModelEvent _ ->
            editorContext

        EditKnowledgeModelEvent eventData ->
            setKnowledgeModel (EditKnowledgeModelEventData.apply eventData knowledgeModel) editorContext
                |> setEdited (Uuid.toString knowledgeModel.uuid)

        AddChapterEvent eventData ->
            applyAdd local KnowledgeModel.insertChapter (AddChapterEventData.toChapter event.entityUuid eventData) event editorContext

        EditChapterEvent eventData ->
            edit KnowledgeModel.getChapter (EditChapterEventData.apply eventData) KnowledgeModel.updateChapter

        DeleteChapterEvent ->
            applyDelete event editorContext

        AddMetricEvent eventData ->
            applyAdd local KnowledgeModel.insertMetric (AddMetricEventData.toMetric event.entityUuid eventData) event editorContext

        EditMetricEvent eventData ->
            edit KnowledgeModel.getMetric (EditMetricEventData.apply eventData) KnowledgeModel.updateMetric

        DeleteMetricEvent ->
            applyDelete event editorContext

        AddPhaseEvent eventData ->
            applyAdd local KnowledgeModel.insertPhase (AddPhaseEventData.toPhase event.entityUuid eventData) event editorContext

        EditPhaseEvent eventData ->
            edit KnowledgeModel.getPhase (EditPhaseEventData.apply eventData) KnowledgeModel.updatePhase

        DeletePhaseEvent ->
            applyDelete event editorContext

        AddTagEvent eventData ->
            applyAdd local KnowledgeModel.insertTag (AddTagEventData.toTag event.entityUuid eventData) event editorContext

        EditTagEvent eventData ->
            edit KnowledgeModel.getTag (EditTagEventData.apply eventData) KnowledgeModel.updateTag

        DeleteTagEvent ->
            applyDelete event editorContext

        AddIntegrationEvent eventData ->
            let
                integration =
                    AddIntegrationEventData.toIntegration event.entityUuid eventData

                withEmpty =
                    { editorContext
                        | emptyIntegrationEditorUuids =
                            Set.insert (Integration.getUuid integration) editorContext.emptyIntegrationEditorUuids
                    }
            in
            applyAdd local KnowledgeModel.insertIntegration integration event withEmpty

        EditIntegrationEvent eventData ->
            edit KnowledgeModel.getIntegration (EditIntegrationEventData.apply eventData) KnowledgeModel.updateIntegration

        DeleteIntegrationEvent ->
            applyDelete event editorContext

        AddQuestionEvent eventData ->
            applyAdd local KnowledgeModel.insertQuestion (AddQuestionEventData.toQuestion event.entityUuid eventData) event editorContext

        EditQuestionEvent eventData ->
            edit KnowledgeModel.getQuestion (EditQuestionEventData.apply eventData) KnowledgeModel.updateQuestion

        DeleteQuestionEvent ->
            applyDelete event editorContext

        AddAnswerEvent eventData ->
            applyAdd local KnowledgeModel.insertAnswer (AddAnswerEventData.toAnswer event.entityUuid eventData) event editorContext

        EditAnswerEvent eventData ->
            edit KnowledgeModel.getAnswer (EditAnswerEventData.apply eventData) KnowledgeModel.updateAnswer

        DeleteAnswerEvent ->
            applyDelete event editorContext

        AddChoiceEvent eventData ->
            applyAdd local KnowledgeModel.insertChoice (AddChoiceEventData.toChoice event.entityUuid eventData) event editorContext

        EditChoiceEvent eventData ->
            edit KnowledgeModel.getChoice (EditChoiceEventData.apply eventData) KnowledgeModel.updateChoice

        DeleteChoiceEvent ->
            applyDelete event editorContext

        AddReferenceEvent eventData ->
            applyAdd local KnowledgeModel.insertReference (AddReferenceEventData.toReference event.entityUuid eventData) event editorContext

        EditReferenceEvent eventData ->
            edit KnowledgeModel.getReference (EditReferenceEventData.apply eventData) KnowledgeModel.updateReference

        DeleteReferenceEvent ->
            applyDelete event editorContext

        AddExpertEvent eventData ->
            applyAdd local KnowledgeModel.insertExpert (AddExpertEventData.toExpert event.entityUuid eventData) event editorContext

        EditExpertEvent eventData ->
            edit KnowledgeModel.getExpert (EditExpertEventData.apply eventData) KnowledgeModel.updateExpert

        DeleteExpertEvent ->
            applyDelete event editorContext

        AddResourceCollectionEvent eventData ->
            applyAdd local
                KnowledgeModel.insertResourceCollection
                (AddResourceCollectionEventData.toResourceCollection event.entityUuid eventData)
                event
                editorContext

        EditResourceCollectionEvent eventData ->
            edit KnowledgeModel.getResourceCollection (EditResourceCollectionEventData.apply eventData) KnowledgeModel.updateResourceCollection

        DeleteResourceCollectionEvent ->
            applyDelete event editorContext

        AddResourcePageEvent eventData ->
            applyAdd local
                KnowledgeModel.insertResourcePage
                (AddResourcePageEventData.toResourcePage event.entityUuid eventData)
                event
                editorContext

        EditResourcePageEvent eventData ->
            edit KnowledgeModel.getResourcePage (EditResourcePageEventData.apply eventData) KnowledgeModel.updateResourcePage

        DeleteResourcePageEvent ->
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
applyAdd local insert entity event editorContext =
    let
        openIfLocal =
            if local then
                setActiveEditor (Just event.entityUuid)

            else
                identity
    in
    editorContext
        |> setKnowledgeModel (insert entity event.parentUuid editorContext.kmEditor.knowledgeModel)
        |> setParent event.entityUuid event.parentUuid
        |> setAdded event.entityUuid
        |> openIfLocal


applyEdit : (a -> KnowledgeModel -> KnowledgeModel) -> Maybe a -> Event -> EditorContext -> EditorContext
applyEdit update mbEntity event editorContext =
    case mbEntity of
        Just entity ->
            editorContext
                |> setKnowledgeModel (update entity editorContext.kmEditor.knowledgeModel)
                |> setEdited event.entityUuid
                |> updateActiveEditor

        Nothing ->
            editorContext


applyDelete : Event -> EditorContext -> EditorContext
applyDelete event editorContext =
    editorContext
        |> setDeleted event.entityUuid
        |> updateActiveEditor


applyMove :
    Bool
    -> (a -> String -> String -> KnowledgeModel -> KnowledgeModel)
    -> (String -> KnowledgeModel -> Maybe a)
    -> Event
    -> MoveEventData
    -> EditorContext
    -> EditorContext
applyMove local move getEntity event { targetUuid } editorContext =
    case getEntity event.entityUuid editorContext.kmEditor.knowledgeModel of
        Just entity ->
            let
                openIfLocal =
                    if local then
                        setActiveEditor (Just event.entityUuid)

                    else
                        identity
            in
            editorContext
                |> setKnowledgeModel (move entity event.parentUuid targetUuid editorContext.kmEditor.knowledgeModel)
                |> setParent event.entityUuid targetUuid
                |> setEdited event.entityUuid
                |> openIfLocal

        Nothing ->
            editorContext


setParent : String -> String -> EditorContext -> EditorContext
setParent entityUuid parentUuid editorContext =
    { editorContext | parentMap = Dict.insert entityUuid parentUuid editorContext.parentMap }


setKnowledgeModel : KnowledgeModel -> EditorContext -> EditorContext
setKnowledgeModel knowledgeModel editorContext =
    let
        kmEditor =
            editorContext.kmEditor
    in
    { editorContext | kmEditor = { kmEditor | knowledgeModel = knowledgeModel } }
