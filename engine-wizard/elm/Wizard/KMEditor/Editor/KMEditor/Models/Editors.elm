module Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing
    ( AnswerEditorData
    , ChapterEditorData
    , ChoiceEditorData
    , Editor(..)
    , EditorLike
    , EditorState(..)
    , ExpertEditorData
    , IntegrationEditorData
    , KMEditorData
    , MetricEditorData
    , PhaseEditorData
    , QuestionEditorData
    , ReferenceEditorData
    , TagEditorData
    , addAnswerFollowUp
    , addChapterQuestion
    , addKMChapter
    , addKMIntegration
    , addKMMetric
    , addKMPhase
    , addKMTag
    , addQuestionAnswer
    , addQuestionAnswerItemTemplateQuestion
    , addQuestionChoice
    , addQuestionExpert
    , addQuestionReference
    , createAnswerEditor
    , createChapterEditor
    , createChoiceEditor
    , createExpertEditor
    , createIntegrationEditor
    , createKnowledgeModelEditor
    , createMetricEditor
    , createPhaseEditor
    , createQuestionEditor
    , createReferenceEditor
    , createTagEditor
    , deleteAnswerEditor
    , deleteChapterEditor
    , deleteEditor
    , deleteEditors
    , deleteExpertEditor
    , deleteKMEditor
    , deleteQuestionEditor
    , deleteReferenceEditor
    , editorNotDeleted
    , getEditorParentUuid
    , getEditorTitle
    , getEditorUuid
    , getNewState
    , isAnswerEditorDirty
    , isChapterEditorDirty
    , isChoiceEditorDirty
    , isEditorDeleted
    , isEditorDirty
    , isExpertEditorDirty
    , isIntegrationEditorDirty
    , isKMEditorDirty
    , isMetricEditorDirty
    , isPhaseEditorDirty
    , isQuestionEditorDirty
    , isReferenceEditorDirty
    , isTagEditorDirty
    , setEditorClosed
    , setEditorOpen
    , toggleEditorOpen
    , updateAnswerEditorData
    , updateChapterEditorData
    , updateChoiceEditorData
    , updateEditorsWithQuestion
    , updateExpertEditorData
    , updateIntegrationEditorData
    , updateKMEditorData
    , updateMetricEditorData
    , updatePhaseEditorData
    , updateQuestionEditorData
    , updateReferenceEditorData
    , updateTagEditorData
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (nilUuid)
import String.Format exposing (format)
import Uuid
import ValueList exposing (ValueList)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (AnswerForm, ChapterForm, ChoiceForm, ExpertForm, IntegrationForm, KnowledgeModelForm, MetricForm, PhaseForm, QuestionForm, ReferenceForm, TagForm, formChanged, initAnswerForm, initChapterForm, initChoiceForm, initExpertForm, initIntegrationForm, initKnowledgeModelFrom, initMetricForm, initPhaseForm, initQuestionForm, initReferenceForm, initTagForm, updateAnswerWithForm, updateChapterWithForm, updateChoiceWithForm, updateExpertWithForm, updateIntegrationWithForm, updateKnowledgeModelWithForm, updateMetricWithForm, updatePhaseWithForm, updateQuestionWithForm, updateReferenceWithForm, updateTagWithForm)


type EditorState
    = Initial
    | Edited
    | Deleted
    | Added
    | AddedEdited


type Editor
    = KMEditor KMEditorData
    | MetricEditor MetricEditorData
    | PhaseEditor PhaseEditorData
    | TagEditor TagEditorData
    | IntegrationEditor IntegrationEditorData
    | ChapterEditor ChapterEditorData
    | QuestionEditor QuestionEditorData
    | AnswerEditor AnswerEditorData
    | ChoiceEditor ChoiceEditorData
    | ReferenceEditor ReferenceEditorData
    | ExpertEditor ExpertEditorData


type alias EditorLike editorData e form =
    { editorData
        | form : Form e form
        , editorState : EditorState
        , uuid : String
        , treeOpen : Bool
        , parentUuid : String
    }


type alias KMEditorData =
    { uuid : String
    , knowledgeModel : KnowledgeModel
    , form : Form FormError KnowledgeModelForm
    , chapters : Children
    , metrics : Children
    , phases : Children
    , tags : Children
    , integrations : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias MetricEditorData =
    { uuid : String
    , metric : Metric
    , form : Form FormError MetricForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias PhaseEditorData =
    { uuid : String
    , phase : Phase
    , form : Form FormError PhaseForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias TagEditorData =
    { uuid : String
    , tag : Tag
    , form : Form FormError TagForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias IntegrationEditorData =
    { uuid : String
    , integration : Integration
    , form : Form FormError IntegrationForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    , props : ValueList
    , deleteConfirmOpen : Bool
    }


type alias ChapterEditorData =
    { uuid : String
    , chapter : Chapter
    , form : Form FormError ChapterForm
    , questions : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias QuestionEditorData =
    { uuid : String
    , question : Question
    , form : Form FormError QuestionForm
    , tagUuids : List String
    , answers : Children
    , itemTemplateQuestions : Children
    , choices : Children
    , references : Children
    , experts : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias AnswerEditorData =
    { uuid : String
    , answer : Answer
    , form : Form FormError AnswerForm
    , followUps : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias ChoiceEditorData =
    { uuid : String
    , choice : Choice
    , form : Form FormError ChoiceForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias ReferenceEditorData =
    { uuid : String
    , reference : Reference
    , form : Form FormError ReferenceForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias ExpertEditorData =
    { uuid : String
    , expert : Expert
    , form : Form FormError ExpertForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }



{- constructors -}


createKnowledgeModelEditor : Maybe String -> (String -> EditorState) -> KnowledgeModel -> Dict String Editor -> Dict String Editor
createKnowledgeModelEditor mbActiveEditorUuid getEditorState km editors =
    let
        chapters =
            KnowledgeModel.getChapters km

        metrics =
            KnowledgeModel.getMetrics km

        phases =
            KnowledgeModel.getPhases km

        tags =
            KnowledgeModel.getTags km

        integrations =
            KnowledgeModel.getIntegrations km

        editor =
            KMEditor
                { uuid = Uuid.toString km.uuid
                , knowledgeModel = km
                , form = initKnowledgeModelFrom km
                , chapters = Children.init <| List.map .uuid chapters
                , metrics = Children.init <| List.map .uuid metrics
                , phases = Children.init <| List.map .uuid phases
                , tags = Children.init <| List.map .uuid tags
                , integrations = Children.init <| List.map .uuid integrations
                , treeOpen = True
                , editorState = getEditorState (Uuid.toString km.uuid)
                , parentUuid = nilUuid
                }

        withChapters =
            List.foldl (createChapterEditor integrations (Uuid.toString km.uuid) getEditorState km) editors chapters

        withMetrics =
            List.foldl (createMetricEditor (Uuid.toString km.uuid) getEditorState km) withChapters metrics

        withPhases =
            List.foldl (createPhaseEditor (Uuid.toString km.uuid) getEditorState km) withMetrics phases

        withTags =
            List.foldl (createTagEditor (Uuid.toString km.uuid) getEditorState km) withPhases tags

        withIntegrations =
            List.foldl (createIntegrationEditor (Uuid.toString km.uuid) getEditorState km) withTags integrations
    in
    openActiveEditorPath mbActiveEditorUuid <| Dict.insert (Uuid.toString km.uuid) editor withIntegrations


openActiveEditorPath : Maybe String -> Dict String Editor -> Dict String Editor
openActiveEditorPath activeEditorUuid editors =
    case Maybe.andThen (\uuid -> Dict.get uuid editors) activeEditorUuid of
        Just editor ->
            openActiveEditorPath
                (Just <| getEditorParentUuid editor)
                (Dict.insert (getEditorUuid editor) (setEditorOpen editor) editors)

        Nothing ->
            editors


createChapterEditor : List Integration -> String -> (String -> EditorState) -> KnowledgeModel -> Chapter -> Dict String Editor -> Dict String Editor
createChapterEditor integrations parentUuid getEditorState km chapter editors =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km

        editor =
            ChapterEditor
                { uuid = chapter.uuid
                , chapter = chapter
                , form = initChapterForm chapter
                , questions = Children.init <| List.map Question.getUuid questions
                , treeOpen = False
                , editorState = getEditorState chapter.uuid
                , parentUuid = parentUuid
                }

        withQuestions =
            List.foldl (createQuestionEditor integrations chapter.uuid getEditorState km) editors questions
    in
    Dict.insert chapter.uuid editor withQuestions


createMetricEditor : String -> (String -> EditorState) -> KnowledgeModel -> Metric -> Dict String Editor -> Dict String Editor
createMetricEditor parentUuid getEditorState _ metric editors =
    let
        editor =
            MetricEditor
                { uuid = metric.uuid
                , metric = metric
                , form = initMetricForm metric
                , treeOpen = False
                , editorState = getEditorState metric.uuid
                , parentUuid = parentUuid
                }
    in
    Dict.insert metric.uuid editor editors


createPhaseEditor : String -> (String -> EditorState) -> KnowledgeModel -> Phase -> Dict String Editor -> Dict String Editor
createPhaseEditor parentUuid getEditorState _ phase editors =
    let
        editor =
            PhaseEditor
                { uuid = phase.uuid
                , phase = phase
                , form = initPhaseForm phase
                , treeOpen = False
                , editorState = getEditorState phase.uuid
                , parentUuid = parentUuid
                }
    in
    Dict.insert phase.uuid editor editors


createTagEditor : String -> (String -> EditorState) -> KnowledgeModel -> Tag -> Dict String Editor -> Dict String Editor
createTagEditor parentUuid getEditorState _ tag editors =
    let
        editor =
            TagEditor
                { uuid = tag.uuid
                , tag = tag
                , form = initTagForm tag
                , treeOpen = False
                , editorState = getEditorState tag.uuid
                , parentUuid = parentUuid
                }
    in
    Dict.insert tag.uuid editor editors


createIntegrationEditor : String -> (String -> EditorState) -> KnowledgeModel -> Integration -> Dict String Editor -> Dict String Editor
createIntegrationEditor parentUuid getEditorState _ integration editors =
    let
        editor =
            IntegrationEditor
                { uuid = integration.uuid
                , integration = integration
                , form = initIntegrationForm [] "" integration
                , treeOpen = False
                , editorState = getEditorState integration.uuid
                , parentUuid = parentUuid
                , props = ValueList.init integration.props
                , deleteConfirmOpen = False
                }
    in
    Dict.insert integration.uuid editor editors


createQuestionEditor : List Integration -> String -> (String -> EditorState) -> KnowledgeModel -> Question -> Dict String Editor -> Dict String Editor
createQuestionEditor integrations parentUuid getEditorState km question editors =
    let
        questionUuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers questionUuid km

        choices =
            KnowledgeModel.getQuestionChoices questionUuid km

        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        editor =
            QuestionEditor
                { uuid = questionUuid
                , question = question
                , form = initQuestionForm integrations question
                , tagUuids = Question.getTagUuids question
                , answers = Children.init <| List.map .uuid answers
                , choices = Children.init <| List.map .uuid choices
                , itemTemplateQuestions = Children.init <| List.map Question.getUuid itemTemplateQuestions
                , references = Children.init <| List.map Reference.getUuid references
                , experts = Children.init <| List.map .uuid experts
                , treeOpen = False
                , editorState = getEditorState questionUuid
                , parentUuid = parentUuid
                }

        withAnswers =
            List.foldl (createAnswerEditor integrations questionUuid getEditorState km) editors answers

        withChoices =
            List.foldl (createChoiceEditor questionUuid getEditorState km) withAnswers choices

        withAnswerItemTemplateQuestions =
            List.foldl (createQuestionEditor integrations questionUuid getEditorState km) withChoices itemTemplateQuestions

        withReferences =
            List.foldl (createReferenceEditor questionUuid getEditorState km) withAnswerItemTemplateQuestions references

        withExperts =
            List.foldl (createExpertEditor questionUuid getEditorState km) withReferences experts
    in
    Dict.insert questionUuid editor withExperts


createAnswerEditor : List Integration -> String -> (String -> EditorState) -> KnowledgeModel -> Answer -> Dict String Editor -> Dict String Editor
createAnswerEditor integrations parentUuid getEditorState km answer editors =
    let
        followUps =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        metrics =
            KnowledgeModel.getMetrics km

        editor =
            AnswerEditor
                { uuid = answer.uuid
                , answer = answer
                , form = initAnswerForm metrics answer
                , followUps = Children.init <| List.map Question.getUuid followUps
                , treeOpen = False
                , editorState = getEditorState answer.uuid
                , parentUuid = parentUuid
                }

        withFollowUps =
            List.foldl (createQuestionEditor integrations answer.uuid getEditorState km) editors followUps
    in
    Dict.insert answer.uuid editor withFollowUps


createChoiceEditor : String -> (String -> EditorState) -> KnowledgeModel -> Choice -> Dict String Editor -> Dict String Editor
createChoiceEditor parentUuid getEditorState _ choice editors =
    let
        editor =
            ChoiceEditor
                { uuid = choice.uuid
                , choice = choice
                , form = initChoiceForm choice
                , treeOpen = False
                , editorState = getEditorState choice.uuid
                , parentUuid = parentUuid
                }
    in
    Dict.insert choice.uuid editor editors


createReferenceEditor : String -> (String -> EditorState) -> KnowledgeModel -> Reference -> Dict String Editor -> Dict String Editor
createReferenceEditor parentUuid getEditorState _ reference editors =
    let
        referenceUuid =
            Reference.getUuid reference

        editor =
            ReferenceEditor
                { uuid = referenceUuid
                , reference = reference
                , form = initReferenceForm reference
                , treeOpen = False
                , editorState = getEditorState referenceUuid
                , parentUuid = parentUuid
                }
    in
    Dict.insert referenceUuid editor editors


createExpertEditor : String -> (String -> EditorState) -> KnowledgeModel -> Expert -> Dict String Editor -> Dict String Editor
createExpertEditor parentUuid getEditorState _ expert editors =
    let
        editor =
            ExpertEditor
                { uuid = expert.uuid
                , expert = expert
                , form = initExpertForm expert
                , treeOpen = False
                , editorState = getEditorState expert.uuid
                , parentUuid = parentUuid
                }
    in
    Dict.insert expert.uuid editor editors



{- deleting editors -}


deleteEditors : Children -> Dict String Editor -> Dict String Editor
deleteEditors children editors =
    List.foldl deleteEditor editors (children.list ++ children.deleted)


deleteEditor : String -> Dict String Editor -> Dict String Editor
deleteEditor uuid editors =
    case Dict.get uuid editors of
        Just (KMEditor editorData) ->
            deleteKMEditor editorData editors

        Just (ChapterEditor editorData) ->
            deleteChapterEditor editorData editors

        Just (TagEditor editorData) ->
            deleteTagEditor editorData editors

        Just (IntegrationEditor editorData) ->
            deleteIntegrationEditor editorData editors

        Just (QuestionEditor editorData) ->
            deleteQuestionEditor editorData editors

        Just (AnswerEditor editorData) ->
            deleteAnswerEditor editorData editors

        Just (ReferenceEditor editorData) ->
            deleteReferenceEditor editorData editors

        Just (ExpertEditor editorData) ->
            deleteExpertEditor editorData editors

        _ ->
            editors


deleteKMEditor : KMEditorData -> Dict String Editor -> Dict String Editor
deleteKMEditor editorData editors =
    editors
        |> deleteEditors editorData.chapters
        |> Dict.remove editorData.uuid


deleteChapterEditor : ChapterEditorData -> Dict String Editor -> Dict String Editor
deleteChapterEditor editorData editors =
    editors
        |> deleteEditors editorData.questions
        |> Dict.remove editorData.uuid


deleteTagEditor : TagEditorData -> Dict String Editor -> Dict String Editor
deleteTagEditor editorData editors =
    Dict.remove editorData.uuid editors


deleteIntegrationEditor : IntegrationEditorData -> Dict String Editor -> Dict String Editor
deleteIntegrationEditor editorData editors =
    Dict.remove editorData.uuid editors


deleteQuestionEditor : QuestionEditorData -> Dict String Editor -> Dict String Editor
deleteQuestionEditor editorData editors =
    editors
        |> deleteEditors editorData.answers
        |> deleteEditors editorData.itemTemplateQuestions
        |> deleteEditors editorData.references
        |> deleteEditors editorData.experts
        |> Dict.remove editorData.uuid


deleteAnswerEditor : AnswerEditorData -> Dict String Editor -> Dict String Editor
deleteAnswerEditor editorData editors =
    editors
        |> deleteEditors editorData.followUps
        |> Dict.remove editorData.uuid


deleteReferenceEditor : ReferenceEditorData -> Dict String Editor -> Dict String Editor
deleteReferenceEditor editorData editors =
    Dict.remove editorData.uuid editors


deleteExpertEditor : ExpertEditorData -> Dict String Editor -> Dict String Editor
deleteExpertEditor editorData editors =
    Dict.remove editorData.uuid editors



{- utils -}


getEditorTitle : String -> Editor -> String
getEditorTitle kmName editor =
    case editor of
        KMEditor _ ->
            kmName

        ChapterEditor data ->
            data.chapter.title

        MetricEditor data ->
            data.metric.title

        PhaseEditor data ->
            data.phase.title

        TagEditor data ->
            data.tag.name

        IntegrationEditor data ->
            data.integration.name

        QuestionEditor data ->
            Question.getTitle data.question

        AnswerEditor data ->
            data.answer.label

        ChoiceEditor data ->
            data.choice.label

        ReferenceEditor data ->
            Reference.getVisibleName data.reference

        ExpertEditor data ->
            data.expert.name


getEditorUuid : Editor -> String
getEditorUuid editor =
    case editor of
        KMEditor data ->
            Uuid.toString data.knowledgeModel.uuid

        ChapterEditor data ->
            data.chapter.uuid

        MetricEditor data ->
            data.metric.uuid

        PhaseEditor data ->
            data.phase.uuid

        TagEditor data ->
            data.tag.uuid

        IntegrationEditor data ->
            data.integration.uuid

        QuestionEditor data ->
            Question.getUuid data.question

        AnswerEditor data ->
            data.answer.uuid

        ChoiceEditor data ->
            data.choice.uuid

        ReferenceEditor data ->
            Reference.getUuid data.reference

        ExpertEditor data ->
            data.expert.uuid


getEditorParentUuid : Editor -> String
getEditorParentUuid editor =
    case editor of
        KMEditor data ->
            data.parentUuid

        ChapterEditor data ->
            data.parentUuid

        MetricEditor data ->
            data.parentUuid

        PhaseEditor data ->
            data.parentUuid

        TagEditor data ->
            data.parentUuid

        IntegrationEditor data ->
            data.parentUuid

        QuestionEditor data ->
            data.parentUuid

        AnswerEditor data ->
            data.parentUuid

        ChoiceEditor data ->
            data.parentUuid

        ReferenceEditor data ->
            data.parentUuid

        ExpertEditor data ->
            data.parentUuid


getNewState : EditorState -> EditorState -> EditorState
getNewState originalState newState =
    if newState == Deleted then
        Deleted

    else if (originalState == Added || originalState == AddedEdited) && newState == Edited then
        AddedEdited

    else
        newState


toggleEditorOpen : Editor -> Editor
toggleEditorOpen =
    updateEditorOpen not


setEditorOpen : Editor -> Editor
setEditorOpen =
    updateEditorOpen (always True)


setEditorClosed : Editor -> Editor
setEditorClosed =
    updateEditorOpen (always False)


updateEditorOpen : (Bool -> Bool) -> Editor -> Editor
updateEditorOpen updateFn editor =
    case editor of
        KMEditor data ->
            KMEditor { data | treeOpen = updateFn data.treeOpen }

        ChapterEditor data ->
            ChapterEditor { data | treeOpen = updateFn data.treeOpen }

        MetricEditor data ->
            MetricEditor { data | treeOpen = updateFn data.treeOpen }

        PhaseEditor data ->
            PhaseEditor { data | treeOpen = updateFn data.treeOpen }

        TagEditor data ->
            TagEditor { data | treeOpen = updateFn data.treeOpen }

        IntegrationEditor data ->
            IntegrationEditor { data | treeOpen = updateFn data.treeOpen }

        QuestionEditor data ->
            QuestionEditor { data | treeOpen = updateFn data.treeOpen }

        AnswerEditor data ->
            AnswerEditor { data | treeOpen = updateFn data.treeOpen }

        ChoiceEditor data ->
            ChoiceEditor { data | treeOpen = updateFn data.treeOpen }

        ReferenceEditor data ->
            ReferenceEditor { data | treeOpen = updateFn data.treeOpen }

        ExpertEditor data ->
            ExpertEditor { data | treeOpen = updateFn data.treeOpen }


editorNotDeleted : Dict String Editor -> String -> Bool
editorNotDeleted editors uuid =
    Dict.get uuid editors
        |> Maybe.map (not << isEditorDeleted)
        |> Maybe.withDefault False


isEditorDeleted : Editor -> Bool
isEditorDeleted editor =
    case editor of
        KMEditor data ->
            data.editorState == Deleted

        ChapterEditor data ->
            data.editorState == Deleted

        MetricEditor data ->
            data.editorState == Deleted

        PhaseEditor data ->
            data.editorState == Deleted

        TagEditor data ->
            data.editorState == Deleted

        IntegrationEditor data ->
            data.editorState == Deleted

        QuestionEditor data ->
            data.editorState == Deleted

        AnswerEditor data ->
            data.editorState == Deleted

        ChoiceEditor data ->
            data.editorState == Deleted

        ReferenceEditor data ->
            data.editorState == Deleted

        ExpertEditor data ->
            data.editorState == Deleted


isEditorDirty : Editor -> Bool
isEditorDirty editor =
    case editor of
        KMEditor data ->
            isKMEditorDirty data

        ChapterEditor data ->
            isChapterEditorDirty data

        MetricEditor data ->
            isMetricEditorDirty data

        PhaseEditor data ->
            isPhaseEditorDirty data

        TagEditor data ->
            isTagEditorDirty data

        IntegrationEditor data ->
            isIntegrationEditorDirty data

        QuestionEditor data ->
            isQuestionEditorDirty data

        AnswerEditor data ->
            isAnswerEditorDirty data

        ChoiceEditor data ->
            isChoiceEditorDirty data

        ReferenceEditor data ->
            isReferenceEditorDirty data

        ExpertEditor data ->
            isExpertEditorDirty data


isKMEditorDirty : KMEditorData -> Bool
isKMEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || editorData.chapters.dirty
        || editorData.metrics.dirty
        || editorData.phases.dirty
        || editorData.tags.dirty
        || editorData.integrations.dirty


isChapterEditorDirty : ChapterEditorData -> Bool
isChapterEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || editorData.questions.dirty


isMetricEditorDirty : MetricEditorData -> Bool
isMetricEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


isPhaseEditorDirty : PhaseEditorData -> Bool
isPhaseEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


isTagEditorDirty : TagEditorData -> Bool
isTagEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


isIntegrationEditorDirty : IntegrationEditorData -> Bool
isIntegrationEditorDirty editorData =
    let
        buildHeader i =
            let
                index =
                    String.fromInt i

                header =
                    Form.getFieldAsString (format "requestHeaders.%s.header" [ index ]) editorData.form

                value =
                    Form.getFieldAsString (format "requestHeaders.%s.value" [ index ]) editorData.form
            in
            ( Maybe.withDefault "" header.value
            , Maybe.withDefault "" value.value
            )

        currentHeaders =
            Dict.fromList <|
                List.map buildHeader <|
                    Form.getListIndexes "requestHeaders" editorData.form
    in
    (editorData.editorState == Added)
        || formChanged editorData.form
        || (currentHeaders /= editorData.integration.requestHeaders)
        || editorData.props.dirty


isQuestionEditorDirty : QuestionEditorData -> Bool
isQuestionEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || (Question.getTagUuids editorData.question /= editorData.tagUuids)
        || editorData.answers.dirty
        || editorData.choices.dirty
        || editorData.itemTemplateQuestions.dirty
        || editorData.references.dirty
        || editorData.experts.dirty


isAnswerEditorDirty : AnswerEditorData -> Bool
isAnswerEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || editorData.followUps.dirty


isChoiceEditorDirty : ChoiceEditorData -> Bool
isChoiceEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


isReferenceEditorDirty : ReferenceEditorData -> Bool
isReferenceEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


isExpertEditorDirty : ExpertEditorData -> Bool
isExpertEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


updateKMEditorData : EditorState -> KnowledgeModelForm -> KMEditorData -> KMEditorData
updateKMEditorData newState form editorData =
    let
        newKM =
            updateKnowledgeModelWithForm editorData.knowledgeModel form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , knowledgeModel = newKM
        , chapters = Children.cleanDirty editorData.chapters
        , form = initKnowledgeModelFrom newKM
    }


updateChapterEditorData : EditorState -> ChapterForm -> ChapterEditorData -> ChapterEditorData
updateChapterEditorData newState form editorData =
    let
        newChapter =
            updateChapterWithForm editorData.chapter form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , chapter = newChapter
        , questions = Children.cleanDirty editorData.questions
        , form = initChapterForm newChapter
    }


updateMetricEditorData : EditorState -> MetricForm -> MetricEditorData -> MetricEditorData
updateMetricEditorData newState form editorData =
    let
        newMetric =
            updateMetricWithForm editorData.metric form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , metric = newMetric
        , form = initMetricForm newMetric
    }


updatePhaseEditorData : EditorState -> PhaseForm -> PhaseEditorData -> PhaseEditorData
updatePhaseEditorData newState form editorData =
    let
        newPhase =
            updatePhaseWithForm editorData.phase form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , phase = newPhase
        , form = initPhaseForm newPhase
    }


updateTagEditorData : EditorState -> TagForm -> TagEditorData -> TagEditorData
updateTagEditorData newState form editorData =
    let
        newTag =
            updateTagWithForm editorData.tag form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , tag = newTag
        , form = initTagForm newTag
    }


updateIntegrationEditorData : EditorState -> IntegrationForm -> IntegrationEditorData -> IntegrationEditorData
updateIntegrationEditorData newState form editorData =
    let
        newIntegration =
            updateIntegrationWithForm editorData.integration form
                |> updateIntegrationWithProps editorData.props
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , integration = newIntegration
        , form = initIntegrationForm [] "" newIntegration
    }


updateIntegrationWithProps : ValueList -> Integration -> Integration
updateIntegrationWithProps props integration =
    { integration | props = props.list }


updateQuestionEditorData : List Integration -> EditorState -> QuestionForm -> QuestionEditorData -> QuestionEditorData
updateQuestionEditorData integrations newState form editorData =
    let
        newQuestion =
            updateQuestionWithForm editorData.question form

        newAnswers =
            if Question.isOptions newQuestion then
                Children.cleanDirty editorData.answers

            else
                Children.init []

        newAnswerItemTemplateQuestions =
            if Question.isList newQuestion then
                Children.cleanDirty editorData.itemTemplateQuestions

            else
                Children.init []
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , question = newQuestion
        , answers = newAnswers
        , itemTemplateQuestions = newAnswerItemTemplateQuestions
        , references = Children.cleanDirty editorData.references
        , experts = Children.cleanDirty editorData.experts
        , form = initQuestionForm integrations newQuestion
    }


updateEditorsWithQuestion : QuestionEditorData -> QuestionEditorData -> Dict String Editor -> Dict String Editor
updateEditorsWithQuestion newEditorData oldEditorData editors =
    case newEditorData.question of
        OptionsQuestion _ _ ->
            deleteEditors oldEditorData.itemTemplateQuestions editors

        ListQuestion _ _ ->
            deleteEditors oldEditorData.answers editors

        _ ->
            editors
                |> deleteEditors oldEditorData.itemTemplateQuestions
                |> deleteEditors oldEditorData.answers


updateAnswerEditorData : List Metric -> EditorState -> AnswerForm -> AnswerEditorData -> AnswerEditorData
updateAnswerEditorData metrics newState form editorData =
    let
        newAnswer =
            updateAnswerWithForm editorData.answer form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , answer = newAnswer
        , followUps = Children.cleanDirty editorData.followUps
        , form = initAnswerForm metrics newAnswer
    }


updateChoiceEditorData : EditorState -> ChoiceForm -> ChoiceEditorData -> ChoiceEditorData
updateChoiceEditorData newState form editorData =
    let
        newChoice =
            updateChoiceWithForm editorData.choice form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , choice = newChoice
        , form = initChoiceForm newChoice
    }


updateReferenceEditorData : EditorState -> ReferenceForm -> ReferenceEditorData -> ReferenceEditorData
updateReferenceEditorData newState form editorData =
    let
        newReference =
            updateReferenceWithForm editorData.reference form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , reference = newReference
        , form = initReferenceForm newReference
    }


updateExpertEditorData : EditorState -> ExpertForm -> ExpertEditorData -> ExpertEditorData
updateExpertEditorData newState form editorData =
    let
        newExpert =
            updateExpertWithForm editorData.expert form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , expert = newExpert
        , form = initExpertForm newExpert
    }


addKMChapter : Chapter -> KMEditorData -> Editor
addKMChapter chapter editorData =
    KMEditor
        { editorData
            | chapters = Children.addChild chapter.uuid editorData.chapters
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addKMMetric : Metric -> KMEditorData -> Editor
addKMMetric metric editorData =
    KMEditor
        { editorData
            | metrics = Children.addChild metric.uuid editorData.metrics
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addKMPhase : Phase -> KMEditorData -> Editor
addKMPhase phase editorData =
    KMEditor
        { editorData
            | phases = Children.addChild phase.uuid editorData.phases
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addKMTag : Tag -> KMEditorData -> Editor
addKMTag tag editorData =
    KMEditor
        { editorData
            | tags = Children.addChild tag.uuid editorData.tags
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addKMIntegration : Integration -> KMEditorData -> Editor
addKMIntegration integration editorData =
    KMEditor
        { editorData
            | integrations = Children.addChild integration.uuid editorData.integrations
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addChapterQuestion : Question -> ChapterEditorData -> Editor
addChapterQuestion question editorData =
    ChapterEditor
        { editorData
            | questions = Children.addChild (Question.getUuid question) editorData.questions
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionAnswer : Answer -> QuestionEditorData -> Editor
addQuestionAnswer answer editorData =
    QuestionEditor
        { editorData
            | answers = Children.addChild answer.uuid editorData.answers
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionChoice : Choice -> QuestionEditorData -> Editor
addQuestionChoice choice editorData =
    QuestionEditor
        { editorData
            | choices = Children.addChild choice.uuid editorData.choices
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionAnswerItemTemplateQuestion : Question -> QuestionEditorData -> Editor
addQuestionAnswerItemTemplateQuestion question editorData =
    QuestionEditor
        { editorData
            | itemTemplateQuestions = Children.addChild (Question.getUuid question) editorData.itemTemplateQuestions
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionReference : Reference -> QuestionEditorData -> Editor
addQuestionReference reference editorData =
    QuestionEditor
        { editorData
            | references = Children.addChild (Reference.getUuid reference) editorData.references
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionExpert : Expert -> QuestionEditorData -> Editor
addQuestionExpert expert editorData =
    QuestionEditor
        { editorData
            | experts = Children.addChild expert.uuid editorData.experts
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addAnswerFollowUp : Question -> AnswerEditorData -> Editor
addAnswerFollowUp followUp editorData =
    AnswerEditor
        { editorData
            | followUps = Children.addChild (Question.getUuid followUp) editorData.followUps
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }
