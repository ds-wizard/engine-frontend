module Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing
    ( AnswerEditorData
    , ChapterEditorData
    , Editor(..)
    , EditorLike
    , EditorState(..)
    , ExpertEditorData
    , IntegrationEditorData
    , KMEditorData
    , QuestionEditorData
    , ReferenceEditorData
    , TagEditorData
    , addAnswerFollowUp
    , addChapterQuestion
    , addKMChapter
    , addKMIntegration
    , addKMTag
    , addQuestionAnswer
    , addQuestionAnswerItemTemplateQuestion
    , addQuestionExpert
    , addQuestionReference
    , createAnswerEditor
    , createChapterEditor
    , createExpertEditor
    , createIntegrationEditor
    , createKnowledgeModelEditor
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
    , isEditorDeleted
    , isEditorDirty
    , isExpertEditorDirty
    , isIntegrationEditorDirty
    , isKMEditorDirty
    , isQuestionEditorDirty
    , isReferenceEditorDirty
    , isTagEditorDirty
    , toggleEditorOpen
    , updateAnswerEditorData
    , updateChapterEditorData
    , updateEditorsWithQuestion
    , updateExpertEditorData
    , updateIntegrationEditorData
    , updateKMEditorData
    , updateQuestionEditorData
    , updateReferenceEditorData
    , updateTagEditorData
    )

import Dict exposing (Dict)
import Form exposing (Form)
import String.Format exposing (format)
import ValueList exposing (ValueList)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import Wizard.KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.KMEditor.Common.KnowledgeModel.Expert exposing (Expert)
import Wizard.KMEditor.Common.KnowledgeModel.Integration exposing (Integration)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.KMEditor.Common.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (..)
import Wizard.Utils exposing (nilUuid)


type EditorState
    = Initial
    | Edited
    | Deleted
    | Added
    | AddedEdited


type Editor
    = KMEditor KMEditorData
    | TagEditor TagEditorData
    | IntegrationEditor IntegrationEditorData
    | ChapterEditor ChapterEditorData
    | QuestionEditor QuestionEditorData
    | AnswerEditor AnswerEditorData
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
    , form : Form CustomFormError KnowledgeModelForm
    , chapters : Children
    , tags : Children
    , integrations : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias TagEditorData =
    { uuid : String
    , tag : Tag
    , form : Form CustomFormError TagForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias IntegrationEditorData =
    { uuid : String
    , integration : Integration
    , form : Form CustomFormError IntegrationForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    , props : ValueList
    , deleteConfirmOpen : Bool
    }


type alias ChapterEditorData =
    { uuid : String
    , chapter : Chapter
    , form : Form CustomFormError ChapterForm
    , questions : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias QuestionEditorData =
    { uuid : String
    , question : Question
    , form : Form CustomFormError QuestionForm
    , tagUuids : List String
    , answers : Children
    , itemTemplateQuestions : Children
    , references : Children
    , experts : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias AnswerEditorData =
    { uuid : String
    , answer : Answer
    , form : Form CustomFormError AnswerForm
    , followUps : Children
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias ReferenceEditorData =
    { uuid : String
    , reference : Reference
    , form : Form CustomFormError ReferenceForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }


type alias ExpertEditorData =
    { uuid : String
    , expert : Expert
    , form : Form CustomFormError ExpertForm
    , treeOpen : Bool
    , editorState : EditorState
    , parentUuid : String
    }



{- constructors -}


createKnowledgeModelEditor : EditorContext -> (String -> EditorState) -> KnowledgeModel -> Dict String Editor -> Dict String Editor
createKnowledgeModelEditor editorContext getEditorState km editors =
    let
        chapters =
            KnowledgeModel.getChapters km

        tags =
            KnowledgeModel.getTags km

        integrations =
            KnowledgeModel.getIntegrations km

        editor =
            KMEditor
                { uuid = km.uuid
                , knowledgeModel = km
                , form = initKnowledgeModelFrom km
                , chapters = Children.init <| List.map .uuid chapters
                , tags = Children.init <| List.map .uuid tags
                , integrations = Children.init <| List.map .uuid integrations
                , treeOpen = True
                , editorState = getEditorState km.uuid
                , parentUuid = nilUuid
                }

        withChapters =
            List.foldl (createChapterEditor editorContext km.uuid getEditorState km) editors chapters

        withTags =
            List.foldl (createTagEditor editorContext km.uuid getEditorState km) withChapters tags

        withIntegrations =
            List.foldl (createIntegrationEditor editorContext km.uuid getEditorState km) withTags integrations
    in
    Dict.insert km.uuid editor withIntegrations


createChapterEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Chapter -> Dict String Editor -> Dict String Editor
createChapterEditor editorContext parentUuid getEditorState km chapter editors =
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
            List.foldl (createQuestionEditor editorContext chapter.uuid getEditorState km) editors questions
    in
    Dict.insert chapter.uuid editor withQuestions


createTagEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Tag -> Dict String Editor -> Dict String Editor
createTagEditor editorContext parentUuid getEditorState km tag editors =
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


createIntegrationEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Integration -> Dict String Editor -> Dict String Editor
createIntegrationEditor editorContext parentUuid getEditorState km integration editors =
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


createQuestionEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Question -> Dict String Editor -> Dict String Editor
createQuestionEditor editorContext parentUuid getEditorState km question editors =
    let
        questionUuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers questionUuid km

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
                , form = initQuestionForm question
                , tagUuids = Question.getTagUuids question
                , answers = Children.init <| List.map .uuid answers
                , itemTemplateQuestions = Children.init <| List.map Question.getUuid itemTemplateQuestions
                , references = Children.init <| List.map Reference.getUuid references
                , experts = Children.init <| List.map .uuid experts
                , treeOpen = False
                , editorState = getEditorState questionUuid
                , parentUuid = parentUuid
                }

        withAnswers =
            List.foldl (createAnswerEditor editorContext questionUuid getEditorState km) editors answers

        withAnswerItemTemplateQuestions =
            List.foldl (createQuestionEditor editorContext questionUuid getEditorState km) withAnswers itemTemplateQuestions

        withReferences =
            List.foldl (createReferenceEditor editorContext questionUuid getEditorState km) withAnswerItemTemplateQuestions references

        withExperts =
            List.foldl (createExpertEditor editorContext questionUuid getEditorState km) withReferences experts
    in
    Dict.insert questionUuid editor withExperts


createAnswerEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Answer -> Dict String Editor -> Dict String Editor
createAnswerEditor editorContext parentUuid getEditorState km answer editors =
    let
        followUps =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        editor =
            AnswerEditor
                { uuid = answer.uuid
                , answer = answer
                , form = initAnswerForm editorContext answer
                , followUps = Children.init <| List.map Question.getUuid followUps
                , treeOpen = False
                , editorState = getEditorState answer.uuid
                , parentUuid = parentUuid
                }

        withFollowUps =
            List.foldl (createQuestionEditor editorContext answer.uuid getEditorState km) editors followUps
    in
    Dict.insert answer.uuid editor withFollowUps


createReferenceEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Reference -> Dict String Editor -> Dict String Editor
createReferenceEditor editorContext parentUuid getEditorState km reference editors =
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


createExpertEditor : EditorContext -> String -> (String -> EditorState) -> KnowledgeModel -> Expert -> Dict String Editor -> Dict String Editor
createExpertEditor editorContext parentUuid getEditorState km expert editors =
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


getEditorTitle : Editor -> String
getEditorTitle editor =
    case editor of
        KMEditor data ->
            data.knowledgeModel.name

        ChapterEditor data ->
            data.chapter.title

        TagEditor data ->
            data.tag.name

        IntegrationEditor data ->
            data.integration.name

        QuestionEditor data ->
            Question.getTitle data.question

        AnswerEditor data ->
            data.answer.label

        ReferenceEditor data ->
            Reference.getVisibleName data.reference

        ExpertEditor data ->
            data.expert.name


getEditorUuid : Editor -> String
getEditorUuid editor =
    case editor of
        KMEditor data ->
            data.knowledgeModel.uuid

        ChapterEditor data ->
            data.chapter.uuid

        TagEditor data ->
            data.tag.uuid

        IntegrationEditor data ->
            data.integration.uuid

        QuestionEditor data ->
            Question.getUuid data.question

        AnswerEditor data ->
            data.answer.uuid

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

        TagEditor data ->
            data.parentUuid

        IntegrationEditor data ->
            data.parentUuid

        QuestionEditor data ->
            data.parentUuid

        AnswerEditor data ->
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
toggleEditorOpen editor =
    case editor of
        KMEditor data ->
            KMEditor { data | treeOpen = not data.treeOpen }

        ChapterEditor data ->
            ChapterEditor { data | treeOpen = not data.treeOpen }

        TagEditor data ->
            TagEditor { data | treeOpen = not data.treeOpen }

        IntegrationEditor data ->
            IntegrationEditor { data | treeOpen = not data.treeOpen }

        QuestionEditor data ->
            QuestionEditor { data | treeOpen = not data.treeOpen }

        AnswerEditor data ->
            AnswerEditor { data | treeOpen = not data.treeOpen }

        ReferenceEditor data ->
            ReferenceEditor { data | treeOpen = not data.treeOpen }

        ExpertEditor data ->
            ExpertEditor { data | treeOpen = not data.treeOpen }


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

        TagEditor data ->
            data.editorState == Deleted

        IntegrationEditor data ->
            data.editorState == Deleted

        QuestionEditor data ->
            data.editorState == Deleted

        AnswerEditor data ->
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

        TagEditor data ->
            isTagEditorDirty data

        IntegrationEditor data ->
            isIntegrationEditorDirty data

        QuestionEditor data ->
            isQuestionEditorDirty data

        AnswerEditor data ->
            isAnswerEditorDirty data

        ReferenceEditor data ->
            isReferenceEditorDirty data

        ExpertEditor data ->
            isExpertEditorDirty data


isKMEditorDirty : KMEditorData -> Bool
isKMEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || editorData.chapters.dirty
        || editorData.tags.dirty
        || editorData.integrations.dirty


isChapterEditorDirty : ChapterEditorData -> Bool
isChapterEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || editorData.questions.dirty


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
        || editorData.itemTemplateQuestions.dirty
        || editorData.references.dirty
        || editorData.experts.dirty


isAnswerEditorDirty : AnswerEditorData -> Bool
isAnswerEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form
        || editorData.followUps.dirty


isReferenceEditorDirty : ReferenceEditorData -> Bool
isReferenceEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


isExpertEditorDirty : ExpertEditorData -> Bool
isExpertEditorDirty editorData =
    (editorData.editorState == Added)
        || formChanged editorData.form


updateKMEditorData : EditorContext -> EditorState -> KnowledgeModelForm -> KMEditorData -> KMEditorData
updateKMEditorData editorContext newState form editorData =
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


updateChapterEditorData : EditorContext -> EditorState -> ChapterForm -> ChapterEditorData -> ChapterEditorData
updateChapterEditorData editorContext newState form editorData =
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


updateTagEditorData : EditorContext -> EditorState -> TagForm -> TagEditorData -> TagEditorData
updateTagEditorData editorContext newState form editorData =
    let
        newTag =
            updateTagWithForm editorData.tag form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , tag = newTag
        , form = initTagForm newTag
    }


updateIntegrationEditorData : EditorContext -> EditorState -> IntegrationForm -> IntegrationEditorData -> IntegrationEditorData
updateIntegrationEditorData editorContext newState form editorData =
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


updateQuestionEditorData : EditorContext -> EditorState -> QuestionForm -> QuestionEditorData -> QuestionEditorData
updateQuestionEditorData editorContext newState form editorData =
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
        , form = initQuestionForm newQuestion
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


updateAnswerEditorData : EditorContext -> EditorState -> AnswerForm -> AnswerEditorData -> AnswerEditorData
updateAnswerEditorData editorContext newState form editorData =
    let
        newAnswer =
            updateAnswerWithForm editorData.answer form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , answer = newAnswer
        , followUps = Children.cleanDirty editorData.followUps
        , form = initAnswerForm editorContext newAnswer
    }


updateReferenceEditorData : EditorContext -> EditorState -> ReferenceForm -> ReferenceEditorData -> ReferenceEditorData
updateReferenceEditorData editorContext newState form editorData =
    let
        newReference =
            updateReferenceWithForm editorData.reference form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , reference = newReference
        , form = initReferenceForm newReference
    }


updateExpertEditorData : EditorContext -> EditorState -> ExpertForm -> ExpertEditorData -> ExpertEditorData
updateExpertEditorData editorContext newState form editorData =
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
