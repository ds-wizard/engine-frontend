module KMEditor.Editor.Models.Editors exposing
    ( AnswerEditorData
    , ChapterEditorData
    , Editor(..)
    , EditorLike
    , EditorState(..)
    , ExpertEditorData
    , KMEditorData
    , QuestionEditorData
    , ReferenceEditorData
    , TagEditorData
    , addAnswerFollowUp
    , addChapterQuestion
    , addKMChapter
    , addKMTag
    , addQuestionAnswer
    , addQuestionAnswerItemTemplateQuestion
    , addQuestionExpert
    , addQuestionReference
    , createAnswerEditor
    , createChapterEditor
    , createExpertEditor
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
    , getEditorPath
    , getEditorTitle
    , getEditorUuid
    , getNewState
    , isAnswerEditorDirty
    , isChapterEditorDirty
    , isEditorDeleted
    , isEditorDirty
    , isExpertEditorDirty
    , isKMEditorDirty
    , isQuestionEditorDirty
    , isReferenceEditorDirty
    , isTagEditorDirty
    , toggleEditorOpen
    , updateAnswerEditorData
    , updateChapterEditorData
    , updateEditorsWithQuestion
    , updateExpertEditorData
    , updateKMEditorData
    , updateQuestionEditorData
    , updateReferenceEditorData
    , updateTagEditorData
    )

import Common.Form exposing (CustomFormError)
import Dict exposing (Dict)
import Form exposing (Form)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Path exposing (Path, PathNode(..))
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.EditorContext exposing (EditorContext)
import KMEditor.Editor.Models.Forms exposing (..)


type EditorState
    = Initial
    | Edited
    | Deleted
    | Added
    | AddedEdited


type Editor
    = KMEditor KMEditorData
    | TagEditor TagEditorData
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
        , path : Path
    }


type alias KMEditorData =
    { uuid : String
    , knowledgeModel : KnowledgeModel
    , form : Form CustomFormError KnowledgeModelForm
    , chapters : Children
    , tags : Children
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }


type alias TagEditorData =
    { uuid : String
    , tag : Tag
    , form : Form CustomFormError TagForm
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }


type alias ChapterEditorData =
    { uuid : String
    , chapter : Chapter
    , form : Form CustomFormError ChapterForm
    , questions : Children
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }


type alias QuestionEditorData =
    { uuid : String
    , question : Question
    , form : Form CustomFormError QuestionForm
    , tagUuids : List String
    , answers : Children
    , itemQuestions : Children
    , references : Children
    , experts : Children
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }


type alias AnswerEditorData =
    { uuid : String
    , answer : Answer
    , form : Form CustomFormError AnswerForm
    , followUps : Children
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }


type alias ReferenceEditorData =
    { uuid : String
    , reference : Reference
    , form : Form CustomFormError ReferenceForm
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }


type alias ExpertEditorData =
    { uuid : String
    , expert : Expert
    , form : Form CustomFormError ExpertForm
    , treeOpen : Bool
    , editorState : EditorState
    , path : Path
    }



{- constructors -}


createKnowledgeModelEditor : EditorContext -> KnowledgeModel -> Dict String Editor -> Dict String Editor
createKnowledgeModelEditor editorContext km editors =
    let
        editor =
            KMEditor
                { uuid = km.uuid
                , knowledgeModel = km
                , form = initKnowledgeModelFrom km
                , chapters = Children.init <| List.map .uuid km.chapters
                , tags = Children.init <| List.map .uuid km.tags
                , treeOpen = True
                , editorState = Initial
                , path = []
                }

        currentPath =
            [ KMPathNode km.uuid ]

        withChapters =
            List.foldl (createChapterEditor editorContext currentPath Initial) editors km.chapters

        withTags =
            List.foldl (createTagEditor editorContext currentPath Initial) withChapters km.tags
    in
    Dict.insert km.uuid editor withTags


createChapterEditor : EditorContext -> Path -> EditorState -> Chapter -> Dict String Editor -> Dict String Editor
createChapterEditor editorContext path editorState chapter editors =
    let
        editor =
            ChapterEditor
                { uuid = chapter.uuid
                , chapter = chapter
                , form = initChapterForm chapter
                , questions = Children.init <| List.map getQuestionUuid chapter.questions
                , treeOpen = False
                , editorState = editorState
                , path = path
                }

        currentPath =
            path ++ [ ChapterPathNode chapter.uuid ]

        withQuestions =
            List.foldl (createQuestionEditor editorContext currentPath Initial) editors chapter.questions
    in
    Dict.insert chapter.uuid editor withQuestions


createTagEditor : EditorContext -> Path -> EditorState -> Tag -> Dict String Editor -> Dict String Editor
createTagEditor editorContext path editorState tag editors =
    let
        editor =
            TagEditor
                { uuid = tag.uuid
                , tag = tag
                , form = initTagForm tag
                , treeOpen = False
                , editorState = editorState
                , path = path
                }
    in
    Dict.insert tag.uuid editor editors


createQuestionEditor : EditorContext -> Path -> EditorState -> Question -> Dict String Editor -> Dict String Editor
createQuestionEditor editorContext path editorState question editors =
    let
        questionUuid =
            getQuestionUuid question

        answers =
            getQuestionAnswers question
                |> List.map .uuid

        itemQuestions =
            getQuestionItemQuestions question
                |> List.map getQuestionUuid

        editor =
            QuestionEditor
                { uuid = questionUuid
                , question = question
                , form = initQuestionForm question
                , tagUuids = getQuestionTagUuids question
                , answers = Children.init answers
                , itemQuestions = Children.init itemQuestions
                , references = Children.init <| List.map getReferenceUuid <| getQuestionReferences question
                , experts = Children.init <| List.map .uuid <| getQuestionExperts question
                , treeOpen = False
                , editorState = editorState
                , path = path
                }

        currentPath =
            path ++ [ QuestionPathNode questionUuid ]

        withAnswers =
            List.foldl (createAnswerEditor editorContext currentPath Initial) editors <| getQuestionAnswers question

        withAnswerItemTemplateQuestions =
            List.foldl (createQuestionEditor editorContext currentPath Initial) withAnswers <| getQuestionItemQuestions question

        withReferences =
            List.foldl (createReferenceEditor editorContext currentPath Initial) withAnswerItemTemplateQuestions <| getQuestionReferences question

        withExperts =
            List.foldl (createExpertEditor editorContext currentPath Initial) withReferences <| getQuestionExperts question
    in
    Dict.insert questionUuid editor withExperts


createAnswerEditor : EditorContext -> Path -> EditorState -> Answer -> Dict String Editor -> Dict String Editor
createAnswerEditor editorContext path editorState answer editors =
    let
        followUps =
            getFollowUpQuestions answer
                |> List.map getQuestionUuid

        editor =
            AnswerEditor
                { uuid = answer.uuid
                , answer = answer
                , form = initAnswerForm editorContext answer
                , followUps = Children.init followUps
                , treeOpen = False
                , editorState = editorState
                , path = path
                }

        currentPath =
            path ++ [ AnswerPathNode answer.uuid ]

        withFollowUps =
            List.foldl (createQuestionEditor editorContext currentPath Initial) editors <| getFollowUpQuestions answer
    in
    Dict.insert answer.uuid editor withFollowUps


createReferenceEditor : EditorContext -> Path -> EditorState -> Reference -> Dict String Editor -> Dict String Editor
createReferenceEditor editorContext path editorState reference editors =
    let
        referenceUuid =
            getReferenceUuid reference

        editor =
            ReferenceEditor
                { uuid = referenceUuid
                , reference = reference
                , form = initReferenceForm reference
                , treeOpen = False
                , editorState = editorState
                , path = path
                }
    in
    Dict.insert referenceUuid editor editors


createExpertEditor : EditorContext -> Path -> EditorState -> Expert -> Dict String Editor -> Dict String Editor
createExpertEditor editorContext path editorState expert editors =
    let
        editor =
            ExpertEditor
                { uuid = expert.uuid
                , expert = expert
                , form = initExpertForm expert
                , treeOpen = False
                , editorState = editorState
                , path = path
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


deleteQuestionEditor : QuestionEditorData -> Dict String Editor -> Dict String Editor
deleteQuestionEditor editorData editors =
    editors
        |> deleteEditors editorData.answers
        |> deleteEditors editorData.itemQuestions
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

        QuestionEditor data ->
            getQuestionTitle data.question

        AnswerEditor data ->
            data.answer.label

        ReferenceEditor data ->
            getReferenceVisibleName data.reference

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

        QuestionEditor data ->
            getQuestionUuid data.question

        AnswerEditor data ->
            data.answer.uuid

        ReferenceEditor data ->
            getReferenceUuid data.reference

        ExpertEditor data ->
            data.expert.uuid


getEditorPath : Editor -> Path
getEditorPath editor =
    case editor of
        KMEditor data ->
            data.path

        ChapterEditor data ->
            data.path

        TagEditor data ->
            data.path

        QuestionEditor data ->
            data.path

        AnswerEditor data ->
            data.path

        ReferenceEditor data ->
            data.path

        ExpertEditor data ->
            data.path


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
    formChanged editorData.form || editorData.chapters.dirty || editorData.tags.dirty


isChapterEditorDirty : ChapterEditorData -> Bool
isChapterEditorDirty editorData =
    formChanged editorData.form || editorData.questions.dirty


isTagEditorDirty : TagEditorData -> Bool
isTagEditorDirty editorData =
    formChanged editorData.form


isQuestionEditorDirty : QuestionEditorData -> Bool
isQuestionEditorDirty editorData =
    formChanged editorData.form
        || (getQuestionTagUuids editorData.question /= editorData.tagUuids)
        || editorData.answers.dirty
        || editorData.itemQuestions.dirty
        || editorData.references.dirty
        || editorData.experts.dirty


isAnswerEditorDirty : AnswerEditorData -> Bool
isAnswerEditorDirty editorData =
    formChanged editorData.form || editorData.followUps.dirty


isReferenceEditorDirty : ReferenceEditorData -> Bool
isReferenceEditorDirty editorData =
    formChanged editorData.form


isExpertEditorDirty : ExpertEditorData -> Bool
isExpertEditorDirty editorData =
    formChanged editorData.form


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


updateQuestionEditorData : EditorContext -> EditorState -> QuestionForm -> QuestionEditorData -> QuestionEditorData
updateQuestionEditorData editorContext newState form editorData =
    let
        newQuestion =
            updateQuestionWithForm editorData.question form

        newAnswers =
            if isQuestionOptions newQuestion then
                Children.cleanDirty editorData.answers

            else
                Children.init []

        newAnswerItemTemplateQuestions =
            if isQuestionList newQuestion then
                Children.cleanDirty editorData.itemQuestions

            else
                Children.init []
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , question = newQuestion
        , answers = newAnswers
        , itemQuestions = newAnswerItemTemplateQuestions
        , references = Children.cleanDirty editorData.references
        , experts = Children.cleanDirty editorData.experts
        , form = initQuestionForm newQuestion
    }


updateEditorsWithQuestion : QuestionEditorData -> QuestionEditorData -> Dict String Editor -> Dict String Editor
updateEditorsWithQuestion newEditorData oldEditorData editors =
    case newEditorData.question of
        OptionsQuestion _ ->
            deleteEditors oldEditorData.itemQuestions editors

        ListQuestion _ ->
            deleteEditors oldEditorData.answers editors

        _ ->
            editors
                |> deleteEditors oldEditorData.itemQuestions
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


addChapterQuestion : Question -> ChapterEditorData -> Editor
addChapterQuestion question editorData =
    ChapterEditor
        { editorData
            | questions = Children.addChild (getQuestionUuid question) editorData.questions
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
            | itemQuestions = Children.addChild (getQuestionUuid question) editorData.itemQuestions
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionReference : Reference -> QuestionEditorData -> Editor
addQuestionReference reference editorData =
    QuestionEditor
        { editorData
            | references = Children.addChild (getReferenceUuid reference) editorData.references
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
            | followUps = Children.addChild (getQuestionUuid followUp) editorData.followUps
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }
