module KMEditor.Editor.Models.Editors exposing (..)

import Common.Form exposing (CustomFormError)
import Dict exposing (Dict)
import Form exposing (Form)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Path exposing (Path, PathNode(..))
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.Forms exposing (..)


type EditorState
    = Initial
    | Edited
    | Deleted
    | Added
    | AddedEdited


type Editor
    = KMEditor KMEditorData
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
    , answers : Children
    , answerItemTemplateQuestions : Children
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


createKnowledgeModelEditor : KnowledgeModel -> Dict String Editor -> Dict String Editor
createKnowledgeModelEditor km editors =
    let
        editor =
            KMEditor
                { uuid = km.uuid
                , knowledgeModel = km
                , form = initKnowledgeModelFrom km
                , chapters = Children.init <| List.map .uuid km.chapters
                , treeOpen = True
                , editorState = Initial
                , path = []
                }

        currentPath =
            [ KMPathNode km.uuid ]

        withChapters =
            List.foldl (createChapterEditor currentPath Initial) editors km.chapters
    in
    Dict.insert km.uuid editor withChapters


createChapterEditor : Path -> EditorState -> Chapter -> Dict String Editor -> Dict String Editor
createChapterEditor path editorState chapter editors =
    let
        editor =
            ChapterEditor
                { uuid = chapter.uuid
                , chapter = chapter
                , form = initChapterForm chapter
                , questions = Children.init <| List.map .uuid chapter.questions
                , treeOpen = False
                , editorState = editorState
                , path = path
                }

        currentPath =
            path ++ [ ChapterPathNode chapter.uuid ]

        withQuestions =
            List.foldl (createQuestionEditor currentPath Initial) editors chapter.questions
    in
    Dict.insert chapter.uuid editor withQuestions


createQuestionEditor : Path -> EditorState -> Question -> Dict String Editor -> Dict String Editor
createQuestionEditor path editorState question editors =
    let
        answers =
            question.answers
                |> Maybe.withDefault []
                |> List.map .uuid

        answerItemTemplateQuestions =
            getAnswerItemTemplateQuestions question
                |> List.map .uuid

        editor =
            QuestionEditor
                { uuid = question.uuid
                , question = question
                , form = initQuestionForm question
                , answers = Children.init answers
                , answerItemTemplateQuestions = Children.init answerItemTemplateQuestions
                , references = Children.init <| List.map .uuid question.references
                , experts = Children.init <| List.map .uuid question.experts
                , treeOpen = False
                , editorState = editorState
                , path = path
                }

        currentPath =
            path ++ [ QuestionPathNode question.uuid ]

        withAnswers =
            List.foldl (createAnswerEditor currentPath Initial) editors <| Maybe.withDefault [] <| question.answers

        withAnswerItemTemplateQuestions =
            List.foldl (createQuestionEditor currentPath Initial) withAnswers <| getAnswerItemTemplateQuestions question

        withReferences =
            List.foldl (createReferenceEditor currentPath Initial) withAnswerItemTemplateQuestions question.references

        withExperts =
            List.foldl (createExpertEditor currentPath Initial) withReferences question.experts
    in
    Dict.insert question.uuid editor withExperts


createAnswerEditor : Path -> EditorState -> Answer -> Dict String Editor -> Dict String Editor
createAnswerEditor path editorState answer editors =
    let
        followUps =
            getFollowUpQuestions answer
                |> List.map .uuid

        editor =
            AnswerEditor
                { uuid = answer.uuid
                , answer = answer
                , form = initAnswerForm answer
                , followUps = Children.init followUps
                , treeOpen = False
                , editorState = editorState
                , path = path
                }

        currentPath =
            path ++ [ AnswerPathNode answer.uuid ]

        withFollowUps =
            List.foldl (createQuestionEditor currentPath Initial) editors <| getFollowUpQuestions answer
    in
    Dict.insert answer.uuid editor withFollowUps


createReferenceEditor : Path -> EditorState -> Reference -> Dict String Editor -> Dict String Editor
createReferenceEditor path editorState reference editors =
    let
        editor =
            ReferenceEditor
                { uuid = reference.uuid
                , reference = reference
                , form = initReferenceForm reference
                , treeOpen = False
                , editorState = editorState
                , path = path
                }
    in
    Dict.insert reference.uuid editor editors


createExpertEditor : Path -> EditorState -> Expert -> Dict String Editor -> Dict String Editor
createExpertEditor path editorState expert editors =
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


deleteQuestionEditor : QuestionEditorData -> Dict String Editor -> Dict String Editor
deleteQuestionEditor editorData editors =
    editors
        |> deleteEditors editorData.answers
        |> deleteEditors editorData.answerItemTemplateQuestions
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

        QuestionEditor data ->
            data.question.title

        AnswerEditor data ->
            data.answer.label

        ReferenceEditor data ->
            data.reference.chapter

        ExpertEditor data ->
            data.expert.name


getEditorUuid : Editor -> String
getEditorUuid editor =
    case editor of
        KMEditor data ->
            data.knowledgeModel.uuid

        ChapterEditor data ->
            data.chapter.uuid

        QuestionEditor data ->
            data.question.uuid

        AnswerEditor data ->
            data.answer.uuid

        ReferenceEditor data ->
            data.reference.uuid

        ExpertEditor data ->
            data.expert.uuid


getEditorPath : Editor -> Path
getEditorPath editor =
    case editor of
        KMEditor data ->
            data.path

        ChapterEditor data ->
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

        QuestionEditor data ->
            data.editorState == Deleted

        AnswerEditor data ->
            data.editorState == Deleted

        ReferenceEditor data ->
            data.editorState == Deleted

        ExpertEditor data ->
            data.editorState == Deleted


isKMEditorDirty : KMEditorData -> Bool
isKMEditorDirty editorData =
    formChanged editorData.form || editorData.chapters.dirty


isChapterEditorDirty : ChapterEditorData -> Bool
isChapterEditorDirty editorData =
    formChanged editorData.form || editorData.questions.dirty


isQuestionEditorDirty : QuestionEditorData -> Bool
isQuestionEditorDirty editorData =
    formChanged editorData.form || editorData.answers.dirty || editorData.answerItemTemplateQuestions.dirty || editorData.references.dirty || editorData.experts.dirty


isAnswerEditorDirty : AnswerEditorData -> Bool
isAnswerEditorDirty editorData =
    formChanged editorData.form || editorData.followUps.dirty


isReferenceEditorDirty : ReferenceEditorData -> Bool
isReferenceEditorDirty editorData =
    formChanged editorData.form


isExpertEditorDirty : ExpertEditorData -> Bool
isExpertEditorDirty editorData =
    formChanged editorData.form


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


updateQuestionEditorData : EditorState -> QuestionForm -> QuestionEditorData -> QuestionEditorData
updateQuestionEditorData newState form editorData =
    let
        newQuestion =
            updateQuestionWithForm editorData.question form

        newAnswers =
            if newQuestion.type_ == "options" then
                Children.cleanDirty editorData.answers
            else
                Children.init []

        newAnswerItemTemplateQuestions =
            if newQuestion.type_ == "list" then
                Children.cleanDirty editorData.answerItemTemplateQuestions
            else
                Children.init []
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , question = newQuestion
        , answers = newAnswers
        , answerItemTemplateQuestions = newAnswerItemTemplateQuestions
        , references = Children.cleanDirty editorData.references
        , experts = Children.cleanDirty editorData.experts
        , form = initQuestionForm newQuestion
    }


updateEditorsWithQuestion : QuestionEditorData -> QuestionEditorData -> Dict String Editor -> Dict String Editor
updateEditorsWithQuestion newEditorData oldEditorData editors =
    case newEditorData.question.type_ of
        "options" ->
            deleteEditors oldEditorData.answerItemTemplateQuestions editors

        "list" ->
            deleteEditors oldEditorData.answers editors

        _ ->
            editors
                |> deleteEditors oldEditorData.answerItemTemplateQuestions
                |> deleteEditors oldEditorData.answers


updateAnswerEditorData : EditorState -> AnswerForm -> AnswerEditorData -> AnswerEditorData
updateAnswerEditorData newState form editorData =
    let
        newAnswer =
            updateAnswerWithForm editorData.answer form
    in
    { editorData
        | editorState = getNewState editorData.editorState newState
        , answer = newAnswer
        , followUps = Children.cleanDirty editorData.followUps
        , form = initAnswerForm newAnswer
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
        , expert = updateExpertWithForm editorData.expert form
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


addChapterQuestion : Question -> ChapterEditorData -> Editor
addChapterQuestion question editorData =
    ChapterEditor
        { editorData
            | questions = Children.addChild question.uuid editorData.questions
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
            | answerItemTemplateQuestions = Children.addChild question.uuid editorData.answerItemTemplateQuestions
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }


addQuestionReference : Reference -> QuestionEditorData -> Editor
addQuestionReference reference editorData =
    QuestionEditor
        { editorData
            | references = Children.addChild reference.uuid editorData.references
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
            | followUps = Children.addChild followUp.uuid editorData.followUps
            , treeOpen = True
            , editorState = getNewState editorData.editorState Edited
        }
