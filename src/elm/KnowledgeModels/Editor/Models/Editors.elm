module KnowledgeModels.Editor.Models.Editors exposing (..)

import Form exposing (Form)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Forms exposing (..)


type KnowledgeModelEditor
    = KnowledgeModelEditor
        { active : Bool
        , form : Form () KnowledgeModelForm
        , knowledgeModel : KnowledgeModel
        , chapters : List ChapterEditor
        , chaptersDirty : Bool
        }


type ChapterEditor
    = ChapterEditor
        { active : Bool
        , form : Form () ChapterForm
        , chapter : Chapter
        , questions : List QuestionEditor
        , order : Int
        }



-- TODO: Refactor follwoing editors


type QuestionEditor
    = QuestionEditor Bool (Form () QuestionForm) (List AnswerEditor) (List ReferenceEditor) (List ExpertEditor)


type AnswerEditor
    = AnswerEditor Bool (Form () AnswerForm) (List QuestionEditor)


type ReferenceEditor
    = ReferenceEditor Bool (Form () ReferenceForm)


type ExpertEditor
    = ExpertEditor Bool (Form () ExpertForm)


createKnowledgeModelEditor : KnowledgeModel -> KnowledgeModelEditor
createKnowledgeModelEditor knowledgeModel =
    let
        form =
            knowledgeModelFormInitials knowledgeModel
                |> initForm knowledgeModelFormValidation

        chapters =
            List.indexedMap createChapterEditor knowledgeModel.chapters
    in
    KnowledgeModelEditor
        { active = True
        , form = form
        , knowledgeModel = knowledgeModel
        , chapters = chapters
        , chaptersDirty = False
        }


updateKnowledgeModelWithForm : KnowledgeModel -> KnowledgeModelForm -> KnowledgeModel
updateKnowledgeModelWithForm knowledgeModel knowledgeModelForm =
    { knowledgeModel | name = knowledgeModelForm.name }


createChapterEditor : Int -> Chapter -> ChapterEditor
createChapterEditor order chapter =
    let
        form =
            chapterFormInitials chapter
                |> initForm chapterFormValidation

        questions =
            List.map createQuestionEditor chapter.questions
    in
    ChapterEditor
        { active = False
        , form = form
        , chapter = chapter
        , questions = questions
        , order = order
        }


updateChapterEditorWithForm : Chapter -> ChapterForm -> Chapter
updateChapterEditorWithForm chapter chapterForm =
    { chapter | title = chapterForm.title, text = chapterForm.text }


createQuestionEditor : Question -> QuestionEditor
createQuestionEditor question =
    let
        form =
            questionFormInitials question
                |> initForm questionFormValidation

        answers =
            List.map createAnswerEditor question.answers

        references =
            List.map createReferenceEditor question.references

        experts =
            List.map createExpertEditor question.experts
    in
    QuestionEditor False form answers references experts


createAnswerEditor : Answer -> AnswerEditor
createAnswerEditor answer =
    let
        form =
            answerFormInitials answer
                |> initForm answerFormValidation

        questions =
            case answer.following of
                Followings questions ->
                    List.map createQuestionEditor questions
    in
    AnswerEditor False form questions


createReferenceEditor : Reference -> ReferenceEditor
createReferenceEditor reference =
    let
        form =
            referenceFormInitials reference
                |> initForm referenceFormValidation
    in
    ReferenceEditor False form


createExpertEditor : Expert -> ExpertEditor
createExpertEditor expert =
    let
        form =
            expertFormInitials expert
                |> initForm expertFormValidation
    in
    ExpertEditor False form
