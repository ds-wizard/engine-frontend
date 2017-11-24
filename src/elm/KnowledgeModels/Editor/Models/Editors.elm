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
        , questionsDirty : Bool
        , order : Int
        }


type QuestionEditor
    = QuestionEditor
        { active : Bool
        , form : Form () QuestionForm
        , question : Question
        , answers : List AnswerEditor
        , references : List ReferenceEditor
        , experts : List ExpertEditor
        , order : Int
        }



-- TODO: Refactor follwoing editors


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
            List.indexedMap (createChapterEditor False) knowledgeModel.chapters
    in
    KnowledgeModelEditor
        { active = True
        , form = form
        , knowledgeModel = knowledgeModel
        , chapters = chapters
        , chaptersDirty = False
        }


createChapterEditor : Bool -> Int -> Chapter -> ChapterEditor
createChapterEditor active order chapter =
    let
        form =
            initChapterForm chapter

        questions =
            List.indexedMap (createQuestionEditor False) chapter.questions
    in
    ChapterEditor
        { active = active
        , form = form
        , chapter = chapter
        , questions = questions
        , questionsDirty = False
        , order = order
        }


getChapterUuid : ChapterEditor -> String
getChapterUuid (ChapterEditor chapterEditor) =
    chapterEditor.chapter.uuid


activateChapter : ChapterEditor -> ChapterEditor
activateChapter (ChapterEditor chapterEditor) =
    ChapterEditor { chapterEditor | active = True }


matchChapter : String -> ChapterEditor -> Bool
matchChapter uuid (ChapterEditor chapterEditor) =
    chapterEditor.chapter.uuid == uuid


createQuestionEditor : Bool -> Int -> Question -> QuestionEditor
createQuestionEditor active order question =
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
    QuestionEditor
        { active = active
        , form = form
        , question = question
        , answers = answers
        , references = references
        , experts = experts
        , order = order
        }


getQuestionUuid : QuestionEditor -> String
getQuestionUuid (QuestionEditor questionEditor) =
    questionEditor.question.uuid


activateQuestion : QuestionEditor -> QuestionEditor
activateQuestion (QuestionEditor questionEditor) =
    QuestionEditor { questionEditor | active = True }


matchQuestion : String -> QuestionEditor -> Bool
matchQuestion uuid (QuestionEditor questionEditor) =
    questionEditor.question.uuid == uuid


createAnswerEditor : Answer -> AnswerEditor
createAnswerEditor answer =
    let
        form =
            answerFormInitials answer
                |> initForm answerFormValidation

        questions =
            []
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
