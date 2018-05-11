module KMEditor.Editor.Update.UpdateChapter exposing (..)

{-|

@docs updateChapterFormMsg, updateChapterCancel
@docs updateChapterAddQuestion, updateChapterViewQuestion, updateChapterDeleteQuestion

-}

import Form
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Entities exposing (KnowledgeModel, newQuestion)
import KMEditor.Editor.Models.Events exposing (..)
import KMEditor.Editor.Models.Forms exposing (..)
import KMEditor.Editor.Update.Utils exposing (addChild, formChanged, updateInList)
import List.Extra as List
import Random.Pcg exposing (Seed)


{-| -}
updateChapterFormMsg : Form.Msg -> Seed -> KnowledgeModel -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapterFormMsg formMsg seed knowledgeModel ((ChapterEditor editor) as originalEditor) =
    case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.questionsDirty ) of
        ( Form.Submit, Just chapterForm, True ) ->
            let
                -- Create new chapter based on the form fields
                newChapter =
                    updateChapterWithForm editor.chapter chapterForm

                -- Initialize new form with new chapter values
                newForm =
                    initChapterForm newChapter

                -- Get the question ids in correct order
                questionIds =
                    List.map (\(QuestionEditor e) -> e.question.uuid) editor.questions

                -- Set the new order for the questions
                newQuestions =
                    List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) editor.questions

                -- Create new editor structure
                newEditor =
                    { editor | active = False, form = newForm, chapter = newChapter, questions = newQuestions, questionsDirty = False }

                -- Create the new event
                ( event, newSeed ) =
                    createEditChapterEvent knowledgeModel questionIds seed newChapter
            in
            ( newSeed, ChapterEditor newEditor, Just event )

        ( Form.Submit, Just chapterForm, False ) ->
            ( seed, ChapterEditor { editor | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update chapterFormValidation formMsg editor.form
            in
            ( seed, ChapterEditor { editor | form = newForm }, Nothing )


{-| -}
updateChapterCancel : Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapterCancel seed (ChapterEditor editor) =
    let
        -- Reinitialize the form with the original chapter values
        newForm =
            initChapterForm editor.chapter

        -- Reset the order of the questions to the original one
        newQuestions =
            List.sortBy (\(QuestionEditor qe) -> qe.order) editor.questions
    in
    ( seed, ChapterEditor { editor | active = False, form = newForm, questions = newQuestions, questionsDirty = False }, Nothing )


{-| -}
updateChapterAddQuestion : Seed -> KnowledgeModel -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapterAddQuestion seed knowledgeModel (ChapterEditor editor) =
    let
        ( newSeed, newQuestions, event ) =
            addChild
                seed
                editor.questions
                createQuestionEditor
                newQuestion
                (createAddQuestionEvent editor.chapter knowledgeModel)
    in
    ( newSeed, ChapterEditor { editor | questions = newQuestions }, Just event )


{-| -}
updateChapterViewQuestion : String -> Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapterViewQuestion uuid seed (ChapterEditor editor) =
    let
        newQuestions =
            updateInList editor.questions (matchQuestion uuid) activateQuestion
    in
    ( seed, ChapterEditor { editor | questions = newQuestions }, Nothing )


{-| -}
updateChapterDeleteQuestion : String -> Seed -> KnowledgeModel -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapterDeleteQuestion uuid seed knowledgeModel (ChapterEditor editor) =
    let
        newQuestions =
            List.filter (not << matchQuestion uuid) editor.questions

        ( event, newSeed ) =
            createDeleteQuestionEvent editor.chapter knowledgeModel seed uuid
    in
    ( newSeed, ChapterEditor { editor | questions = newQuestions }, Just event )
