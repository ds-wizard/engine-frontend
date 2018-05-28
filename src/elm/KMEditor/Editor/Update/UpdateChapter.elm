module KMEditor.Editor.Update.UpdateChapter exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, newQuestion)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (..)
import KMEditor.Editor.Update.Utils exposing (addChild, updateInList)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> Path -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
formMsg formMsg seed path ((ChapterEditor ce) as editor) =
    case ( formMsg, Form.getOutput ce.form, isChapterEditorDirty editor ) of
        ( Form.Submit, Just chapterForm, True ) ->
            let
                -- Create new chapter based on the form fields
                newChapter =
                    updateChapterWithForm ce.chapter chapterForm

                -- Initialize new form with new chapter values
                newForm =
                    initChapterForm newChapter

                -- Set the new order for the questions
                newQuestions =
                    List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) ce.questions

                -- Create new editor structure
                newEditor =
                    ChapterEditor { ce | active = False, form = newForm, chapter = newChapter, questions = newQuestions, questionsDirty = False }

                -- Create the new event
                ( event, newSeed ) =
                    createEditChapterEvent newEditor path seed
            in
            ( newSeed, newEditor, Just event )

        ( Form.Submit, Just chapterForm, False ) ->
            ( seed, ChapterEditor { ce | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update chapterFormValidation formMsg ce.form
            in
            ( seed, ChapterEditor { ce | form = newForm }, Nothing )


cancel : Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
cancel seed (ChapterEditor ce) =
    let
        -- Reinitialize the form with the original chapter values
        newForm =
            initChapterForm ce.chapter

        -- Reset the order of the questions to the original one
        newQuestions =
            List.sortBy (\(QuestionEditor qe) -> qe.order) ce.questions
    in
    ( seed, ChapterEditor { ce | active = False, form = newForm, questions = newQuestions, questionsDirty = False }, Nothing )


addQuestion : Seed -> Path -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
addQuestion seed path (ChapterEditor ce) =
    let
        ( newSeed, newQuestions, event ) =
            addChild
                seed
                ce.questions
                createQuestionEditor
                newQuestion
                (flip createAddQuestionEvent path)
    in
    ( newSeed, ChapterEditor { ce | questions = newQuestions }, Just event )


viewQuestion : String -> Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
viewQuestion uuid seed (ChapterEditor ce) =
    let
        newQuestions =
            updateInList ce.questions (matchQuestion uuid) activateQuestion
    in
    ( seed, ChapterEditor { ce | questions = newQuestions }, Nothing )


deleteQuestion : String -> Seed -> Path -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
deleteQuestion uuid seed path (ChapterEditor ce) =
    let
        newQuestions =
            List.filter (not << matchQuestion uuid) ce.questions

        ( event, newSeed ) =
            createDeleteQuestionEvent uuid path seed
    in
    ( newSeed, ChapterEditor { ce | questions = newQuestions }, Just event )
