module KMEditor.Editor.KMEditor.Update.Chapter exposing
    ( addQuestion
    , deleteChapter
    , removeChapter
    , updateChapterForm
    , updateIfKMEditor
    , withGenerateChapterEditEvent
    )

import Form
import KMEditor.Common.KnowledgeModel.Question as Question
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor.KMEditor.Models.Forms exposing (chapterFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddChapterEvent, createDeleteChapterEvent, createEditChapterEvent)
import Msgs
import Random exposing (Seed)


updateChapterForm : Model -> Form.Msg -> ChapterEditorData -> Model
updateChapterForm =
    updateForm
        { formValidation = chapterFormValidation
        , createEditor = ChapterEditor
        }


withGenerateChapterEditEvent : Seed -> Model -> ChapterEditorData -> (Seed -> Model -> ChapterEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateChapterEditEvent =
    withGenerateEvent
        { isDirty = isChapterEditorDirty
        , formValidation = chapterFormValidation
        , createEditor = ChapterEditor
        , alert = "Please fix the chapter errors first."
        , createAddEvent = createAddChapterEvent
        , createEditEvent = createEditChapterEvent
        , updateEditorData = updateChapterEditorData
        , updateEditors = Nothing
        }


deleteChapter : Seed -> Model -> String -> ChapterEditorData -> ( Seed, Model )
deleteChapter =
    deleteEntity
        { removeEntity = removeChapter
        , createEditor = ChapterEditor
        , createDeleteEvent = createDeleteChapterEvent
        }


removeChapter : (String -> Children -> Children) -> String -> Editor -> Editor
removeChapter removeFn uuid =
    updateIfKMEditor (\data -> { data | chapters = removeFn uuid data.chapters })


updateIfKMEditor : (KMEditorData -> KMEditorData) -> Editor -> Editor
updateIfKMEditor update editor =
    case editor of
        KMEditor kmEditorData ->
            KMEditor <| update kmEditorData

        _ ->
            editor


addQuestion : Cmd Msgs.Msg -> Seed -> Model -> ChapterEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addQuestion =
    addEntity
        { newEntity = Question.new
        , createEntityEditor = createQuestionEditor
        , addEntity = addChapterQuestion
        }
