module KMEditor.Editor2.Update.Chapter exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (newQuestion)
import KMEditor.Common.Models.Path exposing (PathNode(ChapterPathNode))
import KMEditor.Editor2.Models exposing (Model)
import KMEditor.Editor2.Models.Children as Children exposing (Children)
import KMEditor.Editor2.Models.Editors exposing (..)
import KMEditor.Editor2.Models.Forms exposing (chapterFormValidation)
import KMEditor.Editor2.Update.Abstract exposing (addEntity, deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor2.Update.Events exposing (createAddChapterEvent, createDeleteChapterEvent, createEditChapterEvent)
import Msgs
import Random.Pcg exposing (Seed)


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


addQuestion : Seed -> Model -> ChapterEditorData -> ( Seed, Model, Cmd Msgs.Msg )
addQuestion =
    addEntity
        { newEntity = newQuestion
        , createEntityEditor = createQuestionEditor
        , createPathNode = ChapterPathNode
        , addEntity = addChapterQuestion
        }
