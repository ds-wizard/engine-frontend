module KMEditor.Editor.Update.UpdateKnowledgeModel exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (newChapter)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Common.Models.Path exposing (Path)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (initKnowledgeModelFrom, knowledgeModelFormValidation, updateKnowledgeModelWithForm)
import KMEditor.Editor.Update.Utils exposing (..)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
formMsg formMsg seed ((KnowledgeModelEditor kme) as editor) =
    case ( formMsg, Form.getOutput kme.form, isKnowledgeModelEditorDirty editor ) of
        ( Form.Submit, Just form, True ) ->
            let
                newKnowledgeModel =
                    updateKnowledgeModelWithForm kme.knowledgeModel form

                newForm =
                    initKnowledgeModelFrom newKnowledgeModel

                newChapters =
                    List.indexedMap (\i (ChapterEditor ce) -> ChapterEditor { ce | order = i }) kme.chapters

                newEditor =
                    KnowledgeModelEditor { kme | form = newForm, knowledgeModel = newKnowledgeModel, chapters = newChapters }

                ( event, newSeed ) =
                    createEditKnowledgeModelEvent newEditor seed
            in
            ( newSeed, newEditor, Just event, True )

        ( Form.Submit, Just from, False ) ->
            ( seed, editor, Nothing, True )

        _ ->
            let
                newForm =
                    Form.update knowledgeModelFormValidation formMsg kme.form
            in
            ( seed, KnowledgeModelEditor { kme | form = newForm }, Nothing, False )


addChapter : Seed -> Path -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
addChapter seed path (KnowledgeModelEditor kme) =
    let
        ( newSeed, newChapters, event ) =
            addChild
                seed
                kme.chapters
                createChapterEditor
                newChapter
                (flip createAddChapterEvent path)
    in
    ( newSeed, KnowledgeModelEditor { kme | chapters = newChapters }, Just event, False )


viewChapter : String -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
viewChapter uuid seed (KnowledgeModelEditor kme) =
    let
        newChapters =
            updateInList kme.chapters (matchChapter uuid) activateChapter
    in
    ( seed, KnowledgeModelEditor { kme | chapters = newChapters }, Nothing, False )


deleteChapter : String -> Seed -> Path -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
deleteChapter uuid seed path (KnowledgeModelEditor kme) =
    let
        newChapters =
            List.filter (not << matchChapter uuid) kme.chapters

        ( event, newSeed ) =
            createDeleteChapterEvent uuid path seed
    in
    ( newSeed, KnowledgeModelEditor { kme | chapters = newChapters }, Just event, False )
