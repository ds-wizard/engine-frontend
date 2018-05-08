module KnowledgeModels.Editor.Update.UpdateKnowledgeModel exposing (..)

{-|

@docs updateKnowledgeModelFormMsg
@docs updateKnowledgeModelAddChapter, updateKnowledgeModelViewChapter, updateKnowledgeModelDeleteChapter

-}

import Form
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (newChapter)
import KnowledgeModels.Editor.Models.Events exposing (..)
import KnowledgeModels.Editor.Models.Forms exposing (knowledgeModelFormValidation, updateKnowledgeModelWithForm)
import KnowledgeModels.Editor.Update.Utils exposing (..)
import Random.Pcg exposing (Seed)


{-| -}
updateKnowledgeModelFormMsg : Form.Msg -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModelFormMsg formMsg seed ((KnowledgeModelEditor editor) as originalEditor) =
    case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.chaptersDirty ) of
        ( Form.Submit, Just knowledgeModelForm, True ) ->
            let
                newKnowledgeModel =
                    updateKnowledgeModelWithForm editor.knowledgeModel knowledgeModelForm

                ( event, newSeed ) =
                    editor.chapters
                        |> List.map getChapterUuid
                        |> createEditKnowledgeModelEvent seed newKnowledgeModel
            in
            ( newSeed, originalEditor, Just event, True )

        ( Form.Submit, Just chapterForm, False ) ->
            ( seed, originalEditor, Nothing, True )

        _ ->
            let
                newForm =
                    Form.update knowledgeModelFormValidation formMsg editor.form
            in
            ( seed, KnowledgeModelEditor { editor | form = newForm }, Nothing, False )


{-| -}
updateKnowledgeModelAddChapter : Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModelAddChapter seed (KnowledgeModelEditor editor) =
    let
        ( newSeed, newChapters, event ) =
            addChild
                seed
                editor.chapters
                createChapterEditor
                newChapter
                (createAddChapterEvent editor.knowledgeModel)
    in
    ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, Just event, False )


{-| -}
updateKnowledgeModelViewChapter : String -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModelViewChapter uuid seed (KnowledgeModelEditor editor) =
    let
        newChapters =
            updateInList editor.chapters (matchChapter uuid) activateChapter
    in
    ( seed, KnowledgeModelEditor { editor | chapters = newChapters }, Nothing, False )


{-| -}
updateKnowledgeModelDeleteChapter : String -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModelDeleteChapter uuid seed (KnowledgeModelEditor editor) =
    let
        newChapters =
            List.filter (not << matchChapter uuid) editor.chapters

        ( event, newSeed ) =
            createDeleteChapterEvent editor.knowledgeModel seed uuid
    in
    ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, Just event, False )
