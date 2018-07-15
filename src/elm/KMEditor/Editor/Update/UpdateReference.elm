module KMEditor.Editor.Update.UpdateReference exposing (..)

import Form
import KMEditor.Common.Models.Events exposing (Event, createEditReferenceEvent)
import KMEditor.Common.Models.Path exposing (Path)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (initReferenceForm, referenceFormValidation, updateReferenceWithForm)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> Path -> ReferenceEditor -> ( Seed, ReferenceEditor, Maybe Event )
formMsg formMsg seed path ((ReferenceEditor re) as editor) =
    case ( formMsg, Form.getOutput re.form, isReferenceEditorDirty editor ) of
        ( Form.Submit, Just referenceForm, True ) ->
            let
                newReference =
                    updateReferenceWithForm re.reference referenceForm

                newForm =
                    initReferenceForm newReference

                newEditor =
                    ReferenceEditor { re | active = False, form = newForm, reference = newReference }

                ( event, newSeed ) =
                    createEditReferenceEvent newReference path seed
            in
            ( newSeed, newEditor, Just event )

        ( Form.Submit, Just referenceForm, False ) ->
            ( seed, ReferenceEditor { re | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update referenceFormValidation formMsg re.form
            in
            ( seed, ReferenceEditor { re | form = newForm }, Nothing )


cancel : Seed -> ReferenceEditor -> ( Seed, ReferenceEditor, Maybe Event )
cancel seed (ReferenceEditor re) =
    let
        newForm =
            initReferenceForm re.reference
    in
    ( seed, ReferenceEditor { re | active = False, form = newForm }, Nothing )
