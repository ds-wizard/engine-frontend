module KMEditor.Editor.Update.UpdateExpert exposing (..)

import Form
import KMEditor.Common.Models.Events exposing (Event, createEditExpertEvent)
import KMEditor.Common.Models.Path exposing (Path)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (expertFormValidation, initExpertForm, updateExpertWithForm)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> Path -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
formMsg formMsg seed path ((ExpertEditor ee) as editor) =
    case ( formMsg, Form.getOutput ee.form, isExpertEditorDirty editor ) of
        ( Form.Submit, Just expertForm, True ) ->
            let
                newExpert =
                    updateExpertWithForm ee.expert expertForm

                newForm =
                    initExpertForm newExpert

                newEditor =
                    ExpertEditor { ee | active = False, form = newForm, expert = newExpert }

                ( event, newSeed ) =
                    createEditExpertEvent newExpert path seed
            in
            ( newSeed, newEditor, Just event )

        ( Form.Submit, Just expertForm, False ) ->
            ( seed, ExpertEditor { ee | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update expertFormValidation formMsg ee.form
            in
            ( seed, ExpertEditor { ee | form = newForm }, Nothing )


cancel : Seed -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
cancel seed (ExpertEditor ee) =
    let
        newForm =
            initExpertForm ee.expert
    in
    ( seed, ExpertEditor { ee | active = False, form = newForm }, Nothing )
