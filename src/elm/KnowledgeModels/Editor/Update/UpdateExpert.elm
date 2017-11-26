module KnowledgeModels.Editor.Update.UpdateExpert exposing (..)

import Form
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Events exposing (Event, createEditExpertEvent)
import KnowledgeModels.Editor.Models.Forms exposing (expertFormValidation, initExpertForm, updateExpertWithForm)
import KnowledgeModels.Editor.Update.Utils exposing (formChanged)
import Random.Pcg exposing (Seed)


updateExpertFormMsg : Form.Msg -> Seed -> Question -> Chapter -> KnowledgeModel -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
updateExpertFormMsg formMsg seed question chapter knowledgeModel ((ExpertEditor editor) as originalEditor) =
    case ( formMsg, Form.getOutput editor.form, formChanged editor.form ) of
        ( Form.Submit, Just expertForm, True ) ->
            let
                newExpert =
                    updateExpertWithForm editor.expert expertForm

                newForm =
                    initExpertForm newExpert

                newEditor =
                    { editor | active = False, form = newForm, expert = newExpert }

                ( event, newSeed ) =
                    createEditExpertEvent question chapter knowledgeModel [] seed newExpert
            in
            ( newSeed, ExpertEditor { editor | active = False }, Just event )

        ( Form.Submit, Just expertForm, False ) ->
            ( seed, ExpertEditor { editor | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update expertFormValidation formMsg editor.form
            in
            ( seed, ExpertEditor { editor | form = newForm }, Nothing )


updateExpertCancel : Seed -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
updateExpertCancel seed (ExpertEditor editor) =
    let
        newForm =
            initExpertForm editor.expert
    in
    ( seed, ExpertEditor { editor | active = False, form = newForm }, Nothing )
