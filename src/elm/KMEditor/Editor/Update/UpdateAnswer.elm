module KMEditor.Editor.Update.UpdateAnswer exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (answerFormValidation, initAnswerForm, updateAnswerWithForm)
import KMEditor.Editor.Update.Utils exposing (addChild, updateInList)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> Path -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
formMsg formMsg seed path ((AnswerEditor ae) as editor) =
    case ( formMsg, Form.getOutput ae.form, isAnswerEditorDirty editor ) of
        ( Form.Submit, Just answerForm, True ) ->
            let
                newAnswer =
                    updateAnswerWithForm ae.answer answerForm

                newForm =
                    initAnswerForm newAnswer

                newFollowUps =
                    List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) ae.followUps

                newEditor =
                    AnswerEditor { ae | active = False, form = newForm, answer = newAnswer, followUps = newFollowUps, followUpsDirty = False }

                ( event, newSeed ) =
                    createEditAnswerEvent newEditor path seed
            in
            ( newSeed, newEditor, Just event )

        ( Form.Submit, Just answerForm, False ) ->
            ( seed, AnswerEditor { ae | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update answerFormValidation formMsg ae.form
            in
            ( seed, AnswerEditor { ae | form = newForm }, Nothing )


cancel : Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
cancel seed (AnswerEditor ae) =
    let
        newForm =
            initAnswerForm ae.answer

        newFollowUps =
            List.sortBy (\(QuestionEditor qe) -> qe.order) ae.followUps
    in
    ( seed, AnswerEditor { ae | active = False, form = newForm, followUps = newFollowUps, followUpsDirty = False }, Nothing )


addFollowUpQuestion : Seed -> Path -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
addFollowUpQuestion seed path (AnswerEditor ae) =
    let
        ( newSeed, newFollowUps, event ) =
            addChild
                seed
                ae.followUps
                createQuestionEditor
                newQuestion
                (flip createAddQuestionEvent path)
    in
    ( newSeed, AnswerEditor { ae | followUps = newFollowUps }, Just event )


viewFollowUpQuestion : String -> Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
viewFollowUpQuestion uuid seed (AnswerEditor ae) =
    let
        newFollowUps =
            updateInList ae.followUps (matchQuestion uuid) activateQuestion
    in
    ( seed, AnswerEditor { ae | followUps = newFollowUps }, Nothing )


deleteFollowUpQuestion : String -> Seed -> Path -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
deleteFollowUpQuestion uuid seed path (AnswerEditor ae) =
    let
        newFollowUps =
            List.filter (not << matchQuestion uuid) ae.followUps

        ( event, newSeed ) =
            createDeleteQuestionEvent uuid path seed
    in
    ( newSeed, AnswerEditor { ae | followUps = newFollowUps }, Just event )
