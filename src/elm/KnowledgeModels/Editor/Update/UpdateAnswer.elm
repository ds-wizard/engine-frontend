module KnowledgeModels.Editor.Update.UpdateAnswer exposing (..)

{-|

@docs updateAnswerFormMsg, updateAnswerCancel
@docs updateAnswerAddFollowUpQuestion, updateAnswerViewFollowUpQuestion, updateAnswerDeleteFollowUpQuestion

-}

import Form
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Events exposing (Event, createAddFollowUpQuestionEvent, createDeleteFollowUpQuestionEvent, createEditAnswerEvent)
import KnowledgeModels.Editor.Models.Forms exposing (answerFormValidation, initAnswerForm, updateAnswerWithForm)
import KnowledgeModels.Editor.Update.Utils exposing (addChild, formChanged, updateInList)
import List.Extra as List
import Random.Pcg exposing (Seed)


{-| -}
updateAnswerFormMsg : Form.Msg -> Seed -> Question -> Chapter -> KnowledgeModel -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswerFormMsg formMsg seed question chapter knowledgeModel ((AnswerEditor editor) as originalEditor) =
    case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.followUpsDirty ) of
        ( Form.Submit, Just answerForm, True ) ->
            let
                newAnswer =
                    updateAnswerWithForm editor.answer answerForm

                newForm =
                    initAnswerForm newAnswer

                followUpsIds =
                    List.map (\(QuestionEditor qe) -> qe.question.uuid) editor.followUps

                newFollowUps =
                    List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) editor.followUps

                newEditor =
                    { editor | active = False, form = newForm, answer = newAnswer, followUps = newFollowUps, followUpsDirty = False }

                ( event, newSeed ) =
                    createEditAnswerEvent question chapter knowledgeModel followUpsIds seed newAnswer
            in
            ( newSeed, AnswerEditor { editor | active = False }, Just event )

        ( Form.Submit, Just answerForm, False ) ->
            ( seed, AnswerEditor { editor | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update answerFormValidation formMsg editor.form
            in
            ( seed, AnswerEditor { editor | form = newForm }, Nothing )


{-| -}
updateAnswerCancel : Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswerCancel seed (AnswerEditor editor) =
    let
        newForm =
            initAnswerForm editor.answer

        newFollowUps =
            List.sortBy (\(QuestionEditor qe) -> qe.order) editor.followUps
    in
    ( seed, AnswerEditor { editor | active = False, form = newForm, followUps = newFollowUps, followUpsDirty = False }, Nothing )


{-| -}
updateAnswerAddFollowUpQuestion : Seed -> Chapter -> KnowledgeModel -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswerAddFollowUpQuestion seed chapter knowledgeModel (AnswerEditor editor) =
    let
        ( newSeed, newFollowUps, event ) =
            addChild
                seed
                editor.followUps
                createQuestionEditor
                newQuestion
                (createAddFollowUpQuestionEvent editor.answer chapter knowledgeModel)
    in
    ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, Just event )


{-| -}
updateAnswerViewFollowUpQuestion : String -> Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswerViewFollowUpQuestion uuid seed (AnswerEditor editor) =
    let
        newFollowUps =
            updateInList editor.followUps (matchQuestion uuid) activateQuestion
    in
    ( seed, AnswerEditor { editor | followUps = newFollowUps }, Nothing )


{-| -}
updateAnswerDeleteFollowUpQuestion : String -> Seed -> Chapter -> KnowledgeModel -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswerDeleteFollowUpQuestion uuid seed chapter knowledgeModel (AnswerEditor editor) =
    let
        newFollowUps =
            List.removeWhen (matchQuestion uuid) editor.followUps

        ( event, newSeed ) =
            createDeleteFollowUpQuestionEvent editor.answer chapter knowledgeModel seed uuid
    in
    ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, Just event )
