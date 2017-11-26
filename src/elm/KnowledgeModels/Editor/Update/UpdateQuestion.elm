module KnowledgeModels.Editor.Update.UpdateQuestion exposing (..)

import Form
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Events exposing (..)
import KnowledgeModels.Editor.Models.Forms exposing (..)
import KnowledgeModels.Editor.Update.Utils exposing (addChild, formChanged, updateInList)
import List.Extra as List
import Random.Pcg exposing (Seed)


updateQuestionFormMsg : Form.Msg -> Seed -> (Chapter -> KnowledgeModel -> List String -> List String -> List String -> Seed -> Question -> ( Event, Seed )) -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionFormMsg formMsg seed createEditEvent chapter knowledgeModel ((QuestionEditor editor) as questionEditor) =
    case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.answersDirty || editor.referencesDirty || editor.expertsDirty ) of
        ( Form.Submit, Just questionForm, True ) ->
            let
                newQuestion =
                    updateQuestionWithForm editor.question questionForm

                newForm =
                    initQuestionForm newQuestion

                answerIds =
                    List.map (\(AnswerEditor ae) -> ae.answer.uuid) editor.answers

                newAnswers =
                    List.indexedMap (\i (AnswerEditor ae) -> AnswerEditor { ae | order = i }) editor.answers

                referenceIds =
                    List.map (\(ReferenceEditor re) -> re.reference.uuid) editor.references

                newReferences =
                    List.indexedMap (\i (ReferenceEditor re) -> ReferenceEditor { re | order = i }) editor.references

                expertIds =
                    List.map (\(ExpertEditor ee) -> ee.expert.uuid) editor.experts

                newExperts =
                    List.indexedMap (\i (ExpertEditor ee) -> ExpertEditor { ee | order = i }) editor.experts

                newEditor =
                    { editor
                        | active = False
                        , form = newForm
                        , question = newQuestion
                        , answers = newAnswers
                        , answersDirty = False
                        , references = newReferences
                        , referencesDirty = False
                        , experts = newExperts
                        , expertsDirty = False
                    }

                ( event, newSeed ) =
                    createEditEvent chapter knowledgeModel answerIds referenceIds expertIds seed newQuestion
            in
            ( newSeed, QuestionEditor { editor | active = False }, Just event )

        ( Form.Submit, Just questionForm, False ) ->
            ( seed, QuestionEditor { editor | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update questionFormValidation formMsg editor.form
            in
            ( seed, QuestionEditor { editor | form = newForm }, Nothing )


updateQuestionCancel : Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionCancel seed (QuestionEditor editor) =
    let
        newForm =
            initQuestionForm editor.question

        newAnswers =
            List.sortBy (\(AnswerEditor ae) -> ae.order) editor.answers

        newReferences =
            List.sortBy (\(ReferenceEditor re) -> re.order) editor.references

        newExperts =
            List.sortBy (\(ExpertEditor ee) -> ee.order) editor.experts
    in
    ( seed
    , QuestionEditor
        { editor
            | active = False
            , form = newForm
            , answers = newAnswers
            , answersDirty = False
            , references = newReferences
            , referencesDirty = False
            , experts = newExperts
            , expertsDirty = False
        }
    , Nothing
    )


updateQuestionAddAnswer : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionAddAnswer seed chapter knowledgeModel (QuestionEditor editor) =
    let
        ( newSeed, newAnswers, event ) =
            addChild
                seed
                editor.answers
                createAnswerEditor
                newAnswer
                (createAddAnswerEvent editor.question chapter knowledgeModel)
    in
    ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )


updateQuestionViewAnswer : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionViewAnswer uuid seed (QuestionEditor editor) =
    let
        newAnswers =
            updateInList editor.answers (matchAnswer uuid) activateAnswer
    in
    ( seed, QuestionEditor { editor | answers = newAnswers }, Nothing )


updateQuestionDeleteAnswer : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionDeleteAnswer uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newAnswers =
            List.removeWhen (matchAnswer uuid) editor.answers

        ( event, newSeed ) =
            createDeleteAnswerEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )


updateQuestionAddReference : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionAddReference seed chapter knowledgeModel (QuestionEditor editor) =
    let
        ( newSeed, newReferences, event ) =
            addChild
                seed
                editor.references
                createReferenceEditor
                newReference
                (createAddReferenceEvent editor.question chapter knowledgeModel)
    in
    ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )


updateQuestionViewReference : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionViewReference uuid seed (QuestionEditor editor) =
    let
        newReferences =
            updateInList editor.references (matchReference uuid) activateReference
    in
    ( seed, QuestionEditor { editor | references = newReferences }, Nothing )


updateQuestionDeleteReference : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionDeleteReference uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newReferences =
            List.removeWhen (matchReference uuid) editor.references

        ( event, newSeed ) =
            createDeleteReferenceEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )


updateQuestionAddExpert : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionAddExpert seed chapter knowledgeModel (QuestionEditor editor) =
    let
        ( newSeed, newExperts, event ) =
            addChild
                seed
                editor.experts
                createExpertEditor
                newExpert
                (createAddExpertEvent editor.question chapter knowledgeModel)
    in
    ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )


updateQuestionViewExpert : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionViewExpert uuid seed (QuestionEditor editor) =
    let
        newExperts =
            updateInList editor.experts (matchExpert uuid) activateExpert
    in
    ( seed, QuestionEditor { editor | experts = newExperts }, Nothing )


updateQuestionDeleteExpert : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestionDeleteExpert uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newExperts =
            List.removeWhen (matchExpert uuid) editor.experts

        ( event, newSeed ) =
            createDeleteExpertEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )
