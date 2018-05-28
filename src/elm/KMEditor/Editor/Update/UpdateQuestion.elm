module KMEditor.Editor.Update.UpdateQuestion exposing (..)

import Form
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (..)
import KMEditor.Editor.Update.Utils exposing (addChild, updateInList)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
formMsg formMsg seed path ((QuestionEditor qe) as editor) =
    case ( formMsg, Form.getOutput qe.form, isQuestionEditorDirty editor ) of
        ( Form.Submit, Just questionForm, True ) ->
            let
                newQuestion =
                    updateQuestionWithForm qe.question questionForm

                newForm =
                    initQuestionForm newQuestion

                newAnswerItemTemplateQuestions =
                    List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) qe.answerItemTemplateQuestions

                newAnswers =
                    List.indexedMap (\i (AnswerEditor ae) -> AnswerEditor { ae | order = i }) qe.answers

                newReferences =
                    List.indexedMap (\i (ReferenceEditor re) -> ReferenceEditor { re | order = i }) qe.references

                newExperts =
                    List.indexedMap (\i (ExpertEditor ee) -> ExpertEditor { ee | order = i }) qe.experts

                newEditor =
                    QuestionEditor
                        { qe
                            | active = False
                            , form = newForm
                            , question = newQuestion
                            , answerItemTemplateQuestions = newAnswerItemTemplateQuestions
                            , answerItemTemplateQuestionsDirty = False
                            , answers = newAnswers
                            , answersDirty = False
                            , references = newReferences
                            , referencesDirty = False
                            , experts = newExperts
                            , expertsDirty = False
                        }

                ( event, newSeed ) =
                    createEditQuestionEvent newEditor path seed
            in
            ( newSeed, newEditor, Just event )

        ( Form.Submit, Just questionForm, False ) ->
            ( seed, QuestionEditor { qe | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update questionFormValidation formMsg qe.form
            in
            ( seed, QuestionEditor { qe | form = newForm }, Nothing )


cancel : Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
cancel seed (QuestionEditor qe) =
    let
        newForm =
            initQuestionForm qe.question

        newAnswers =
            List.sortBy (\(AnswerEditor ae) -> ae.order) qe.answers

        newReferences =
            List.sortBy (\(ReferenceEditor re) -> re.order) qe.references

        newExperts =
            List.sortBy (\(ExpertEditor ee) -> ee.order) qe.experts
    in
    ( seed
    , QuestionEditor
        { qe
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


addAnswerItemTemplateQuestion : Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addAnswerItemTemplateQuestion seed path (QuestionEditor qe) =
    let
        ( newSeed, newAnswerItemTepmlateQuestions, event ) =
            addChild
                seed
                qe.answerItemTemplateQuestions
                createQuestionEditor
                newQuestion
                (flip createAddQuestionEvent path)
    in
    ( newSeed, QuestionEditor { qe | answerItemTemplateQuestions = newAnswerItemTepmlateQuestions }, Just event )


viewAnswerItemTemplateQuestion : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewAnswerItemTemplateQuestion uuid seed (QuestionEditor qe) =
    let
        newAnswerItemTemplateQuestions =
            updateInList qe.answerItemTemplateQuestions (matchQuestion uuid) activateQuestion
    in
    ( seed, QuestionEditor { qe | answerItemTemplateQuestions = newAnswerItemTemplateQuestions }, Nothing )


deleteAnswerItemTemplateQuestion : String -> Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteAnswerItemTemplateQuestion uuid seed path (QuestionEditor qe) =
    let
        newAnswerItemTemplateQuestions =
            List.filter (not << matchQuestion uuid) qe.answerItemTemplateQuestions

        ( event, newSeed ) =
            createDeleteQuestionEvent uuid path seed
    in
    ( newSeed, QuestionEditor { qe | answerItemTemplateQuestions = newAnswerItemTemplateQuestions }, Just event )


addAnswer : Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addAnswer seed path (QuestionEditor qe) =
    let
        ( newSeed, newAnswers, event ) =
            addChild
                seed
                qe.answers
                createAnswerEditor
                newAnswer
                (flip createAddAnswerEvent path)
    in
    ( newSeed, QuestionEditor { qe | answers = newAnswers }, Just event )


viewAnswer : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewAnswer uuid seed (QuestionEditor qe) =
    let
        newAnswers =
            updateInList qe.answers (matchAnswer uuid) activateAnswer
    in
    ( seed, QuestionEditor { qe | answers = newAnswers }, Nothing )


deleteAnswer : String -> Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteAnswer uuid seed path (QuestionEditor qe) =
    let
        newAnswers =
            List.filter (not << matchAnswer uuid) qe.answers

        ( event, newSeed ) =
            createDeleteAnswerEvent uuid path seed
    in
    ( newSeed, QuestionEditor { qe | answers = newAnswers }, Just event )


addReference : Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addReference seed path (QuestionEditor qe) =
    let
        ( newSeed, newReferences, event ) =
            addChild
                seed
                qe.references
                createReferenceEditor
                newReference
                (flip createAddReferenceEvent path)
    in
    ( newSeed, QuestionEditor { qe | references = newReferences }, Just event )


viewReference : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewReference uuid seed (QuestionEditor qe) =
    let
        newReferences =
            updateInList qe.references (matchReference uuid) activateReference
    in
    ( seed, QuestionEditor { qe | references = newReferences }, Nothing )


deleteReference : String -> Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteReference uuid seed path (QuestionEditor qe) =
    let
        newReferences =
            List.filter (not << matchReference uuid) qe.references

        ( event, newSeed ) =
            createDeleteReferenceEvent uuid path seed
    in
    ( newSeed, QuestionEditor { qe | references = newReferences }, Just event )


addExpert : Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addExpert seed path (QuestionEditor qe) =
    let
        ( newSeed, newExperts, event ) =
            addChild
                seed
                qe.experts
                createExpertEditor
                newExpert
                (flip createAddExpertEvent path)
    in
    ( newSeed, QuestionEditor { qe | experts = newExperts }, Just event )


viewExpert : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewExpert uuid seed (QuestionEditor qe) =
    let
        newExperts =
            updateInList qe.experts (matchExpert uuid) activateExpert
    in
    ( seed, QuestionEditor { qe | experts = newExperts }, Nothing )


deleteExpert : String -> Seed -> Path -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteExpert uuid seed path (QuestionEditor qe) =
    let
        newExperts =
            List.filter (not << matchExpert uuid) qe.experts

        ( event, newSeed ) =
            createDeleteExpertEvent uuid path seed
    in
    ( newSeed, QuestionEditor { qe | experts = newExperts }, Just event )
