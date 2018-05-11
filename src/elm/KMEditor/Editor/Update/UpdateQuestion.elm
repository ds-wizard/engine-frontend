module KMEditor.Editor.Update.UpdateQuestion exposing (..)

import Form
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Entities exposing (..)
import KMEditor.Editor.Models.Events exposing (..)
import KMEditor.Editor.Models.Forms exposing (..)
import KMEditor.Editor.Update.Utils exposing (addChild, formChanged, updateInList)
import Random.Pcg exposing (Seed)


formMsg : Form.Msg -> Seed -> (Chapter -> KnowledgeModel -> Seed -> QuestionEditor -> ( Event, Seed )) -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
formMsg formMsg seed createEditEvent chapter knowledgeModel ((QuestionEditor editor) as questionEditor) =
    case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.answerItemTemplateQuestionsDirty || editor.answersDirty || editor.referencesDirty || editor.expertsDirty ) of
        ( Form.Submit, Just questionForm, True ) ->
            let
                newQuestion =
                    updateQuestionWithForm editor.question questionForm

                newForm =
                    initQuestionForm newQuestion

                newAnswerItemTemplateQuestions =
                    List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) editor.answerItemTemplateQuestions

                newAnswers =
                    List.indexedMap (\i (AnswerEditor ae) -> AnswerEditor { ae | order = i }) editor.answers

                newReferences =
                    List.indexedMap (\i (ReferenceEditor re) -> ReferenceEditor { re | order = i }) editor.references

                newExperts =
                    List.indexedMap (\i (ExpertEditor ee) -> ExpertEditor { ee | order = i }) editor.experts

                newEditor =
                    QuestionEditor
                        { editor
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
                    createEditEvent chapter knowledgeModel seed newEditor
            in
            ( newSeed, newEditor, Just event )

        ( Form.Submit, Just questionForm, False ) ->
            ( seed, QuestionEditor { editor | active = False }, Nothing )

        _ ->
            let
                newForm =
                    Form.update questionFormValidation formMsg editor.form
            in
            ( seed, QuestionEditor { editor | form = newForm }, Nothing )


cancel : Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
cancel seed (QuestionEditor editor) =
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


addAnswerItemTemplateQuestion : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addAnswerItemTemplateQuestion seed chapter knowledgeModel (QuestionEditor editor) =
    let
        ( newSeed, newAnswerItemTepmlateQuestions, event ) =
            addChild
                seed
                editor.answerItemTemplateQuestions
                createQuestionEditor
                newQuestion
                (createAddAnswerItemTemplateQuestionEvent editor.question chapter knowledgeModel)
    in
    ( newSeed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTepmlateQuestions }, Just event )


viewAnswerItemTemplateQuestion : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewAnswerItemTemplateQuestion uuid seed (QuestionEditor editor) =
    let
        newAnswerItemTemplateQuestions =
            updateInList editor.answerItemTemplateQuestions (matchQuestion uuid) activateQuestion
    in
    ( seed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTemplateQuestions }, Nothing )


deleteAnswerItemTemplateQuestion : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteAnswerItemTemplateQuestion uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newAnswerItemTemplateQuestions =
            List.filter (not << matchQuestion uuid) editor.answerItemTemplateQuestions

        ( event, newSeed ) =
            createDeleteAnswerItemTemplateQuestionEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTemplateQuestions }, Just event )


addAnswer : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addAnswer seed chapter knowledgeModel (QuestionEditor editor) =
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


viewAnswer : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewAnswer uuid seed (QuestionEditor editor) =
    let
        newAnswers =
            updateInList editor.answers (matchAnswer uuid) activateAnswer
    in
    ( seed, QuestionEditor { editor | answers = newAnswers }, Nothing )


deleteAnswer : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteAnswer uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newAnswers =
            List.filter (not << matchAnswer uuid) editor.answers

        ( event, newSeed ) =
            createDeleteAnswerEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )


addReference : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addReference seed chapter knowledgeModel (QuestionEditor editor) =
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


viewReference : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewReference uuid seed (QuestionEditor editor) =
    let
        newReferences =
            updateInList editor.references (matchReference uuid) activateReference
    in
    ( seed, QuestionEditor { editor | references = newReferences }, Nothing )


deleteReference : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteReference uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newReferences =
            List.filter (not << matchReference uuid) editor.references

        ( event, newSeed ) =
            createDeleteReferenceEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )


addExpert : Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
addExpert seed chapter knowledgeModel (QuestionEditor editor) =
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


viewExpert : String -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
viewExpert uuid seed (QuestionEditor editor) =
    let
        newExperts =
            updateInList editor.experts (matchExpert uuid) activateExpert
    in
    ( seed, QuestionEditor { editor | experts = newExperts }, Nothing )


deleteExpert : String -> Seed -> Chapter -> KnowledgeModel -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
deleteExpert uuid seed chapter knowledgeModel (QuestionEditor editor) =
    let
        newExperts =
            List.filter (not << matchExpert uuid) editor.experts

        ( event, newSeed ) =
            createDeleteExpertEvent editor.question chapter knowledgeModel seed uuid
    in
    ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )
