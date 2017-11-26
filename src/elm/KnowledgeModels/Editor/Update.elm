module KnowledgeModels.Editor.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Jwt
import KnowledgeModels.Editor.Models exposing (..)
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Models.Entities exposing (..)
import KnowledgeModels.Editor.Models.Events exposing (..)
import KnowledgeModels.Editor.Models.Forms exposing (..)
import KnowledgeModels.Editor.Msgs exposing (..)
import KnowledgeModels.Requests exposing (getKnowledgeModelData, postEventsBulk)
import List.Extra as List
import Msgs
import Random.Pcg exposing (Seed)
import Reorderable
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import Set
import Utils exposing (getUuid, tuplePrepend)


getKnowledgeModelCmd : String -> Session -> Cmd Msgs.Msg
getKnowledgeModelCmd uuid session =
    getKnowledgeModelData uuid session
        |> toCmd GetKnowledgeModelCompleted Msgs.KnowledgeModelsEditorMsg


getKnowledgeModelCompleted : Model -> Result Jwt.JwtError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelEditor = Success <| createKnowledgeModelEditor knowledgeModel }

                Err error ->
                    { model | knowledgeModelEditor = Error "Unable to get knowledge model" }
    in
    ( newModel, Cmd.none )


postEventsBulkCmd : String -> List Event -> Session -> Cmd Msgs.Msg
postEventsBulkCmd uuid events session =
    encodeEvents events
        |> postEventsBulk session uuid
        |> toCmd SaveCompleted Msgs.KnowledgeModelsEditorMsg


postEventsBulkCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postEventsBulkCompleted model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate KnowledgeModels )

        Err error ->
            ( { model | saving = Error "Knowledge model could not be saved" }, Cmd.none )


updateEdit : KnowledgeModelMsg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
updateEdit msg seed session model =
    case model.knowledgeModelEditor of
        Success knowledgeModelEditor ->
            let
                ( newSeed, newKnowledgeModelEditor, maybeEvent, submit ) =
                    updateKnowledgeModel msg seed knowledgeModelEditor

                newEvents =
                    case maybeEvent of
                        Just event ->
                            model.events ++ [ event ]

                        Nothing ->
                            model.events

                newReorderableState =
                    Reorderable.update (Reorderable.MouseOverIgnored False) model.reorderableState

                ( newModel, cmd ) =
                    if submit then
                        ( { model | saving = Loading }, postEventsBulkCmd model.branchUuid newEvents session )
                    else
                        ( model, Cmd.none )
            in
            ( newSeed
            , { newModel
                | knowledgeModelEditor = Success newKnowledgeModelEditor
                , events = newEvents
                , reorderableState = newReorderableState
              }
            , cmd
            )

        _ ->
            ( seed, model, Cmd.none )


updateKnowledgeModel : KnowledgeModelMsg -> Seed -> KnowledgeModelEditor -> ( Seed, KnowledgeModelEditor, Maybe Event, Bool )
updateKnowledgeModel msg seed ((KnowledgeModelEditor editor) as originalEditor) =
    case msg of
        KnowledgeModelFormMsg formMsg ->
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

        AddChapter ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newChapters =
                    createChapterEditor True (List.length editor.chapters) (newChapter newUuid)
                        |> List.singleton
                        |> List.append editor.chapters

                ( event, newSeed ) =
                    createAddChapterEvent editor.knowledgeModel seed2 (newChapter newUuid)
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, Just event, False )

        ViewChapter uuid ->
            let
                newChapters =
                    updateInList editor.chapters (matchChapter uuid) activateChapter
            in
            ( seed, KnowledgeModelEditor { editor | chapters = newChapters }, Nothing, False )

        DeleteChapter uuid ->
            let
                newChapters =
                    List.removeWhen (matchChapter uuid) editor.chapters

                ( event, newSeed ) =
                    createDeleteChapterEvent editor.knowledgeModel seed uuid
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, Just event, False )

        ReorderChapterList newChapters ->
            ( seed, KnowledgeModelEditor { editor | chapters = newChapters, chaptersDirty = True }, Nothing, False )

        ChapterMsg uuid chapterMsg ->
            let
                ( newSeed, newChapters, event ) =
                    updateInListWithSeed editor.chapters seed (matchChapter uuid) (updateChapter editor.knowledgeModel chapterMsg)
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, event, False )


updateChapter : KnowledgeModel -> ChapterMsg -> Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapter knowledgeModel msg seed ((ChapterEditor editor) as originalEditor) =
    case msg of
        ChapterFormMsg formMsg ->
            case ( formMsg, Form.getOutput editor.form, formChanged editor.form || editor.questionsDirty ) of
                ( Form.Submit, Just chapterForm, True ) ->
                    let
                        -- Create new chapter based on the form fields
                        newChapter =
                            updateChapterWithForm editor.chapter chapterForm

                        -- Initialize new form with new chapter values
                        newForm =
                            initChapterForm newChapter

                        -- Get the question ids in correct order
                        questionIds =
                            List.map (\(QuestionEditor e) -> e.question.uuid) editor.questions

                        -- Set the new order for the questions
                        newQuestions =
                            List.indexedMap (\i (QuestionEditor qe) -> QuestionEditor { qe | order = i }) editor.questions

                        -- Create new editor structure
                        newEditor =
                            { editor | active = False, form = newForm, chapter = newChapter, questions = newQuestions, questionsDirty = False }

                        -- Create the new event
                        ( event, newSeed ) =
                            createEditChapterEvent knowledgeModel questionIds seed newChapter
                    in
                    ( newSeed, ChapterEditor newEditor, Just event )

                ( Form.Submit, Just chapterForm, False ) ->
                    ( seed, ChapterEditor { editor | active = False }, Nothing )

                _ ->
                    let
                        newForm =
                            Form.update chapterFormValidation formMsg editor.form
                    in
                    ( seed, ChapterEditor { editor | form = newForm }, Nothing )

        ChapterCancel ->
            let
                -- Reinitialize the form with the original chapter values
                newForm =
                    initChapterForm editor.chapter

                -- Reset the order of the questions to the original one
                newQuestions =
                    List.sortBy (\(QuestionEditor qe) -> qe.order) editor.questions
            in
            ( seed, ChapterEditor { editor | active = False, form = newForm, questions = newQuestions }, Nothing )

        AddChapterQuestion ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newQuestions =
                    createQuestionEditor True (List.length editor.questions) (newQuestion newUuid)
                        |> List.singleton
                        |> List.append editor.questions

                ( event, newSeed ) =
                    createAddQuestionEvent editor.chapter knowledgeModel seed2 (newQuestion newUuid)
            in
            ( newSeed, ChapterEditor { editor | questions = newQuestions }, Just event )

        ViewQuestion uuid ->
            let
                newQuestions =
                    updateInList editor.questions (matchQuestion uuid) activateQuestion
            in
            ( seed, ChapterEditor { editor | questions = newQuestions }, Nothing )

        DeleteChapterQuestion uuid ->
            let
                newQuestions =
                    List.removeWhen (matchQuestion uuid) editor.questions

                ( event, newSeed ) =
                    createDeleteQuestionEvent editor.chapter knowledgeModel seed uuid
            in
            ( newSeed, ChapterEditor { editor | questions = newQuestions }, Just event )

        ReorderQuestionList newQuestions ->
            ( seed, ChapterEditor { editor | questions = newQuestions, questionsDirty = True }, Nothing )

        ChapterQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newQuestions, event ) =
                    updateInListWithSeed editor.questions seed (matchQuestion uuid) (updateQuestion editor.chapter knowledgeModel questionMsg)
            in
            ( newSeed, ChapterEditor { editor | questions = newQuestions }, event )


updateQuestion : Chapter -> KnowledgeModel -> QuestionMsg -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestion chapter knowledgeModel msg seed ((QuestionEditor editor) as originalEditor) =
    case msg of
        QuestionFormMsg formMsg ->
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
                            createEditQuestionEvent chapter knowledgeModel answerIds referenceIds expertIds seed newQuestion
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

        QuestionCancel ->
            let
                newForm =
                    initQuestionForm editor.question

                newExperts =
                    List.sortBy (\(ExpertEditor ae) -> ae.order) editor.experts
            in
            ( seed, QuestionEditor { editor | active = False, form = newForm, experts = newExperts }, Nothing )

        AddAnswer ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newAnswers =
                    createAnswerEditor True (List.length editor.answers) (newAnswer newUuid)
                        |> List.singleton
                        |> List.append editor.answers

                ( event, newSeed ) =
                    createAddAnswerEvent editor.question chapter knowledgeModel seed2 (newAnswer newUuid)
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )

        ViewAnswer uuid ->
            let
                newAnswers =
                    updateInList editor.answers (matchAnswer uuid) activateAnswer
            in
            ( seed, QuestionEditor { editor | answers = newAnswers }, Nothing )

        DeleteAnswer uuid ->
            let
                newAnswers =
                    List.removeWhen (matchAnswer uuid) editor.answers

                ( event, newSeed ) =
                    createDeleteAnswerEvent editor.question chapter knowledgeModel seed uuid
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )

        ReorderAnswerList newAnswers ->
            ( seed, QuestionEditor { editor | answers = newAnswers, answersDirty = True }, Nothing )

        AnswerMsg uuid answerMsg ->
            let
                ( newSeed, newAnswers, event ) =
                    updateInListWithSeed editor.answers seed (matchAnswer uuid) (updateAnswer editor.question chapter knowledgeModel answerMsg)
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, event )

        AddReference ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newReferences =
                    createReferenceEditor True (List.length editor.references) (newReference newUuid)
                        |> List.singleton
                        |> List.append editor.references

                ( event, newSeed ) =
                    createAddReferenceEvent editor.question chapter knowledgeModel seed2 (newReference newUuid)
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )

        ViewReference uuid ->
            let
                newReferences =
                    updateInList editor.references (matchReference uuid) activateReference
            in
            ( seed, QuestionEditor { editor | references = newReferences }, Nothing )

        DeleteReference uuid ->
            let
                newReferences =
                    List.removeWhen (matchReference uuid) editor.references

                ( event, newSeed ) =
                    createDeleteReferenceEvent editor.question chapter knowledgeModel seed uuid
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )

        ReorderReferenceList newReferences ->
            ( seed, QuestionEditor { editor | references = newReferences, referencesDirty = True }, Nothing )

        ReferenceMsg uuid referenceMsg ->
            let
                ( newSeed, newReferences, event ) =
                    updateInListWithSeed editor.references seed (matchReference uuid) (updateReference editor.question chapter knowledgeModel referenceMsg)
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, event )

        AddExpert ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newExperts =
                    createExpertEditor True (List.length editor.experts) (newExpert newUuid)
                        |> List.singleton
                        |> List.append editor.experts

                ( event, newSeed ) =
                    createAddExpertEvent editor.question chapter knowledgeModel seed2 (newExpert newUuid)
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )

        ViewExpert uuid ->
            let
                newExperts =
                    updateInList editor.experts (matchExpert uuid) activateExpert
            in
            ( seed, QuestionEditor { editor | experts = newExperts }, Nothing )

        DeleteExpert uuid ->
            let
                newExperts =
                    List.removeWhen (matchExpert uuid) editor.experts

                ( event, newSeed ) =
                    createDeleteExpertEvent editor.question chapter knowledgeModel seed uuid
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )

        ReorderExpertList newExperts ->
            ( seed, QuestionEditor { editor | experts = newExperts, expertsDirty = True }, Nothing )

        ExpertMsg uuid expertMsg ->
            let
                ( newSeed, newExperts, event ) =
                    updateInListWithSeed editor.experts seed (matchExpert uuid) (updateExpert editor.question chapter knowledgeModel expertMsg)
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, event )


updateAnswer : Question -> Chapter -> KnowledgeModel -> AnswerMsg -> Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswer question chapter knowledgeModel msg seed ((AnswerEditor editor) as originalEditor) =
    case msg of
        AnswerFormMsg formMsg ->
            case ( formMsg, Form.getOutput editor.form, formChanged editor.form ) of
                ( Form.Submit, Just answerForm, True ) ->
                    let
                        newAnswer =
                            updateAnswerWithForm editor.answer answerForm

                        newForm =
                            initAnswerForm newAnswer

                        newEditor =
                            { editor | active = False, form = newForm, answer = newAnswer }

                        ( event, newSeed ) =
                            createEditAnswerEvent question chapter knowledgeModel [] seed newAnswer
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

        AnswerCancel ->
            let
                newForm =
                    initAnswerForm editor.answer
            in
            ( seed, AnswerEditor { editor | active = False, form = newForm }, Nothing )

        AddFollowUpQuestion ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newFollowUps =
                    createQuestionEditor True (List.length editor.followUps) (newQuestion newUuid)
                        |> List.singleton
                        |> List.append editor.followUps

                ( event, newSeed ) =
                    createAddFollowUpQuestionEvent editor.answer chapter knowledgeModel seed2 (newQuestion newUuid)
            in
            ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, Just event )

        ViewFollowUpQuestion uuid ->
            let
                newFollowUps =
                    updateInList editor.followUps (matchQuestion uuid) activateQuestion
            in
            ( seed, AnswerEditor { editor | followUps = newFollowUps }, Nothing )

        DeleteFollowUpQuestion uuid ->
            let
                newFollowUps =
                    List.removeWhen (matchQuestion uuid) editor.followUps

                ( event, newSeed ) =
                    createDeleteFollowUpQuestionEvent editor.answer chapter knowledgeModel seed uuid
            in
            ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, Just event )

        ReorderFollowUpQuestionList newFollowUps ->
            ( seed, AnswerEditor { editor | followUps = newFollowUps, followUpsDirty = True }, Nothing )

        FollowUpQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newFollowUps, event ) =
                    updateInListWithSeed editor.followUps seed (matchQuestion uuid) (updateFollowUpQuestion editor.answer chapter knowledgeModel questionMsg)
            in
            ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, event )


updateReference : Question -> Chapter -> KnowledgeModel -> ReferenceMsg -> Seed -> ReferenceEditor -> ( Seed, ReferenceEditor, Maybe Event )
updateReference question chapter knowledgeModel msg seed ((ReferenceEditor editor) as originalEditor) =
    case msg of
        ReferenceFormMsg formMsg ->
            case ( formMsg, Form.getOutput editor.form, formChanged editor.form ) of
                ( Form.Submit, Just referenceForm, True ) ->
                    let
                        newReference =
                            updateReferenceWithForm editor.reference referenceForm

                        newForm =
                            initReferenceForm newReference

                        newEditor =
                            { editor | active = False, form = newForm, reference = newReference }

                        ( event, newSeed ) =
                            createEditReferenceEvent question chapter knowledgeModel [] seed newReference
                    in
                    ( newSeed, ReferenceEditor { editor | active = False }, Just event )

                ( Form.Submit, Just referenceForm, False ) ->
                    ( seed, ReferenceEditor { editor | active = False }, Nothing )

                _ ->
                    let
                        newForm =
                            Form.update referenceFormValidation formMsg editor.form
                    in
                    ( seed, ReferenceEditor { editor | form = newForm }, Nothing )

        ReferenceCancel ->
            let
                newForm =
                    initReferenceForm editor.reference
            in
            ( seed, ReferenceEditor { editor | active = False, form = newForm }, Nothing )


updateExpert : Question -> Chapter -> KnowledgeModel -> ExpertMsg -> Seed -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
updateExpert question chapter knowledgeModel msg seed ((ExpertEditor editor) as originalEditor) =
    case msg of
        ExpertFormMsg formMsg ->
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

        ExpertCancel ->
            let
                newForm =
                    initExpertForm editor.expert
            in
            ( seed, ExpertEditor { editor | active = False, form = newForm }, Nothing )


updateFollowUpQuestion : Answer -> Chapter -> KnowledgeModel -> QuestionMsg -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateFollowUpQuestion answer chapter knowledgeModel msg seed ((QuestionEditor editor) as originalEditor) =
    case msg of
        QuestionFormMsg formMsg ->
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
                            createEditFollowUpQuestionEvent answer chapter knowledgeModel answerIds referenceIds expertIds seed newQuestion
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

        QuestionCancel ->
            let
                newForm =
                    initQuestionForm editor.question

                newExperts =
                    List.sortBy (\(ExpertEditor ae) -> ae.order) editor.experts
            in
            ( seed, QuestionEditor { editor | active = False, form = newForm, experts = newExperts }, Nothing )

        AddAnswer ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newAnswers =
                    createAnswerEditor True (List.length editor.answers) (newAnswer newUuid)
                        |> List.singleton
                        |> List.append editor.answers

                ( event, newSeed ) =
                    createAddAnswerEvent editor.question chapter knowledgeModel seed2 (newAnswer newUuid)
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )

        ViewAnswer uuid ->
            let
                newAnswers =
                    updateInList editor.answers (matchAnswer uuid) activateAnswer
            in
            ( seed, QuestionEditor { editor | answers = newAnswers }, Nothing )

        DeleteAnswer uuid ->
            let
                newAnswers =
                    List.removeWhen (matchAnswer uuid) editor.answers

                ( event, newSeed ) =
                    createDeleteAnswerEvent editor.question chapter knowledgeModel seed uuid
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, Just event )

        ReorderAnswerList newAnswers ->
            ( seed, QuestionEditor { editor | answers = newAnswers, answersDirty = True }, Nothing )

        AnswerMsg uuid answerMsg ->
            let
                ( newSeed, newAnswers, event ) =
                    updateInListWithSeed editor.answers seed (matchAnswer uuid) (updateAnswer editor.question chapter knowledgeModel answerMsg)
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, event )

        AddReference ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newReferences =
                    createReferenceEditor True (List.length editor.references) (newReference newUuid)
                        |> List.singleton
                        |> List.append editor.references

                ( event, newSeed ) =
                    createAddReferenceEvent editor.question chapter knowledgeModel seed2 (newReference newUuid)
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )

        ViewReference uuid ->
            let
                newReferences =
                    updateInList editor.references (matchReference uuid) activateReference
            in
            ( seed, QuestionEditor { editor | references = newReferences }, Nothing )

        DeleteReference uuid ->
            let
                newReferences =
                    List.removeWhen (matchReference uuid) editor.references

                ( event, newSeed ) =
                    createDeleteReferenceEvent editor.question chapter knowledgeModel seed uuid
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, Just event )

        ReorderReferenceList newReferences ->
            ( seed, QuestionEditor { editor | references = newReferences, referencesDirty = True }, Nothing )

        ReferenceMsg uuid referenceMsg ->
            let
                ( newSeed, newReferences, event ) =
                    updateInListWithSeed editor.references seed (matchReference uuid) (updateReference editor.question chapter knowledgeModel referenceMsg)
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, event )

        AddExpert ->
            let
                ( newUuid, seed2 ) =
                    getUuid seed

                newExperts =
                    createExpertEditor True (List.length editor.experts) (newExpert newUuid)
                        |> List.singleton
                        |> List.append editor.experts

                ( event, newSeed ) =
                    createAddExpertEvent editor.question chapter knowledgeModel seed2 (newExpert newUuid)
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )

        ViewExpert uuid ->
            let
                newExperts =
                    updateInList editor.experts (matchExpert uuid) activateExpert
            in
            ( seed, QuestionEditor { editor | experts = newExperts }, Nothing )

        DeleteExpert uuid ->
            let
                newExperts =
                    List.removeWhen (matchExpert uuid) editor.experts

                ( event, newSeed ) =
                    createDeleteExpertEvent editor.question chapter knowledgeModel seed uuid
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, Just event )

        ReorderExpertList newExperts ->
            ( seed, QuestionEditor { editor | experts = newExperts, expertsDirty = True }, Nothing )

        ExpertMsg uuid expertMsg ->
            let
                ( newSeed, newExperts, event ) =
                    updateInListWithSeed editor.experts seed (matchExpert uuid) (updateExpert editor.question chapter knowledgeModel expertMsg)
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, event )


updateInListWithSeed : List t -> Seed -> (t -> Bool) -> (Seed -> t -> ( Seed, t, Maybe Event )) -> ( Seed, List t, Maybe Event )
updateInListWithSeed list seed predicate updateFunction =
    let
        fn =
            \item ( currentSeed, items, currentEvent ) ->
                if predicate item then
                    let
                        ( updatedSeed, updatedItem, event ) =
                            updateFunction seed item
                    in
                    ( updatedSeed, items ++ [ updatedItem ], event )
                else
                    ( currentSeed, items ++ [ item ], currentEvent )
    in
    List.foldl fn ( seed, [], Nothing ) list


updateInList : List a -> (a -> Bool) -> (a -> a) -> List a
updateInList list predicate updateFunction =
    let
        fn =
            \item ->
                if predicate item then
                    updateFunction item
                else
                    item
    in
    List.map fn list


formChanged : Form () a -> Bool
formChanged form =
    Set.size (Form.getChangedFields form) > 0


update : Msg -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg seed session model =
    case msg of
        GetKnowledgeModelCompleted result ->
            getKnowledgeModelCompleted model result |> tuplePrepend seed

        Edit knowledgeModelMsg ->
            updateEdit knowledgeModelMsg seed session model

        SaveCompleted result ->
            postEventsBulkCompleted model result |> tuplePrepend seed

        ReorderableMsg reorderableMsg ->
            let
                newReorderableState =
                    Reorderable.update reorderableMsg model.reorderableState
            in
            ( seed, { model | reorderableState = newReorderableState }, Cmd.none )
