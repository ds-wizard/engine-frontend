module KMEditor.Editor.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Common.Models.Path exposing (Path, PathNode(..))
import KMEditor.Editor.Models exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Msgs exposing (..)
import KMEditor.Editor.Update.UpdateAnswer as UpdateAnswer
import KMEditor.Editor.Update.UpdateChapter as UpdateChapter
import KMEditor.Editor.Update.UpdateExpert as UpdateExpert
import KMEditor.Editor.Update.UpdateKnowledgeModel as UpdateKnowledgeModel
import KMEditor.Editor.Update.UpdateQuestion as UpdateQuestion
import KMEditor.Editor.Update.UpdateReference as UpdateReference
import KMEditor.Editor.Update.Utils exposing (..)
import KMEditor.Requests exposing (getKnowledgeModelData, postEventsBulk)
import KMEditor.Routing exposing (Route(Index))
import Msgs
import Random.Pcg exposing (Seed)
import Reorderable
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)
import Utils exposing (getUuid, tuplePrepend)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    getKnowledgeModelData uuid session
        |> Jwt.send GetKnowledgeModelCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed session model =
    case msg of
        GetKnowledgeModelCompleted result ->
            getKnowledgeModelCompleted model result |> tuplePrepend seed

        Edit knowledgeModelMsg ->
            updateEdit knowledgeModelMsg wrapMsg seed session model

        SaveCompleted result ->
            postEventsBulkCompleted model result |> tuplePrepend seed

        ReorderableMsg reorderableMsg ->
            let
                newReorderableState =
                    Reorderable.update reorderableMsg model.reorderableState
            in
            ( seed, { model | reorderableState = newReorderableState }, Cmd.none )


getKnowledgeModelCompleted : Model -> Result Jwt.JwtError KnowledgeModel -> ( Model, Cmd Msgs.Msg )
getKnowledgeModelCompleted model result =
    let
        newModel =
            case result of
                Ok knowledgeModel ->
                    { model | knowledgeModelEditor = Success <| createKnowledgeModelEditor knowledgeModel }

                Err error ->
                    { model | knowledgeModelEditor = getServerErrorJwt error "Unable to get knowledge model" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


postEventsBulkCmd : (Msg -> Msgs.Msg) -> String -> List Event -> Session -> Cmd Msgs.Msg
postEventsBulkCmd wrapMsg uuid events session =
    encodeEvents events
        |> postEventsBulk session uuid
        |> Jwt.send SaveCompleted
        |> Cmd.map wrapMsg


postEventsBulkCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postEventsBulkCompleted model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate <| KMEditor Index )

        Err error ->
            ( { model | saving = getServerErrorJwt error "Knowledge model could not be saved" }
            , getResultCmd result
            )


updateEdit : KnowledgeModelMsg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
updateEdit msg wrapMsg seed session model =
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
                        ( { model | saving = Loading }
                        , postEventsBulkCmd wrapMsg model.branchUuid newEvents session
                        )
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
updateKnowledgeModel msg seed ((KnowledgeModelEditor editor) as kmEditor) =
    let
        currentPath =
            [ KMPathNode editor.knowledgeModel.uuid ]
    in
    case msg of
        KnowledgeModelFormMsg formMsg ->
            UpdateKnowledgeModel.formMsg formMsg seed kmEditor

        AddChapter ->
            UpdateKnowledgeModel.addChapter seed currentPath kmEditor

        ViewChapter uuid ->
            UpdateKnowledgeModel.viewChapter uuid seed kmEditor

        DeleteChapter uuid ->
            UpdateKnowledgeModel.deleteChapter uuid seed currentPath kmEditor

        ReorderChapterList newChapters ->
            ( seed, KnowledgeModelEditor { editor | chapters = newChapters, chaptersDirty = True }, Nothing, False )

        ChapterMsg uuid chapterMsg ->
            let
                ( newSeed, newChapters, event ) =
                    updateInListWithSeed editor.chapters seed (matchChapter uuid) (updateChapter chapterMsg currentPath)
            in
            ( newSeed, KnowledgeModelEditor { editor | chapters = newChapters }, event, False )


updateChapter : ChapterMsg -> Path -> Seed -> ChapterEditor -> ( Seed, ChapterEditor, Maybe Event )
updateChapter msg path seed ((ChapterEditor editor) as chapterEditor) =
    let
        currentPath =
            appendPath path (ChapterPathNode editor.chapter.uuid)
    in
    case msg of
        ChapterFormMsg formMsg ->
            UpdateChapter.formMsg formMsg seed path chapterEditor

        ChapterCancel ->
            UpdateChapter.cancel seed chapterEditor

        AddChapterQuestion ->
            UpdateChapter.addQuestion seed currentPath chapterEditor

        ViewQuestion uuid ->
            UpdateChapter.viewQuestion uuid seed chapterEditor

        DeleteChapterQuestion uuid ->
            UpdateChapter.deleteQuestion uuid seed currentPath chapterEditor

        ReorderQuestionList newQuestions ->
            ( seed, ChapterEditor { editor | questions = newQuestions, questionsDirty = True }, Nothing )

        ChapterQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newQuestions, event ) =
                    updateInListWithSeed editor.questions seed (matchQuestion uuid) (updateQuestion questionMsg currentPath)
            in
            ( newSeed, ChapterEditor { editor | questions = newQuestions }, event )

        ChapterEditorStateMsg state ->
            ( seed, ChapterEditor { editor | editorState = state }, Nothing )


updateQuestion : QuestionMsg -> Path -> Seed -> QuestionEditor -> ( Seed, QuestionEditor, Maybe Event )
updateQuestion msg path seed ((QuestionEditor editor) as questionEditor) =
    let
        currentPath =
            appendPath path (QuestionPathNode editor.question.uuid)
    in
    case msg of
        QuestionFormMsg formMsg ->
            UpdateQuestion.formMsg formMsg seed path questionEditor

        QuestionCancel ->
            UpdateQuestion.cancel seed questionEditor

        AddAnswerItemTemplateQuestion ->
            UpdateQuestion.addAnswerItemTemplateQuestion seed currentPath questionEditor

        ViewAnswerItemTemplateQuestion uuid ->
            UpdateQuestion.viewAnswerItemTemplateQuestion uuid seed questionEditor

        DeleteAnswerItemTemplateQuestion uuid ->
            UpdateQuestion.deleteAnswerItemTemplateQuestion uuid seed currentPath questionEditor

        ReorderAnswerItemTemplateQuestions newAnswerItemTemplateQuestions ->
            ( seed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTemplateQuestions, answersDirty = True }, Nothing )

        AnswerItemTemplateQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newAnswerItemTemplateQuestions, event ) =
                    updateInListWithSeed editor.answerItemTemplateQuestions seed (matchQuestion uuid) (updateQuestion questionMsg currentPath)
            in
            ( seed, QuestionEditor { editor | answerItemTemplateQuestions = newAnswerItemTemplateQuestions, answerItemTemplateQuestionsDirty = True }, event )

        AddAnswer ->
            UpdateQuestion.addAnswer seed currentPath questionEditor

        ViewAnswer uuid ->
            UpdateQuestion.viewAnswer uuid seed questionEditor

        DeleteAnswer uuid ->
            UpdateQuestion.deleteAnswer uuid seed currentPath questionEditor

        ReorderAnswerList newAnswers ->
            ( seed, QuestionEditor { editor | answers = newAnswers, answersDirty = True }, Nothing )

        AnswerMsg uuid answerMsg ->
            let
                ( newSeed, newAnswers, event ) =
                    updateInListWithSeed editor.answers seed (matchAnswer uuid) (updateAnswer answerMsg currentPath)
            in
            ( newSeed, QuestionEditor { editor | answers = newAnswers }, event )

        AddReference ->
            UpdateQuestion.addReference seed currentPath questionEditor

        ViewReference uuid ->
            UpdateQuestion.viewReference uuid seed questionEditor

        DeleteReference uuid ->
            UpdateQuestion.deleteReference uuid seed currentPath questionEditor

        ReorderReferenceList newReferences ->
            ( seed, QuestionEditor { editor | references = newReferences, referencesDirty = True }, Nothing )

        ReferenceMsg uuid referenceMsg ->
            let
                ( newSeed, newReferences, event ) =
                    updateInListWithSeed editor.references seed (matchReference uuid) (updateReference referenceMsg currentPath)
            in
            ( newSeed, QuestionEditor { editor | references = newReferences }, event )

        AddExpert ->
            UpdateQuestion.addExpert seed currentPath questionEditor

        ViewExpert uuid ->
            UpdateQuestion.viewExpert uuid seed questionEditor

        DeleteExpert uuid ->
            UpdateQuestion.deleteExpert uuid seed currentPath questionEditor

        ReorderExpertList newExperts ->
            ( seed, QuestionEditor { editor | experts = newExperts, expertsDirty = True }, Nothing )

        ExpertMsg uuid expertMsg ->
            let
                ( newSeed, newExperts, event ) =
                    updateInListWithSeed editor.experts seed (matchExpert uuid) (updateExpert expertMsg currentPath)
            in
            ( newSeed, QuestionEditor { editor | experts = newExperts }, event )

        QuestionEditorStateMsg state ->
            ( seed, QuestionEditor { editor | editorState = state }, Nothing )


updateAnswer : AnswerMsg -> Path -> Seed -> AnswerEditor -> ( Seed, AnswerEditor, Maybe Event )
updateAnswer msg path seed ((AnswerEditor editor) as answerEditor) =
    let
        currentPath =
            appendPath path (AnswerPathNode editor.answer.uuid)
    in
    case msg of
        AnswerFormMsg formMsg ->
            UpdateAnswer.formMsg formMsg seed path answerEditor

        AnswerCancel ->
            UpdateAnswer.cancel seed answerEditor

        AddFollowUpQuestion ->
            UpdateAnswer.addFollowUpQuestion seed currentPath answerEditor

        ViewFollowUpQuestion uuid ->
            UpdateAnswer.viewFollowUpQuestion uuid seed answerEditor

        DeleteFollowUpQuestion uuid ->
            UpdateAnswer.deleteFollowUpQuestion uuid seed currentPath answerEditor

        ReorderFollowUpQuestionList newFollowUps ->
            ( seed, AnswerEditor { editor | followUps = newFollowUps, followUpsDirty = True }, Nothing )

        FollowUpQuestionMsg uuid questionMsg ->
            let
                ( newSeed, newFollowUps, event ) =
                    updateInListWithSeed editor.followUps seed (matchQuestion uuid) (updateQuestion questionMsg currentPath)
            in
            ( newSeed, AnswerEditor { editor | followUps = newFollowUps }, event )

        AnswerEditorStateMsg state ->
            ( seed, AnswerEditor { editor | editorState = state }, Nothing )


updateReference : ReferenceMsg -> Path -> Seed -> ReferenceEditor -> ( Seed, ReferenceEditor, Maybe Event )
updateReference msg path seed referenceEditor =
    case msg of
        ReferenceFormMsg formMsg ->
            UpdateReference.formMsg formMsg seed path referenceEditor

        ReferenceCancel ->
            UpdateReference.cancel seed referenceEditor


updateExpert : ExpertMsg -> Path -> Seed -> ExpertEditor -> ( Seed, ExpertEditor, Maybe Event )
updateExpert msg path seed expertEditor =
    case msg of
        ExpertFormMsg formMsg ->
            UpdateExpert.formMsg formMsg seed path expertEditor

        ExpertCancel ->
            UpdateExpert.cancel seed expertEditor
