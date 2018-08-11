module KMEditor.Editor.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(Loading, Success), combine3, mapSuccess)
import Dom.Scroll
import Jwt
import KMEditor.Common.Models.Events exposing (encodeEvents)
import KMEditor.Editor.Models exposing (..)
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Msgs exposing (..)
import KMEditor.Editor.Update.Abstract exposing (updateEditor)
import KMEditor.Editor.Update.Answer exposing (..)
import KMEditor.Editor.Update.Chapter exposing (..)
import KMEditor.Editor.Update.Expert exposing (..)
import KMEditor.Editor.Update.KnowledgeModel exposing (..)
import KMEditor.Editor.Update.Question exposing (..)
import KMEditor.Editor.Update.Reference exposing (..)
import KMEditor.Requests exposing (getKnowledgeModelData, getLevels, getMetrics, postEventsBulk)
import KMEditor.Routing exposing (Route(Index))
import Msgs
import Random.Pcg exposing (Seed)
import Reorderable
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)
import SplitPane
import Task
import Utils exposing (pair)


fetchData : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg uuid session =
    Cmd.batch
        [ fetchKnowledgeModel wrapMsg uuid session
        , fetchMetrics wrapMsg session
        , fetchLevels wrapMsg session
        ]


fetchKnowledgeModel : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
fetchKnowledgeModel wrapMsg uuid session =
    getKnowledgeModelData uuid session
        |> Jwt.send GetKnowledgeModelCompleted
        |> Cmd.map wrapMsg


fetchMetrics : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchMetrics wrapMsg session =
    getMetrics session
        |> Jwt.send GetMetricsCompleted
        |> Cmd.map wrapMsg


fetchLevels : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchLevels wrapMsg session =
    getLevels session
        |> Jwt.send GetLevelsCompleted
        |> Cmd.map wrapMsg


sendEventsCmd : (Msg -> Msgs.Msg) -> Session -> Model -> Cmd Msgs.Msg
sendEventsCmd wrapMsg session model =
    encodeEvents model.events
        |> postEventsBulk session model.branchUuid
        |> Jwt.send SubmitCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed session model =
    case msg of
        Submit ->
            let
                send seed model a =
                    ( seed, { model | submitting = Loading }, sendEventsCmd wrapMsg session model )
            in
            case getActiveEditor model of
                Just editor ->
                    case editor of
                        KMEditor data ->
                            send
                                |> withGenerateKMEditEvent seed model data

                        ChapterEditor data ->
                            send
                                |> withGenerateChapterEditEvent seed model data

                        QuestionEditor data ->
                            send
                                |> withGenerateQuestionEditEvent seed model data

                        AnswerEditor data ->
                            send
                                |> withGenerateAnswerEditEvent seed model data

                        ReferenceEditor data ->
                            send
                                |> withGenerateReferenceEditEvent seed model data

                        ExpertEditor data ->
                            send
                                |> withGenerateExpertEditEvent seed model data

                _ ->
                    send seed model ()

        SubmitCompleted result ->
            case result of
                Ok _ ->
                    ( seed, model, cmdNavigate <| Routing.KMEditor Index )

                Err error ->
                    ( seed
                    , { model | submitting = getServerErrorJwt error "Knowledge model could not be saved" }
                    , getResultCmd result
                    )

        PaneMsg paneMsg ->
            ( seed, { model | splitPane = SplitPane.update paneMsg model.splitPane }, Cmd.none )

        GetKnowledgeModelCompleted result ->
            let
                newModel =
                    case result of
                        Ok knowledgeModel ->
                            { model
                                | activeEditorUuid = Just knowledgeModel.uuid
                                , kmUuid = Success knowledgeModel.uuid
                                , knowledgeModel = Success knowledgeModel
                            }

                        Err error ->
                            { model
                                | kmUuid = getServerErrorJwt error "Unable to get knowledge model"
                            }

                cmd =
                    getResultCmd result
            in
            ( seed, createEditors newModel, cmd )

        GetMetricsCompleted result ->
            let
                newModel =
                    case result of
                        Ok metrics ->
                            { model | metrics = Success metrics }

                        Err error ->
                            { model | metrics = getServerErrorJwt error "Unable to get metrics" }

                cmd =
                    getResultCmd result
            in
            ( seed, createEditors newModel, cmd )

        GetLevelsCompleted result ->
            let
                newModel =
                    case result of
                        Ok levels ->
                            { model | levels = Success levels }

                        Err error ->
                            { model | levels = getServerErrorJwt error "Unable to get levels" }

                cmd =
                    getResultCmd result
            in
            ( seed, createEditors newModel, cmd )

        ToggleOpen uuid ->
            let
                newEditors =
                    updateEditor model.editors toggleEditorOpen uuid
            in
            ( seed, { model | editors = newEditors }, Cmd.none )

        CloseAlert ->
            ( seed, { model | alert = Nothing }, Cmd.none )

        SetActiveEditor uuid ->
            case getActiveEditor model of
                Just editor ->
                    case editor of
                        KMEditor data ->
                            setActiveEditor wrapMsg uuid
                                |> withGenerateKMEditEvent seed model data

                        ChapterEditor data ->
                            setActiveEditor wrapMsg uuid
                                |> withGenerateChapterEditEvent seed model data

                        QuestionEditor data ->
                            setActiveEditor wrapMsg uuid
                                |> withGenerateQuestionEditEvent seed model data

                        AnswerEditor data ->
                            setActiveEditor wrapMsg uuid
                                |> withGenerateAnswerEditEvent seed model data

                        ReferenceEditor data ->
                            setActiveEditor wrapMsg uuid
                                |> withGenerateReferenceEditEvent seed model data

                        ExpertEditor data ->
                            setActiveEditor wrapMsg uuid
                                |> withGenerateExpertEditEvent seed model data

                _ ->
                    setActiveEditor wrapMsg uuid seed model ()

        EditorMsg editorMsg ->
            case ( editorMsg, getActiveEditor model ) of
                ( KMEditorMsg kmEditorMsg, Just (KMEditor editorData) ) ->
                    case kmEditorMsg of
                        KMEditorFormMsg formMsg ->
                            updateKMForm model formMsg editorData
                                |> pair seed
                                |> withNoCmd

                        ReorderChapters chapterList ->
                            model
                                |> insertEditor (KMEditor { editorData | chapters = Children.updateList chapterList editorData.chapters })
                                |> pair seed
                                |> withNoCmd

                        AddChapter ->
                            addChapter
                                |> withGenerateKMEditEvent seed model editorData

                ( ChapterEditorMsg chapterEditorMsg, Just (ChapterEditor editorData) ) ->
                    case chapterEditorMsg of
                        ChapterFormMsg formMsg ->
                            updateChapterForm model formMsg editorData
                                |> pair seed
                                |> withNoCmd

                        DeleteChapter uuid ->
                            deleteChapter seed model uuid editorData
                                |> withNoCmd

                        ReorderQuestions questionList ->
                            model
                                |> insertEditor (ChapterEditor { editorData | questions = Children.updateList questionList editorData.questions })
                                |> pair seed
                                |> withNoCmd

                        AddQuestion ->
                            addQuestion
                                |> withGenerateChapterEditEvent seed model editorData

                ( QuestionEditorMsg questionEditorMsg, Just (QuestionEditor editorData) ) ->
                    case questionEditorMsg of
                        QuestionFormMsg formMsg ->
                            updateQuestionForm model formMsg editorData
                                |> pair seed
                                |> withNoCmd

                        DeleteQuestion uuid ->
                            deleteQuestion seed model uuid editorData
                                |> withNoCmd

                        ReorderAnswers answerList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | answers = Children.updateList answerList editorData.answers })
                                |> pair seed
                                |> withNoCmd

                        AddAnswer ->
                            addAnswer
                                |> withGenerateQuestionEditEvent seed model editorData

                        ReorderAnswerItemTemplateQuestions answerItemTemplateQuestionList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | answerItemTemplateQuestions = Children.updateList answerItemTemplateQuestionList editorData.answerItemTemplateQuestions })
                                |> pair seed
                                |> withNoCmd

                        AddAnswerItemTemplateQuestion ->
                            addAnswerItemTemplateQuestion
                                |> withGenerateQuestionEditEvent seed model editorData

                        ReorderReferences referenceList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | references = Children.updateList referenceList editorData.references })
                                |> pair seed
                                |> withNoCmd

                        AddReference ->
                            addReference
                                |> withGenerateQuestionEditEvent seed model editorData

                        ReorderExperts expertList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | experts = Children.updateList expertList editorData.experts })
                                |> pair seed
                                |> withNoCmd

                        AddExpert ->
                            addExpert
                                |> withGenerateQuestionEditEvent seed model editorData

                ( AnswerEditorMsg answerEditorMsg, Just (AnswerEditor editorData) ) ->
                    case answerEditorMsg of
                        AnswerFormMsg formMsg ->
                            updateAnswerForm model formMsg editorData
                                |> pair seed
                                |> withNoCmd

                        DeleteAnswer uuid ->
                            deleteAnswer seed model uuid editorData
                                |> withNoCmd

                        ReorderFollowUps followUpList ->
                            model
                                |> insertEditor (AnswerEditor { editorData | followUps = Children.updateList followUpList editorData.followUps })
                                |> pair seed
                                |> withNoCmd

                        AddFollowUp ->
                            addFollowUp
                                |> withGenerateAnswerEditEvent seed model editorData

                ( ReferenceEditorMsg referenceEditorMsg, Just (ReferenceEditor editorData) ) ->
                    case referenceEditorMsg of
                        ReferenceFormMsg formMsg ->
                            updateReferenceForm model formMsg editorData
                                |> pair seed
                                |> withNoCmd

                        DeleteReference uuid ->
                            deleteReference seed model uuid editorData
                                |> withNoCmd

                ( ExpertEditorMsg expertEditorMsg, Just (ExpertEditor editorData) ) ->
                    case expertEditorMsg of
                        ExpertFormMsg formMsg ->
                            updateExpertForm model formMsg editorData
                                |> pair seed
                                |> withNoCmd

                        DeleteExpert uuid ->
                            deleteExpert seed model uuid editorData
                                |> withNoCmd

                _ ->
                    ( seed, model, Cmd.none )

        ReorderableMsg reorderableMsg ->
            ( seed, { model | reorderableState = Reorderable.update reorderableMsg model.reorderableState }, Cmd.none )

        NoOp ->
            ( seed, model, Cmd.none )


withNoCmd : ( a, b ) -> ( a, b, Cmd msg )
withNoCmd ( a, b ) =
    ( a, b, Cmd.none )


setActiveEditor : (Msg -> Msgs.Msg) -> String -> Seed -> Model -> a -> ( Seed, Model, Cmd Msgs.Msg )
setActiveEditor wrapMsg uuid seed model _ =
    let
        cmd =
            Dom.Scroll.toTop "editor-view"
                |> Task.attempt (always NoOp)
                |> Cmd.map wrapMsg
    in
    ( seed, { model | activeEditorUuid = Just uuid }, cmd )


createEditors : Model -> Model
createEditors model =
    case combine3 model.knowledgeModel model.metrics model.levels of
        Success ( knowledgeModel, metrics, levels ) ->
            { model | editors = createKnowledgeModelEditor (getEditorContext model) knowledgeModel model.editors }

        _ ->
            model
