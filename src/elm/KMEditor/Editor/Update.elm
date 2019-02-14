module KMEditor.Editor.Update exposing
    ( fetchData
    , isGuarded
    , update
    )

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
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
import KMEditor.Editor.Update.Tag exposing (deleteTag, updateTagForm, withGenerateTagEditEvent)
import KMEditor.Requests exposing (getKnowledgeModelData, getLevels, getMetrics, postEventsBulk)
import KMEditor.Routing exposing (Route(..))
import Models exposing (State)
import Msgs
import Ports
import Random exposing (Seed)
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


isGuarded : Model -> Maybe String
isGuarded model =
    if containsChanges model then
        Just unsavedChangesMsg

    else
        Nothing


unsavedChangesMsg : String
unsavedChangesMsg =
    "You have unsaved changes in the Knowledge Model, save or discard them first."


sendEventsCmd : (Msg -> Msgs.Msg) -> Session -> Model -> Cmd Msgs.Msg
sendEventsCmd wrapMsg session model =
    encodeEvents model.events
        |> postEventsBulk session model.branchUuid
        |> Jwt.send SubmitCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    let
        updateResult =
            case msg of
                Submit ->
                    let
                        send seed newModel a =
                            ( seed, { newModel | submitting = Loading }, sendEventsCmd wrapMsg state.session newModel )
                    in
                    case getActiveEditor model of
                        Just editor ->
                            case editor of
                                KMEditor data ->
                                    send
                                        |> withGenerateKMEditEvent state.seed model data

                                TagEditor data ->
                                    send
                                        |> withGenerateTagEditEvent state.seed model data

                                ChapterEditor data ->
                                    send
                                        |> withGenerateChapterEditEvent state.seed model data

                                QuestionEditor data ->
                                    send
                                        |> withGenerateQuestionEditEvent state.seed model data

                                AnswerEditor data ->
                                    send
                                        |> withGenerateAnswerEditEvent state.seed model data

                                ReferenceEditor data ->
                                    send
                                        |> withGenerateReferenceEditEvent state.seed model data

                                ExpertEditor data ->
                                    send
                                        |> withGenerateExpertEditEvent state.seed model data

                        _ ->
                            send state.seed model ()

                SubmitCompleted result ->
                    case result of
                        Ok _ ->
                            ( state.seed
                            , initialModel ""
                            , Cmd.batch [ Ports.clearUnloadMessage (), cmdNavigate state.key <| Routing.KMEditor Index ]
                            )

                        Err error ->
                            ( state.seed
                            , { model | submitting = getServerErrorJwt error "Knowledge model could not be saved" }
                            , getResultCmd result
                            )

                Discard ->
                    ( state.seed
                    , initialModel ""
                    , Cmd.batch [ Ports.clearUnloadMessage (), cmdNavigate state.key <| Routing.KMEditor Index ]
                    )

                PaneMsg paneMsg ->
                    ( state.seed, { model | splitPane = SplitPane.update paneMsg model.splitPane }, Cmd.none )

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
                    ( state.seed, createEditors newModel, cmd )

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
                    ( state.seed, createEditors newModel, cmd )

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
                    ( state.seed, createEditors newModel, cmd )

                ToggleOpen uuid ->
                    let
                        newEditors =
                            updateEditor model.editors toggleEditorOpen uuid
                    in
                    ( state.seed, { model | editors = newEditors }, Cmd.none )

                CloseAlert ->
                    ( state.seed, { model | alert = Nothing }, Cmd.none )

                SetActiveEditor uuid ->
                    case getActiveEditor model of
                        Just editor ->
                            case editor of
                                KMEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateKMEditEvent state.seed model data

                                TagEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateTagEditEvent state.seed model data

                                ChapterEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateChapterEditEvent state.seed model data

                                QuestionEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateQuestionEditEvent state.seed model data

                                AnswerEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateAnswerEditEvent state.seed model data

                                ReferenceEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateReferenceEditEvent state.seed model data

                                ExpertEditor data ->
                                    setActiveEditor wrapMsg uuid
                                        |> withGenerateExpertEditEvent state.seed model data

                        _ ->
                            setActiveEditor wrapMsg uuid state.seed model ()

                EditorMsg editorMsg ->
                    case ( editorMsg, getActiveEditor model ) of
                        ( KMEditorMsg kmEditorMsg, Just (KMEditor editorData) ) ->
                            case kmEditorMsg of
                                KMEditorFormMsg formMsg ->
                                    updateKMForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                ReorderChapters chapterList ->
                                    model
                                        |> insertEditor (KMEditor { editorData | chapters = Children.updateList chapterList editorData.chapters })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddChapter ->
                                    addChapter (scrollTopCmd wrapMsg)
                                        |> withGenerateKMEditEvent state.seed model editorData

                                ReorderTags tagList ->
                                    model
                                        |> insertEditor (KMEditor { editorData | tags = Children.updateList tagList editorData.tags })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddTag ->
                                    addTag (scrollTopCmd wrapMsg)
                                        |> withGenerateKMEditEvent state.seed model editorData

                        ( ChapterEditorMsg chapterEditorMsg, Just (ChapterEditor editorData) ) ->
                            case chapterEditorMsg of
                                ChapterFormMsg formMsg ->
                                    updateChapterForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                DeleteChapter uuid ->
                                    deleteChapter state.seed model uuid editorData
                                        |> withNoCmd

                                ReorderQuestions questionList ->
                                    model
                                        |> insertEditor (ChapterEditor { editorData | questions = Children.updateList questionList editorData.questions })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddQuestion ->
                                    addQuestion (scrollTopCmd wrapMsg)
                                        |> withGenerateChapterEditEvent state.seed model editorData

                        ( TagEditorMsg tagEditorMsg, Just (TagEditor editorData) ) ->
                            case tagEditorMsg of
                                TagFormMsg formMsg ->
                                    updateTagForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                DeleteTag uuid ->
                                    deleteTag state.seed model uuid editorData
                                        |> withNoCmd

                        ( QuestionEditorMsg questionEditorMsg, Just (QuestionEditor editorData) ) ->
                            case questionEditorMsg of
                                QuestionFormMsg formMsg ->
                                    updateQuestionForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                AddQuestionTag uuid ->
                                    addQuestionTag model uuid editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                RemoveQuestionTag uuid ->
                                    removeQuestionTag model uuid editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                DeleteQuestion uuid ->
                                    deleteQuestion state.seed model uuid editorData
                                        |> withNoCmd

                                ReorderAnswers answerList ->
                                    model
                                        |> insertEditor (QuestionEditor { editorData | answers = Children.updateList answerList editorData.answers })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddAnswer ->
                                    addAnswer (scrollTopCmd wrapMsg)
                                        |> withGenerateQuestionEditEvent state.seed model editorData

                                ReorderItemQuestions itemQuestionList ->
                                    model
                                        |> insertEditor (QuestionEditor { editorData | itemQuestions = Children.updateList itemQuestionList editorData.itemQuestions })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddAnswerItemTemplateQuestion ->
                                    addAnswerItemTemplateQuestion (scrollTopCmd wrapMsg)
                                        |> withGenerateQuestionEditEvent state.seed model editorData

                                ReorderReferences referenceList ->
                                    model
                                        |> insertEditor (QuestionEditor { editorData | references = Children.updateList referenceList editorData.references })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddReference ->
                                    addReference (scrollTopCmd wrapMsg)
                                        |> withGenerateQuestionEditEvent state.seed model editorData

                                ReorderExperts expertList ->
                                    model
                                        |> insertEditor (QuestionEditor { editorData | experts = Children.updateList expertList editorData.experts })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddExpert ->
                                    addExpert (scrollTopCmd wrapMsg)
                                        |> withGenerateQuestionEditEvent state.seed model editorData

                        ( AnswerEditorMsg answerEditorMsg, Just (AnswerEditor editorData) ) ->
                            case answerEditorMsg of
                                AnswerFormMsg formMsg ->
                                    updateAnswerForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                DeleteAnswer uuid ->
                                    deleteAnswer state.seed model uuid editorData
                                        |> withNoCmd

                                ReorderFollowUps followUpList ->
                                    model
                                        |> insertEditor (AnswerEditor { editorData | followUps = Children.updateList followUpList editorData.followUps })
                                        |> pair state.seed
                                        |> withNoCmd

                                AddFollowUp ->
                                    addFollowUp (scrollTopCmd wrapMsg)
                                        |> withGenerateAnswerEditEvent state.seed model editorData

                        ( ReferenceEditorMsg referenceEditorMsg, Just (ReferenceEditor editorData) ) ->
                            case referenceEditorMsg of
                                ReferenceFormMsg formMsg ->
                                    updateReferenceForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                DeleteReference uuid ->
                                    deleteReference state.seed model uuid editorData
                                        |> withNoCmd

                        ( ExpertEditorMsg expertEditorMsg, Just (ExpertEditor editorData) ) ->
                            case expertEditorMsg of
                                ExpertFormMsg formMsg ->
                                    updateExpertForm model formMsg editorData
                                        |> pair state.seed
                                        |> withNoCmd

                                DeleteExpert uuid ->
                                    deleteExpert state.seed model uuid editorData
                                        |> withNoCmd

                        _ ->
                            ( state.seed, model, Cmd.none )

                ReorderableMsg reorderableMsg ->
                    ( state.seed, { model | reorderableState = Reorderable.update reorderableMsg model.reorderableState }, Cmd.none )
    in
    updateResult |> withSetUnloadMsgCmd


withNoCmd : ( a, b ) -> ( a, b, Cmd msg )
withNoCmd ( a, b ) =
    ( a, b, Cmd.none )


withSetUnloadMsgCmd : ( a, Model, Cmd msg ) -> ( a, Model, Cmd msg )
withSetUnloadMsgCmd ( a, model, cmd ) =
    let
        newCmd =
            if containsChanges model then
                Cmd.batch [ cmd, Ports.setUnloadMessage unsavedChangesMsg ]

            else
                cmd
    in
    ( a, model, newCmd )


setActiveEditor : (Msg -> Msgs.Msg) -> String -> Seed -> Model -> a -> ( Seed, Model, Cmd Msgs.Msg )
setActiveEditor wrapMsg uuid seed model _ =
    ( seed, { model | activeEditorUuid = Just uuid }, scrollTopCmd wrapMsg )


createEditors : Model -> Model
createEditors model =
    case ActionResult.combine3 model.knowledgeModel model.metrics model.levels of
        Success ( knowledgeModel, metrics, levels ) ->
            { model | editors = createKnowledgeModelEditor (getEditorContext model) knowledgeModel model.editors }

        _ ->
            model


scrollTopCmd : (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
scrollTopCmd wrapMsg =
    Ports.scrollToTop "editor-view"
