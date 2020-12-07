module Wizard.KMEditor.Editor.KMEditor.Update exposing (generateEvents, update)

import Dict
import Random exposing (Seed)
import Reorderable
import Shared.Utils exposing (pair)
import SplitPane
import ValueList
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModal as MoveModal
import Wizard.KMEditor.Editor.KMEditor.Models exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (updateEditor)
import Wizard.KMEditor.Editor.KMEditor.Update.Answer exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Chapter exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createMoveAnswerEvent, createMoveExpertEvent, createMoveQuestionEvent, createMoveReferenceEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Expert exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Integration exposing (deleteIntegration, updateIntegrationForm, withGenerateIntegrationEditEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.KnowledgeModel exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Question exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Reference exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Update.Tag exposing (deleteTag, updateTagForm, withGenerateTagEditEvent)
import Wizard.Msgs
import Wizard.Ports as Ports


update : Msg -> AppState -> Model -> Cmd Wizard.Msgs.Msg -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg appState model fetchPreviewCmd =
    case msg of
        PaneMsg paneMsg ->
            ( appState.seed, { model | splitPane = SplitPane.update paneMsg model.splitPane }, Cmd.none )

        ToggleOpen uuid ->
            let
                newEditors =
                    updateEditor model.editors toggleEditorOpen uuid
            in
            ( appState.seed, { model | editors = newEditors }, Cmd.none )

        CloseAlert ->
            ( appState.seed, { model | alert = Nothing }, Cmd.none )

        SetActiveEditor uuid ->
            case getActiveEditor model of
                Just editor ->
                    case editor of
                        KMEditor data ->
                            setActiveEditor uuid
                                |> withGenerateKMEditEvent appState appState.seed model data

                        TagEditor data ->
                            setActiveEditor uuid
                                |> withGenerateTagEditEvent appState appState.seed model data

                        IntegrationEditor data ->
                            setActiveEditor uuid
                                |> withGenerateIntegrationEditEvent appState appState.seed model data

                        ChapterEditor data ->
                            setActiveEditor uuid
                                |> withGenerateChapterEditEvent appState appState.seed model data

                        QuestionEditor data ->
                            setActiveEditor uuid
                                |> withGenerateQuestionEditEvent appState appState.seed model data

                        AnswerEditor data ->
                            setActiveEditor uuid
                                |> withGenerateAnswerEditEvent appState appState.seed model data

                        ReferenceEditor data ->
                            setActiveEditor uuid
                                |> withGenerateReferenceEditEvent appState appState.seed model data

                        ExpertEditor data ->
                            setActiveEditor uuid
                                |> withGenerateExpertEditEvent appState appState.seed model data

                _ ->
                    setActiveEditor uuid appState.seed model ()

        EditorMsg editorMsg ->
            case ( editorMsg, getActiveEditor model ) of
                ( KMEditorMsg kmEditorMsg, Just (KMEditor editorData) ) ->
                    case kmEditorMsg of
                        KMEditorFormMsg formMsg ->
                            updateKMForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        ReorderChapters chapterList ->
                            model
                                |> insertEditor (KMEditor { editorData | chapters = Children.updateList chapterList editorData.chapters })
                                |> pair appState.seed
                                |> withNoCmd

                        AddChapter ->
                            addChapter scrollTopCmd
                                |> withGenerateKMEditEvent appState appState.seed model editorData

                        ReorderTags tagList ->
                            model
                                |> insertEditor (KMEditor { editorData | tags = Children.updateList tagList editorData.tags })
                                |> pair appState.seed
                                |> withNoCmd

                        AddTag ->
                            addTag scrollTopCmd
                                |> withGenerateKMEditEvent appState appState.seed model editorData

                        ReorderIntegrations integrationList ->
                            model
                                |> insertEditor (KMEditor { editorData | integrations = Children.updateList integrationList editorData.tags })
                                |> pair appState.seed
                                |> withNoCmd

                        AddIntegration ->
                            addIntegration scrollTopCmd
                                |> withGenerateKMEditEvent appState appState.seed model editorData

                ( ChapterEditorMsg chapterEditorMsg, Just (ChapterEditor editorData) ) ->
                    case chapterEditorMsg of
                        ChapterFormMsg formMsg ->
                            updateChapterForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteChapter uuid ->
                            deleteChapter appState.seed model uuid editorData
                                |> withNoCmd

                        ReorderQuestions questionList ->
                            model
                                |> insertEditor (ChapterEditor { editorData | questions = Children.updateList questionList editorData.questions })
                                |> pair appState.seed
                                |> withNoCmd

                        AddQuestion ->
                            addQuestion scrollTopCmd
                                |> withGenerateChapterEditEvent appState appState.seed model editorData

                ( TagEditorMsg tagEditorMsg, Just (TagEditor editorData) ) ->
                    case tagEditorMsg of
                        TagFormMsg formMsg ->
                            updateTagForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteTag uuid ->
                            deleteTag appState.seed model uuid editorData
                                |> withNoCmd

                ( IntegrationEditorMsg integrationEditorMsg, Just (IntegrationEditor editorData) ) ->
                    case integrationEditorMsg of
                        IntegrationFormMsg formMsg ->
                            updateIntegrationForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        ToggleDeleteConfirm open ->
                            insertEditor (IntegrationEditor { editorData | deleteConfirmOpen = open }) model
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteIntegration uuid ->
                            deleteIntegration appState.seed model uuid editorData
                                |> withCmd fetchPreviewCmd

                        PropsListMsg propsListMsg ->
                            insertEditor (IntegrationEditor { editorData | props = ValueList.update propsListMsg editorData.props }) model
                                |> pair appState.seed
                                |> withNoCmd

                ( QuestionEditorMsg questionEditorMsg, Just (QuestionEditor editorData) ) ->
                    case questionEditorMsg of
                        QuestionFormMsg formMsg ->
                            updateQuestionForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        AddQuestionTag uuid ->
                            addQuestionTag model uuid editorData
                                |> pair appState.seed
                                |> withNoCmd

                        RemoveQuestionTag uuid ->
                            removeQuestionTag model uuid editorData
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteQuestion uuid ->
                            deleteQuestion appState.seed model uuid editorData
                                |> withNoCmd

                        ReorderAnswers answerList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | answers = Children.updateList answerList editorData.answers })
                                |> pair appState.seed
                                |> withNoCmd

                        AddAnswer ->
                            addAnswer scrollTopCmd
                                |> withGenerateQuestionEditEvent appState appState.seed model editorData

                        ReorderItemQuestions itemQuestionList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | itemTemplateQuestions = Children.updateList itemQuestionList editorData.itemTemplateQuestions })
                                |> pair appState.seed
                                |> withNoCmd

                        AddAnswerItemTemplateQuestion ->
                            addAnswerItemTemplateQuestion scrollTopCmd
                                |> withGenerateQuestionEditEvent appState appState.seed model editorData

                        ReorderReferences referenceList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | references = Children.updateList referenceList editorData.references })
                                |> pair appState.seed
                                |> withNoCmd

                        AddReference ->
                            addReference scrollTopCmd
                                |> withGenerateQuestionEditEvent appState appState.seed model editorData

                        ReorderExperts expertList ->
                            model
                                |> insertEditor (QuestionEditor { editorData | experts = Children.updateList expertList editorData.experts })
                                |> pair appState.seed
                                |> withNoCmd

                        AddExpert ->
                            addExpert scrollTopCmd
                                |> withGenerateQuestionEditEvent appState appState.seed model editorData

                ( AnswerEditorMsg answerEditorMsg, Just (AnswerEditor editorData) ) ->
                    case answerEditorMsg of
                        AnswerFormMsg formMsg ->
                            updateAnswerForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteAnswer uuid ->
                            deleteAnswer appState.seed model uuid editorData
                                |> withNoCmd

                        ReorderFollowUps followUpList ->
                            model
                                |> insertEditor (AnswerEditor { editorData | followUps = Children.updateList followUpList editorData.followUps })
                                |> pair appState.seed
                                |> withNoCmd

                        AddFollowUp ->
                            addFollowUp scrollTopCmd
                                |> withGenerateAnswerEditEvent appState appState.seed model editorData

                ( ReferenceEditorMsg referenceEditorMsg, Just (ReferenceEditor editorData) ) ->
                    case referenceEditorMsg of
                        ReferenceFormMsg formMsg ->
                            updateReferenceForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteReference uuid ->
                            deleteReference appState.seed model uuid editorData
                                |> withNoCmd

                ( ExpertEditorMsg expertEditorMsg, Just (ExpertEditor editorData) ) ->
                    case expertEditorMsg of
                        ExpertFormMsg formMsg ->
                            updateExpertForm model formMsg editorData
                                |> pair appState.seed
                                |> withNoCmd

                        DeleteExpert uuid ->
                            deleteExpert appState.seed model uuid editorData
                                |> withNoCmd

                _ ->
                    ( appState.seed, model, Cmd.none )

        ReorderableMsg reorderableMsg ->
            ( appState.seed, { model | reorderableState = Reorderable.update reorderableMsg model.reorderableState }, Cmd.none )

        CopyUuid uuid ->
            ( appState.seed, model, Ports.copyToClipboard uuid )

        OpenMoveModal ->
            ( appState.seed, { model | moveModal = MoveModal.open model.moveModal }, Cmd.none )

        MoveModalMsg moveModalMsg ->
            let
                createMoveEvent constructor entityUuid parentUuid seed newModel _ =
                    let
                        ( moveEvent, newSeed ) =
                            constructor
                                (MoveModal.getSelectedTargetUuid newModel.moveModal)
                                entityUuid
                                parentUuid
                                seed

                        events =
                            newModel.events ++ [ moveEvent ]
                    in
                    ( newSeed, { newModel | events = events }, fetchPreviewCmd )
            in
            case moveModalMsg of
                MoveModal.Submit ->
                    case getActiveEditor model of
                        Just (QuestionEditor questionEditor) ->
                            createMoveEvent createMoveQuestionEvent questionEditor.uuid questionEditor.parentUuid
                                |> withGenerateQuestionEditEvent appState appState.seed model questionEditor

                        Just (AnswerEditor answerEditor) ->
                            createMoveEvent createMoveAnswerEvent answerEditor.uuid answerEditor.parentUuid
                                |> withGenerateAnswerEditEvent appState appState.seed model answerEditor

                        Just (ReferenceEditor referenceEditor) ->
                            createMoveEvent createMoveReferenceEvent referenceEditor.uuid referenceEditor.parentUuid
                                |> withGenerateReferenceEditEvent appState appState.seed model referenceEditor

                        Just (ExpertEditor expertEditor) ->
                            createMoveEvent createMoveExpertEvent expertEditor.uuid expertEditor.parentUuid
                                |> withGenerateExpertEditEvent appState appState.seed model expertEditor

                        _ ->
                            ( appState.seed, { model | moveModal = MoveModal.update moveModalMsg model.moveModal { editors = model.editors } }, Cmd.none )

                _ ->
                    ( appState.seed, { model | moveModal = MoveModal.update moveModalMsg model.moveModal { editors = model.editors } }, Cmd.none )

        TreeExpandAll ->
            let
                newModel =
                    { model | editors = Dict.map (\_ e -> setEditorOpen e) model.editors }
            in
            ( appState.seed, newModel, Cmd.none )

        TreeCollapseAll ->
            let
                newModel =
                    { model | editors = Dict.map (\_ e -> setEditorClosed e) model.editors }
            in
            ( appState.seed, newModel, Cmd.none )


generateEvents : AppState -> Seed -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
generateEvents appState seed model =
    let
        updateModel newSeed newModel a =
            ( newSeed, newModel, Cmd.none )
    in
    case getActiveEditor model of
        Just editor ->
            case editor of
                KMEditor data ->
                    withGenerateKMEditEvent appState seed model data updateModel

                TagEditor data ->
                    withGenerateTagEditEvent appState seed model data updateModel

                IntegrationEditor data ->
                    withGenerateIntegrationEditEvent appState seed model data updateModel

                ChapterEditor data ->
                    withGenerateChapterEditEvent appState seed model data updateModel

                QuestionEditor data ->
                    withGenerateQuestionEditEvent appState seed model data updateModel

                AnswerEditor data ->
                    withGenerateAnswerEditEvent appState seed model data updateModel

                ReferenceEditor data ->
                    withGenerateReferenceEditEvent appState seed model data updateModel

                ExpertEditor data ->
                    withGenerateExpertEditEvent appState seed model data updateModel

        _ ->
            ( seed, model, Cmd.none )


withNoCmd : ( a, b ) -> ( a, b, Cmd msg )
withNoCmd =
    withCmd Cmd.none


withCmd : Cmd msg -> ( a, b ) -> ( a, b, Cmd msg )
withCmd cmd ( a, b ) =
    ( a, b, cmd )


setActiveEditor : String -> Seed -> Model -> a -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
setActiveEditor uuid seed model _ =
    ( seed, { model | activeEditorUuid = Just uuid }, scrollTopCmd )


scrollTopCmd : Cmd Wizard.Msgs.Msg
scrollTopCmd =
    Ports.scrollToTop "#editor-view"
