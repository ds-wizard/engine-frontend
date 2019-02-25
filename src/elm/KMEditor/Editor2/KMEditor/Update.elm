module KMEditor.Editor2.KMEditor.Update exposing (generateEvents, update)

import KMEditor.Editor2.KMEditor.Models exposing (..)
import KMEditor.Editor2.KMEditor.Models.Children as Children exposing (Children)
import KMEditor.Editor2.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor2.KMEditor.Msgs exposing (..)
import KMEditor.Editor2.KMEditor.Update.Abstract exposing (updateEditor)
import KMEditor.Editor2.KMEditor.Update.Answer exposing (..)
import KMEditor.Editor2.KMEditor.Update.Chapter exposing (..)
import KMEditor.Editor2.KMEditor.Update.Expert exposing (..)
import KMEditor.Editor2.KMEditor.Update.KnowledgeModel exposing (..)
import KMEditor.Editor2.KMEditor.Update.Question exposing (..)
import KMEditor.Editor2.KMEditor.Update.Reference exposing (..)
import KMEditor.Editor2.KMEditor.Update.Tag exposing (deleteTag, updateTagForm, withGenerateTagEditEvent)
import Models exposing (State)
import Msgs
import Ports
import Random exposing (Seed)
import Reorderable
import SplitPane
import Utils exposing (pair)


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        PaneMsg paneMsg ->
            ( state.seed, { model | splitPane = SplitPane.update paneMsg model.splitPane }, Cmd.none )

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
                                |> insertEditor (QuestionEditor { editorData | itemTemplateQuestions = Children.updateList itemQuestionList editorData.itemTemplateQuestions })
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


generateEvents : Seed -> Model -> ( Seed, Model, Cmd Msgs.Msg )
generateEvents seed model =
    let
        updateModel newSeed newModel a =
            ( newSeed, newModel, Cmd.none )
    in
    case getActiveEditor model of
        Just editor ->
            case editor of
                KMEditor data ->
                    withGenerateKMEditEvent seed model data updateModel

                TagEditor data ->
                    withGenerateTagEditEvent seed model data updateModel

                ChapterEditor data ->
                    withGenerateChapterEditEvent seed model data updateModel

                QuestionEditor data ->
                    withGenerateQuestionEditEvent seed model data updateModel

                AnswerEditor data ->
                    withGenerateAnswerEditEvent seed model data updateModel

                ReferenceEditor data ->
                    withGenerateReferenceEditEvent seed model data updateModel

                ExpertEditor data ->
                    withGenerateExpertEditEvent seed model data updateModel

        _ ->
            ( seed, model, Cmd.none )


withNoCmd : ( a, b ) -> ( a, b, Cmd msg )
withNoCmd ( a, b ) =
    ( a, b, Cmd.none )


setActiveEditor : (Msg -> Msgs.Msg) -> String -> Seed -> Model -> a -> ( Seed, Model, Cmd Msgs.Msg )
setActiveEditor wrapMsg uuid seed model _ =
    ( seed, { model | activeEditorUuid = Just uuid }, scrollTopCmd wrapMsg )


scrollTopCmd : (Msg -> Msgs.Msg) -> Cmd Msgs.Msg
scrollTopCmd wrapMsg =
    Ports.scrollToTop "editor-view"
