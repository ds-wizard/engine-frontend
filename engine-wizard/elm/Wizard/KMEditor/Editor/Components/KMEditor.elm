module Wizard.KMEditor.Editor.Components.KMEditor exposing
    ( DeleteModalState
    , EventMsg
    , Model
    , MoveModalState
    , Msg(..)
    , closeAllModals
    , initialModel
    , subscriptions
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h3, h5, i, img, label, li, small, span, strong, text, ul)
import Html.Attributes exposing (class, disabled, id, src, title)
import Html.Events exposing (onClick)
import Html.Keyed
import Maybe.Extra as Maybe
import Reorderable
import Set
import Shared.Copy as Copy
import Shared.Data.Event exposing (Event(..))
import Shared.Data.Event.AddAnswerEventData as AddAnswerEventData
import Shared.Data.Event.AddChapterEventData as AddChapterEventData
import Shared.Data.Event.AddChoiceEventData as AddChoiceEventData
import Shared.Data.Event.AddExpertEventData as AddExpertEventData
import Shared.Data.Event.AddIntegrationEventData as AddIntegrationEventData
import Shared.Data.Event.AddMetricEventData as AddMetricEventData
import Shared.Data.Event.AddPhaseEventData as AddPhaseEventData
import Shared.Data.Event.AddQuestionEventData as AddQuestionEventData
import Shared.Data.Event.AddReferenceEventData as AddReferenceEventData
import Shared.Data.Event.AddTagEventData as AddTagEventData
import Shared.Data.Event.CommonEventData exposing (CommonEventData)
import Shared.Data.Event.EditAnswerEventData as EditAnswerEventData
import Shared.Data.Event.EditChapterEventData as EditChapterEventData
import Shared.Data.Event.EditChoiceEventData as EditChoiceEventData
import Shared.Data.Event.EditEventSetters exposing (setAbbreviation, setAdvice, setAnnotations, setAnswerUuids, setChapterUuids, setChoiceUuids, setColor, setDescription, setEmail, setExpertUuids, setFollowUpUuids, setId, setIntegrationUuid, setIntegrationUuids, setItemTemplateQuestionUuids, setItemUrl, setLabel, setLogo, setMetricMeasures, setMetricUuids, setName, setPhaseUuids, setProps, setQuestionUuids, setReferenceUuids, setRequestBody, setRequestEmptySearch, setRequestHeaders, setRequestMethod, setRequestUrl, setRequiredPhaseUuid, setResponseItemId, setResponseItemTemplate, setResponseListField, setShortUuid, setTagUuids, setText, setTitle, setUrl, setValueType, setWidgetUrl)
import Shared.Data.Event.EditExpertEventData as EditExpertEventData
import Shared.Data.Event.EditIntegrationApiEventData as EditIntegrationApiEventData
import Shared.Data.Event.EditIntegrationEventData exposing (EditIntegrationEventData(..))
import Shared.Data.Event.EditIntegrationWidgetEventData as EditIntegrationWidgetEventData
import Shared.Data.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData
import Shared.Data.Event.EditMetricEventData as EditMetricEventData
import Shared.Data.Event.EditPhaseEventData as EditPhaseEventData
import Shared.Data.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Shared.Data.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData
import Shared.Data.Event.EditQuestionListEventData as EditQuestionListEventData
import Shared.Data.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData
import Shared.Data.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData
import Shared.Data.Event.EditQuestionValueEventData as EditQuestionValueEventData
import Shared.Data.Event.EditReferenceEventData exposing (EditReferenceEventData(..))
import Shared.Data.Event.EditReferenceResourcePageEventData as EditReferenceResourcePageEventData
import Shared.Data.Event.EditReferenceURLEventData as EditReferenceURLEventData
import Shared.Data.Event.EditTagEventData as EditTagEventData
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration(..))
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lgx, lx)
import Shared.Markdown as Markdown
import Shared.Utils exposing (compose2, dispatch, flip, httpMethodOptions, nilUuid)
import SplitPane
import String.Extra as String
import Uuid
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor.Breadcrumbs as Breadcrumbs
import Wizard.KMEditor.Editor.Components.KMEditor.Input as Input
import Wizard.KMEditor.Editor.Components.KMEditor.Tree as Tree
import Wizard.KMEditor.Editor.Components.KMEditor.TreeInput as TreeInput
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.Components.KMEditor"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.Components.KMEditor"



-- MODEL


type alias Model =
    { splitPane : SplitPane.State
    , markdownPreviews : List String
    , reorderableStates : Dict String Reorderable.State
    , deleteModalState : DeleteModalState
    , moveModalState : Maybe MoveModalState
    }


type DeleteModalState
    = ChapterState String
    | MetricState String
    | PhaseState String
    | TagState String
    | IntegrationState String
    | QuestionState String
    | AnswerState String
    | ChoiceState String
    | ReferenceState String
    | ExpertState String
    | Closed


type alias MoveModalState =
    { movingEntity : TreeInput.MovingEntity
    , movingUuid : String
    , treeInputModel : TreeInput.Model
    }


initialModel : Model
initialModel =
    { splitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.2 (Just ( 0.05, 0.7 )))
    , markdownPreviews = []
    , reorderableStates = Dict.empty
    , deleteModalState = Closed
    , moveModalState = Nothing
    }


closeAllModals : Model -> Model
closeAllModals model =
    { model | deleteModalState = Closed, moveModalState = Nothing }



-- UPDATE


type Msg
    = SplitPaneMsg SplitPane.Msg
    | SetFullscreen Bool
    | SetTreeOpen String Bool
    | ExpandAll
    | CollapseAll
    | CopyUuid String
    | ShowHideMarkdownPreview Bool String
    | ReorderableMsg String Reorderable.Msg
    | SetDeleteModalState DeleteModalState
    | OpenMoveModal TreeInput.MovingEntity String
    | MoveModalMsg TreeInput.Msg
    | CloseMoveModal


update : (Bool -> msg) -> Msg -> Model -> EditorBranch -> ( EditorBranch, Model, Cmd msg )
update setFullscreenMsg msg model editorBranch =
    case msg of
        SplitPaneMsg splitPaneMsg ->
            ( editorBranch, { model | splitPane = SplitPane.update splitPaneMsg model.splitPane }, Cmd.none )

        SetFullscreen fullscreen ->
            ( editorBranch, model, dispatch (setFullscreenMsg fullscreen) )

        SetTreeOpen entityUuid open ->
            ( EditorBranch.treeSetNodeOpen entityUuid open editorBranch, model, Cmd.none )

        ExpandAll ->
            ( EditorBranch.treeExpandAll editorBranch, model, Cmd.none )

        CollapseAll ->
            ( EditorBranch.treeCollapseAll editorBranch, model, Cmd.none )

        CopyUuid uuid ->
            ( editorBranch, model, Copy.copyToClipboard uuid )

        ShowHideMarkdownPreview visible field ->
            if visible then
                ( editorBranch, { model | markdownPreviews = field :: model.markdownPreviews }, Cmd.none )

            else
                ( editorBranch, { model | markdownPreviews = List.filter ((/=) field) model.markdownPreviews }, Cmd.none )

        ReorderableMsg field reorderableMsg ->
            let
                reorderableState =
                    Dict.get field model.reorderableStates
                        |> Maybe.withDefault Reorderable.initialState
                        |> Reorderable.update reorderableMsg
            in
            ( editorBranch
            , { model | reorderableStates = Dict.insert field reorderableState model.reorderableStates }
            , Cmd.none
            )

        SetDeleteModalState deleteModalState ->
            ( editorBranch, { model | deleteModalState = deleteModalState }, Cmd.none )

        OpenMoveModal movingEntity movingUuid ->
            ( editorBranch
            , { model
                | moveModalState =
                    Just
                        { movingEntity = movingEntity
                        , movingUuid = movingUuid
                        , treeInputModel = TreeInput.initialModel (Set.fromList editorBranch.openNodeUuids)
                        }
              }
            , Cmd.none
            )

        MoveModalMsg moveModalMsg ->
            ( editorBranch
            , { model
                | moveModalState =
                    case model.moveModalState of
                        Just oldState ->
                            Just { oldState | treeInputModel = TreeInput.update moveModalMsg editorBranch oldState.treeInputModel }

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        CloseMoveModal ->
            ( editorBranch, { model | moveModalState = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        toReorderableSubscriptions ( field, state ) =
            Sub.map (ReorderableMsg field) <|
                Reorderable.subscriptions state

        reorderableSubscriptions =
            List.map toReorderableSubscriptions <|
                Dict.toList model.reorderableStates

        splitPaneSubscriptions =
            Sub.map SplitPaneMsg <|
                SplitPane.subscriptions model.splitPane
    in
    Sub.batch <|
        splitPaneSubscriptions
            :: reorderableSubscriptions



-- VIEW


type alias EventMsg msg =
    String -> Maybe String -> (CommonEventData -> Event) -> msg


view : AppState -> (Msg -> msg) -> EventMsg msg -> Model -> List Integration -> EditorBranch -> Html msg
view appState wrapMsg eventMsg model integrationPrefabs editorBranch =
    let
        ( expandIcon, expandMsg ) =
            if AppState.isFullscreen appState then
                ( faSet "questionnaire.shrink" appState, wrapMsg <| SetFullscreen False )

            else
                ( faSet "questionnaire.expand" appState, wrapMsg <| SetFullscreen True )

        treeViewProps =
            { expandAll = wrapMsg ExpandAll
            , collapseAll = wrapMsg CollapseAll
            , setTreeOpen = compose2 wrapMsg SetTreeOpen
            }

        splitPaneConfig =
            SplitPane.createViewConfig
                { toMsg = wrapMsg << SplitPaneMsg
                , customSplitter = Nothing
                }
    in
    div [ class "KMEditor__Editor__KMEditor", dataCy "km-editor_km" ]
        [ div [ class "editor-breadcrumbs" ]
            [ Breadcrumbs.view appState editorBranch
            , a [ class "breadcrumb-button", onClick expandMsg ] [ expandIcon ]
            ]
        , SplitPane.view splitPaneConfig
            (Tree.view treeViewProps appState editorBranch)
            (viewEditor appState wrapMsg eventMsg model integrationPrefabs editorBranch)
            model.splitPane
        , deleteModal appState wrapMsg eventMsg editorBranch model.deleteModalState
        , moveModal appState wrapMsg eventMsg editorBranch model.moveModalState
        ]


type alias EditorConfig msg =
    { appState : AppState
    , wrapMsg : Msg -> msg
    , eventMsg : EventMsg msg
    , model : Model
    , editorBranch : EditorBranch
    , integrationPrefabs : List Integration
    }


viewEditor : AppState -> (Msg -> msg) -> EventMsg msg -> Model -> List Integration -> EditorBranch -> Html msg
viewEditor appState wrapMsg eventMsg model integrationPrefabs editorBranch =
    let
        km =
            editorBranch.branch.knowledgeModel

        kmUuid =
            Uuid.toString km.uuid

        editorConfig =
            { appState = appState
            , wrapMsg = wrapMsg
            , eventMsg = eventMsg
            , model = model
            , editorBranch = editorBranch
            , integrationPrefabs = integrationPrefabs
            }

        kmEditor =
            if editorBranch.activeUuid == kmUuid then
                Just <| viewKnowledgeModelEditor editorConfig editorBranch.branch.knowledgeModel

            else
                Nothing

        createEditor viewEntityEditor getEntity =
            Maybe.map (viewEntityEditor editorConfig) (getEntity editorBranch.activeUuid km)

        chapterEditor =
            createEditor viewChapterEditor KnowledgeModel.getChapter

        questionEditor =
            createEditor viewQuestionEditor KnowledgeModel.getQuestion

        metricEditor =
            createEditor viewMetricEditor KnowledgeModel.getMetric

        phaseEditor =
            createEditor viewPhaseEditor KnowledgeModel.getPhase

        tagEditor =
            createEditor viewTagEditor KnowledgeModel.getTag

        integrationEditor =
            createEditor viewIntegrationEditor KnowledgeModel.getIntegration

        answerEditor =
            createEditor viewAnswerEditor KnowledgeModel.getAnswer

        choiceEditor =
            createEditor viewChoiceEditor KnowledgeModel.getChoice

        referenceEditor =
            createEditor viewReferenceEditor KnowledgeModel.getReference

        expertEditor =
            createEditor viewExpertEditor KnowledgeModel.getExpert

        emptyEditor =
            ( "empty", viewEmptyEditor appState )

        editorContent =
            kmEditor
                |> Maybe.orElse chapterEditor
                |> Maybe.orElse questionEditor
                |> Maybe.orElse metricEditor
                |> Maybe.orElse phaseEditor
                |> Maybe.orElse tagEditor
                |> Maybe.orElse integrationEditor
                |> Maybe.orElse answerEditor
                |> Maybe.orElse choiceEditor
                |> Maybe.orElse referenceEditor
                |> Maybe.orElse expertEditor
                |> Maybe.map (Tuple.pair editorBranch.activeUuid)
                |> Maybe.withDefault emptyEditor
    in
    Html.Keyed.node "div"
        [ class "editor-form-view", id "editor-view" ]
        [ editorContent ]


viewKnowledgeModelEditor : EditorConfig msg -> KnowledgeModel -> Html msg
viewKnowledgeModelEditor { appState, wrapMsg, eventMsg, model, editorBranch } km =
    let
        kmUuid =
            Uuid.toString km.uuid

        kmEditorTitle =
            editorTitle appState
                { title = lg "knowledgeModel" appState
                , uuid = kmUuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Nothing
                , mbMovingEntity = Nothing
                }

        createEditEvent map value =
            EditKnowledgeModelEventData.init
                |> map value
                |> EditKnowledgeModelEvent
                |> eventMsg nilUuid (Just kmUuid)

        addChapterEvent =
            AddChapterEvent AddChapterEventData.init
                |> eventMsg kmUuid Nothing

        addMetricEvent =
            AddMetricEvent AddMetricEventData.init
                |> eventMsg kmUuid Nothing

        addPhaseEvent =
            AddPhaseEvent AddPhaseEventData.init
                |> eventMsg kmUuid Nothing

        addTagEvent =
            AddTagEvent AddTagEventData.init
                |> eventMsg kmUuid Nothing

        addIntegrationEvent =
            AddIntegrationEvent AddIntegrationEventData.init
                |> eventMsg kmUuid Nothing

        chaptersInput =
            Input.reorderable appState
                { name = "chapters"
                , label = lg "chapters" appState
                , items = EditorBranch.filterDeleted editorBranch km.chapterUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setChapterUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getChapterName km
                , untitledLabel = lg "chapter.untitled" appState
                , addChildLabel = l_ "knowledgeModel.addChapter" appState
                , addChildMsg = addChapterEvent
                , addChildDataCy = "chapter"
                }

        metricsInput =
            Input.reorderable appState
                { name = "metrics"
                , label = lg "metrics" appState
                , items = EditorBranch.filterDeleted editorBranch km.metricUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setMetricUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getMetricName km
                , untitledLabel = lg "metric.untitled" appState
                , addChildLabel = l_ "knowledgeModel.addMetric" appState
                , addChildMsg = addMetricEvent
                , addChildDataCy = "metric"
                }

        phasesInput =
            Input.reorderable appState
                { name = "phases"
                , label = lg "phases" appState
                , items = EditorBranch.filterDeleted editorBranch km.phaseUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setPhaseUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getPhaseName km
                , untitledLabel = lg "phase.untitled" appState
                , addChildLabel = l_ "knowledgeModel.addPhase" appState
                , addChildMsg = addPhaseEvent
                , addChildDataCy = "phase"
                }

        tagsInput =
            Input.reorderable appState
                { name = "tags"
                , label = lg "tags" appState
                , items = EditorBranch.filterDeleted editorBranch km.tagUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setTagUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getTagName km
                , untitledLabel = lg "tag.untitled" appState
                , addChildLabel = l_ "knowledgeModel.addTag" appState
                , addChildMsg = addTagEvent
                , addChildDataCy = "tag"
                }

        integrationsInput =
            Input.reorderable appState
                { name = "integrations"
                , label = lg "integrations" appState
                , items = EditorBranch.filterDeleted editorBranch km.integrationUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setIntegrationUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getIntegrationName km
                , untitledLabel = lg "integration.untitled" appState
                , addChildLabel = l_ "knowledgeModel.addIntegration" appState
                , addChildMsg = addIntegrationEvent
                , addChildDataCy = "integration"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = km.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("km-" ++ kmUuid)
        [ kmEditorTitle
        , chaptersInput
        , metricsInput
        , phasesInput
        , tagsInput
        , integrationsInput
        , annotationsInput
        ]


viewChapterEditor : EditorConfig msg -> Chapter -> Html msg
viewChapterEditor { appState, wrapMsg, eventMsg, model, editorBranch } chapter =
    let
        parentUuid =
            EditorBranch.getParentUuid chapter.uuid editorBranch

        createEditEvent map value =
            EditChapterEventData.init
                |> map value
                |> EditChapterEvent
                |> eventMsg parentUuid (Just chapter.uuid)

        questionAddEvent =
            AddQuestionEventData.init
                |> AddQuestionEvent
                |> eventMsg chapter.uuid Nothing

        chapterEditorTitle =
            editorTitle appState
                { title = lg "chapter" appState
                , uuid = chapter.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just ChapterState
                , mbMovingEntity = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = lg "chapter.title" appState
                , value = chapter.title
                , onInput = createEditEvent setTitle
                }

        textInput =
            Input.markdown appState
                { name = "text"
                , label = lg "chapter.text" appState
                , value = Maybe.withDefault "" chapter.text
                , onInput = createEditEvent setText << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = chapter.uuid
                , markdownPreviews = model.markdownPreviews
                }

        questionsInput =
            Input.reorderable appState
                { name = "questions"
                , label = lg "questions" appState
                , items = EditorBranch.filterDeleted editorBranch chapter.questionUuids
                , entityUuid = chapter.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setQuestionUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getQuestionName editorBranch.branch.knowledgeModel
                , untitledLabel = lg "question.untitled" appState
                , addChildLabel = l_ "chapter.addQuestion" appState
                , addChildMsg = questionAddEvent
                , addChildDataCy = "question"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = chapter.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("chapter-" ++ chapter.uuid)
        [ chapterEditorTitle
        , titleInput
        , textInput
        , questionsInput
        , annotationsInput
        ]


viewQuestionEditor : EditorConfig msg -> Question -> Html msg
viewQuestionEditor { appState, wrapMsg, eventMsg, model, editorBranch } question =
    let
        questionUuid =
            Question.getUuid question

        parentUuid =
            EditorBranch.getParentUuid questionUuid editorBranch

        createEditEvent setOptions setList setValue setIntegration setMultiChoice value =
            eventMsg parentUuid (Just questionUuid) <|
                EditQuestionEvent <|
                    case question of
                        OptionsQuestion _ _ ->
                            EditQuestionOptionsEventData.init
                                |> setOptions value
                                |> EditQuestionOptionsEvent

                        ListQuestion _ _ ->
                            EditQuestionListEventData.init
                                |> setList value
                                |> EditQuestionListEvent

                        ValueQuestion _ _ ->
                            EditQuestionValueEventData.init
                                |> setValue value
                                |> EditQuestionValueEvent

                        IntegrationQuestion _ _ ->
                            EditQuestionIntegrationEventData.init
                                |> setIntegration value
                                |> EditQuestionIntegrationEvent

                        MultiChoiceQuestion _ _ ->
                            EditQuestionMultiChoiceEventData.init
                                |> setMultiChoice value
                                |> EditQuestionMultiChoiceEvent

        onTypeChange value =
            eventMsg parentUuid (Just questionUuid) <|
                case value of
                    "List" ->
                        EditQuestionListEventData.init
                            |> EditQuestionListEvent
                            |> EditQuestionEvent

                    "Value" ->
                        EditQuestionValueEventData.init
                            |> setValueType QuestionValueType.StringQuestionValueType
                            |> EditQuestionValueEvent
                            |> EditQuestionEvent

                    "Integration" ->
                        EditQuestionIntegrationEventData.init
                            |> setIntegrationUuid (Uuid.toString Uuid.nil)
                            |> EditQuestionIntegrationEvent
                            |> EditQuestionEvent

                    "MultiChoice" ->
                        EditQuestionMultiChoiceEventData.init
                            |> EditQuestionMultiChoiceEvent
                            |> EditQuestionEvent

                    _ ->
                        EditQuestionOptionsEventData.init
                            |> EditQuestionOptionsEvent
                            |> EditQuestionEvent

        addReferenceEvent =
            AddReferenceEventData.init
                |> AddReferenceEvent
                |> eventMsg questionUuid Nothing

        expertAddEvent =
            AddExpertEventData.init
                |> AddExpertEvent
                |> eventMsg questionUuid Nothing

        questionTypeOptions =
            [ ( "Options", lg "questionType.options" appState )
            , ( "List", lg "questionType.list" appState )
            , ( "Value", lg "questionType.value" appState )
            , ( "Integration", lg "questionType.integration" appState )
            , ( "MultiChoice", lg "questionType.multiChoice" appState )
            ]

        requiredPhaseUuidOptions =
            KnowledgeModel.getPhases editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith .uuid editorBranch
                |> List.map (\phase -> ( phase.uuid, phase.title ))
                |> (::) ( "", l_ "question.phase.never" appState )

        questionEditorTitle =
            editorTitle appState
                { title = lg "question" appState
                , uuid = Question.getUuid question
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just QuestionState
                , mbMovingEntity = Just TreeInput.MovingQuestion
                }

        typeInput =
            Input.select
                { name = "type"
                , label = lg "question.type" appState
                , value = Question.getTypeString question
                , options = questionTypeOptions
                , onChange = onTypeChange
                }

        titleInput =
            Input.string
                { name = "title"
                , label = lg "question.title" appState
                , value = Question.getTitle question
                , onInput = createEditEvent setTitle setTitle setTitle setTitle setTitle
                }

        textInput =
            Input.markdown appState
                { name = "text"
                , label = lg "question.text" appState
                , value = Maybe.withDefault "" (Question.getText question)
                , onInput = createEditEvent setText setText setText setText setText << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = questionUuid
                , markdownPreviews = model.markdownPreviews
                }

        requiredPhaseUuidInput =
            Input.select
                { name = "requiredPhaseUuid"
                , label = lg "question.requiredLevel" appState
                , value = String.fromMaybe <| Question.getRequiredPhaseUuid question
                , options = requiredPhaseUuidOptions
                , onChange = createEditEvent setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid << String.toMaybe
                }

        tagUuidsInput =
            Input.tags appState
                { label = lg "tags" appState
                , tags = EditorBranch.filterDeletedWith .uuid editorBranch <| KnowledgeModel.getTags editorBranch.branch.knowledgeModel
                , selected = Question.getTagUuids question
                , onChange = createEditEvent setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids
                }

        referencesInput =
            Input.reorderable appState
                { name = "references"
                , label = lg "references" appState
                , items = EditorBranch.filterDeleted editorBranch <| Question.getReferenceUuids question
                , entityUuid = questionUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getReferenceName editorBranch.branch.knowledgeModel
                , untitledLabel = lg "reference.untitled" appState
                , addChildLabel = l_ "question.addReference" appState
                , addChildMsg = addReferenceEvent
                , addChildDataCy = "reference"
                }

        expertsInput =
            Input.reorderable appState
                { name = "experts"
                , label = lg "experts" appState
                , items = EditorBranch.filterDeleted editorBranch <| Question.getExpertUuids question
                , entityUuid = questionUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getExpertName editorBranch.branch.knowledgeModel
                , untitledLabel = lg "expert.untitled" appState
                , addChildLabel = l_ "question.addExpert" appState
                , addChildMsg = expertAddEvent
                , addChildDataCy = "expert"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = Question.getAnnotations question
                , onEdit = createEditEvent setAnnotations setAnnotations setAnnotations setAnnotations setAnnotations
                }

        questionTypeInputs =
            case question of
                OptionsQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionOptionsEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionOptionsEvent)
                                |> eventMsg parentUuid (Just questionUuid)

                        addAnswerEvent =
                            AddAnswerEventData.init
                                |> AddAnswerEvent
                                |> eventMsg questionUuid Nothing

                        answersInput =
                            Input.reorderable appState
                                { name = "answers"
                                , label = lg "answers" appState
                                , items = EditorBranch.filterDeleted editorBranch <| Question.getAnswerUuids question
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setAnswerUuids
                                , getRoute = editorRoute editorBranch
                                , getName = KnowledgeModel.getAnswerName editorBranch.branch.knowledgeModel
                                , untitledLabel = lg "answer.untitled" appState
                                , addChildLabel = l_ "question.addAnswer" appState
                                , addChildMsg = addAnswerEvent
                                , addChildDataCy = "answer"
                                }
                    in
                    [ answersInput ]

                ListQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionListEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionListEvent)
                                |> eventMsg parentUuid (Just questionUuid)

                        addItemTemplateQuestionEvent =
                            AddQuestionEventData.init
                                |> AddQuestionEvent
                                |> eventMsg questionUuid Nothing

                        itemTemplateQuestionsInput =
                            Input.reorderable appState
                                { name = "questions"
                                , label = lg "questions" appState
                                , items = EditorBranch.filterDeleted editorBranch <| Question.getItemTemplateQuestionUuids question
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setItemTemplateQuestionUuids
                                , getRoute = editorRoute editorBranch
                                , getName = KnowledgeModel.getQuestionName editorBranch.branch.knowledgeModel
                                , untitledLabel = lg "question.untitled" appState
                                , addChildLabel = l_ "question.addItemTemplateQuestion" appState
                                , addChildMsg = addItemTemplateQuestionEvent
                                , addChildDataCy = "question"
                                }
                    in
                    [ div [ class "form-group" ]
                        [ div [ class "card card-border-light card-item-template" ]
                            [ div [ class "card-header" ]
                                [ lgx "question.itemTemplate" appState ]
                            , div [ class "card-body" ]
                                [ itemTemplateQuestionsInput ]
                            ]
                        ]
                    ]

                ValueQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionValueEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionValueEvent)
                                |> eventMsg parentUuid (Just questionUuid)

                        questionValueTypeOptions =
                            [ ( "StringQuestionValueType", lg "questionValueType.string" appState )
                            , ( "NumberQuestionValueType", lg "questionValueType.number" appState )
                            , ( "DateQuestionValueType", lg "questionValueType.date" appState )
                            , ( "DateTimeQuestionValueType", lg "questionValueType.datetime" appState )
                            , ( "TimeQuestionValueType", lg "questionValueType.time" appState )
                            , ( "TextQuestionValueType", lg "questionValueType.text" appState )
                            , ( "EmailQuestionValueType", lg "questionValueType.email" appState )
                            , ( "UrlQuestionValueType", lg "questionValueType.url" appState )
                            , ( "ColorQuestionValueType", lg "questionValueType.color" appState )
                            ]

                        valueTypeInput =
                            Input.select
                                { name = "valueType"
                                , label = lg "questionValueType" appState
                                , value = QuestionValueType.toString <| Maybe.withDefault QuestionValueType.default <| Question.getValueType question
                                , options = questionValueTypeOptions
                                , onChange = createTypeEditEvent setValueType << Maybe.withDefault QuestionValueType.default << QuestionValueType.fromString
                                }
                    in
                    [ valueTypeInput ]

                IntegrationQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionIntegrationEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionIntegrationEvent)
                                |> eventMsg parentUuid (Just questionUuid)

                        integrationUuidOptions =
                            KnowledgeModel.getIntegrations editorBranch.branch.knowledgeModel
                                |> EditorBranch.filterDeletedWith Integration.getUuid editorBranch
                                |> List.map (\integration -> ( Integration.getUuid integration, String.withDefault (lg "integration.untitled" appState) (Integration.getName integration) ))
                                |> (::) ( Uuid.toString Uuid.nil, l_ "question.integration.select" appState )

                        selectedIntegrationProps =
                            Question.getIntegrationUuid question
                                |> Maybe.andThen (flip KnowledgeModel.getIntegration editorBranch.branch.knowledgeModel)
                                |> Maybe.unwrap [] Integration.getProps

                        onPropInput prop value =
                            let
                                props =
                                    Question.getProps question
                                        |> Maybe.unwrap Dict.empty (Dict.insert prop value)
                            in
                            createTypeEditEvent setProps props

                        propsInput =
                            if List.length selectedIntegrationProps > 0 then
                                let
                                    propInput prop =
                                        Input.string
                                            { name = "props-" ++ prop
                                            , label = prop
                                            , value = String.fromMaybe <| Question.getPropValue prop question
                                            , onInput = onPropInput prop
                                            }
                                in
                                div [ class "form-group" ]
                                    [ div [ class "card card-border-light" ]
                                        [ div [ class "card-header" ] [ lx_ "question.integration.configuration" appState ]
                                        , div [ class "card-body" ]
                                            (List.map propInput selectedIntegrationProps)
                                        ]
                                    ]

                            else
                                emptyNode

                        integrationUuidInput =
                            Input.select
                                { name = "integrationUuid"
                                , label = lg "integration" appState
                                , value = String.fromMaybe <| Question.getIntegrationUuid question
                                , options = integrationUuidOptions
                                , onChange = createTypeEditEvent setIntegrationUuid
                                }
                    in
                    [ integrationUuidInput
                    , propsInput
                    ]

                MultiChoiceQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionMultiChoiceEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionMultiChoiceEvent)
                                |> eventMsg parentUuid (Just questionUuid)

                        addChoiceEvent =
                            AddChoiceEventData.init
                                |> AddChoiceEvent
                                |> eventMsg questionUuid Nothing

                        choicesInput =
                            Input.reorderable appState
                                { name = "choices"
                                , label = lg "choices" appState
                                , items = EditorBranch.filterDeleted editorBranch <| Question.getChoiceUuids question
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setChoiceUuids
                                , getRoute = editorRoute editorBranch
                                , getName = KnowledgeModel.getChoiceName editorBranch.branch.knowledgeModel
                                , untitledLabel = lg "choice.untitled" appState
                                , addChildLabel = l_ "question.addChoice" appState
                                , addChildMsg = addChoiceEvent
                                , addChildDataCy = "choice"
                                }
                    in
                    [ choicesInput ]
    in
    editor ("question-" ++ questionUuid)
        ([ questionEditorTitle
         , typeInput
         , titleInput
         , textInput
         , requiredPhaseUuidInput
         , tagUuidsInput
         ]
            ++ questionTypeInputs
            ++ [ referencesInput
               , expertsInput
               , annotationsInput
               ]
        )


viewMetricEditor : EditorConfig msg -> Metric -> Html msg
viewMetricEditor { appState, wrapMsg, eventMsg, model, editorBranch } metric =
    let
        parentUuid =
            EditorBranch.getParentUuid metric.uuid editorBranch

        createEditEvent map value =
            EditMetricEventData.init
                |> map value
                |> EditMetricEvent
                |> eventMsg parentUuid (Just metric.uuid)

        metricEditorTitle =
            editorTitle appState
                { title = lg "metric" appState
                , uuid = metric.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just MetricState
                , mbMovingEntity = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = lg "metric.title" appState
                , value = metric.title
                , onInput = createEditEvent setTitle
                }

        abbreviationInput =
            Input.string
                { name = "abbreviation"
                , label = lg "metric.abbreviation" appState
                , value = Maybe.withDefault "" metric.abbreviation
                , onInput = createEditEvent setAbbreviation << String.toMaybe
                }

        descriptionInput =
            Input.markdown appState
                { name = "description"
                , label = lg "metric.description" appState
                , value = Maybe.withDefault "" metric.description
                , onInput = createEditEvent setDescription << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = metric.uuid
                , markdownPreviews = model.markdownPreviews
                }

        annotationsInput =
            Input.annotations appState
                { annotations = metric.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("metric-" ++ metric.uuid)
        [ metricEditorTitle
        , titleInput
        , abbreviationInput
        , descriptionInput
        , annotationsInput
        ]


viewPhaseEditor : EditorConfig msg -> Phase -> Html msg
viewPhaseEditor { appState, wrapMsg, eventMsg, model, editorBranch } phase =
    let
        parentUuid =
            EditorBranch.getParentUuid phase.uuid editorBranch

        createEditEvent map value =
            EditPhaseEventData.init
                |> map value
                |> EditPhaseEvent
                |> eventMsg parentUuid (Just phase.uuid)

        phaseEditorTitle =
            editorTitle appState
                { title = lg "phase" appState
                , uuid = phase.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just PhaseState
                , mbMovingEntity = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = lg "phase.title" appState
                , value = phase.title
                , onInput = createEditEvent setTitle
                }

        descriptionInput =
            Input.markdown appState
                { name = "description"
                , label = lg "phase.description" appState
                , value = Maybe.withDefault "" phase.description
                , onInput = createEditEvent setDescription << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = phase.uuid
                , markdownPreviews = model.markdownPreviews
                }

        annotationsInput =
            Input.annotations appState
                { annotations = phase.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("phase-" ++ phase.uuid)
        [ phaseEditorTitle
        , titleInput
        , descriptionInput
        , annotationsInput
        ]


viewTagEditor : EditorConfig msg -> Tag -> Html msg
viewTagEditor { appState, wrapMsg, eventMsg, editorBranch } tag =
    let
        parentUuid =
            EditorBranch.getParentUuid tag.uuid editorBranch

        createEditEvent map value =
            EditTagEventData.init
                |> map value
                |> EditTagEvent
                |> eventMsg parentUuid (Just tag.uuid)

        tagEditorTitle =
            editorTitle appState
                { title = lg "tag" appState
                , uuid = tag.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just TagState
                , mbMovingEntity = Nothing
                }

        nameInput =
            Input.string
                { name = "name"
                , label = lg "tag.name" appState
                , value = tag.name
                , onInput = createEditEvent setName
                }

        descriptionInput =
            Input.textarea
                { name = "description"
                , label = lg "tag.description" appState
                , value = Maybe.withDefault "" tag.description
                , onInput = createEditEvent setDescription << String.toMaybe
                }

        colorInput =
            Input.color
                { name = "color"
                , label = lg "tag.color" appState
                , value = tag.color
                , onInput = createEditEvent setColor
                }

        annotationsInput =
            Input.annotations appState
                { annotations = tag.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("tag-" ++ tag.uuid)
        [ tagEditorTitle
        , nameInput
        , descriptionInput
        , colorInput
        , annotationsInput
        ]


viewIntegrationEditor : EditorConfig msg -> Integration -> Html msg
viewIntegrationEditor { appState, wrapMsg, eventMsg, integrationPrefabs, editorBranch } integration =
    let
        integrationUuid =
            Integration.getUuid integration

        parentUuid =
            EditorBranch.getParentUuid integrationUuid editorBranch

        createEditEvent setApi setWidget value =
            eventMsg parentUuid (Just integrationUuid) <|
                EditIntegrationEvent <|
                    case integration of
                        ApiIntegration _ _ ->
                            EditIntegrationApiEventData.init
                                |> setApi value
                                |> EditIntegrationApiEvent

                        WidgetIntegration _ _ ->
                            EditIntegrationWidgetEventData.init
                                |> setWidget value
                                |> EditIntegrationWidgetEvent

        createEditEventFromPrefab integrationPrefab =
            eventMsg parentUuid (Just integrationUuid) <|
                EditIntegrationEvent <|
                    case integrationPrefab of
                        ApiIntegration commonData apiData ->
                            EditIntegrationApiEvent
                                { id = EventField.create commonData.id True
                                , name = EventField.create commonData.name True
                                , props = EventField.create commonData.props True
                                , logo = EventField.create commonData.logo True
                                , itemUrl = EventField.create commonData.itemUrl True
                                , annotations = EventField.create commonData.annotations True
                                , requestMethod = EventField.create apiData.requestMethod True
                                , requestUrl = EventField.create apiData.requestUrl True
                                , requestHeaders = EventField.create apiData.requestHeaders True
                                , requestBody = EventField.create apiData.requestBody True
                                , requestEmptySearch = EventField.create apiData.requestEmptySearch True
                                , responseListField = EventField.create apiData.responseListField True
                                , responseItemId = EventField.create apiData.responseItemId True
                                , responseItemTemplate = EventField.create apiData.responseItemTemplate True
                                }

                        WidgetIntegration commonData widgetData ->
                            EditIntegrationWidgetEvent
                                { id = EventField.create commonData.id True
                                , name = EventField.create commonData.name True
                                , props = EventField.create commonData.props True
                                , logo = EventField.create commonData.logo True
                                , itemUrl = EventField.create commonData.itemUrl True
                                , annotations = EventField.create commonData.annotations True
                                , widgetUrl = EventField.create widgetData.widgetUrl True
                                }

        onTypeChange value =
            eventMsg parentUuid (Just integrationUuid) <|
                case value of
                    "Widget" ->
                        EditIntegrationWidgetEventData.init
                            |> EditIntegrationWidgetEvent
                            |> EditIntegrationEvent

                    _ ->
                        EditIntegrationApiEventData.init
                            |> EditIntegrationApiEvent
                            |> EditIntegrationEvent

        integrationTypeOptions =
            [ ( "Api", lg "integrationType.api" appState )
            , ( "Widget", lg "integrationType.widget" appState )
            ]

        integrationEditorTitle =
            editorTitle appState
                { title = lg "integration" appState
                , uuid = integrationUuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just IntegrationState
                , mbMovingEntity = Nothing
                }

        typeInput =
            Input.select
                { name = "type"
                , label = lg "integration.type" appState
                , value = Integration.getTypeString integration
                , options = integrationTypeOptions
                , onChange = onTypeChange
                }

        idInput =
            Input.string
                { name = "id"
                , label = lg "integration.id" appState
                , value = Integration.getId integration
                , onInput = createEditEvent setId setId
                }

        nameInput =
            Input.string
                { name = "name"
                , label = lg "integration.name" appState
                , value = Integration.getName integration
                , onInput = createEditEvent setName setName
                }

        logoUrlInput =
            Input.string
                { name = "logo"
                , label = lg "integration.logo" appState
                , value = Integration.getLogo integration
                , onInput = createEditEvent setLogo setLogo
                }

        propsInput =
            Input.props appState
                { label = lg "integration.props" appState
                , values = Integration.getProps integration
                , onChange = createEditEvent setProps setProps
                }

        itemUrl =
            Input.string
                { name = "itemUrl"
                , label = lg "integration.itemUrl" appState
                , value = Integration.getItemUrl integration
                , onInput = createEditEvent setItemUrl setItemUrl
                }

        annotationsInput =
            Input.annotations appState
                { annotations = Integration.getAnnotations integration
                , onEdit = createEditEvent setAnnotations setAnnotations
                }

        integrationTypeInputs =
            case integration of
                ApiIntegration _ data ->
                    let
                        createTypeEditEvent map value =
                            EditIntegrationApiEventData.init
                                |> map value
                                |> (EditIntegrationEvent << EditIntegrationApiEvent)
                                |> eventMsg parentUuid (Just integrationUuid)

                        requestUrlInput =
                            Input.string
                                { name = "requestUrl"
                                , label = lg "integration.request.url" appState
                                , value = data.requestUrl
                                , onInput = createTypeEditEvent setRequestUrl
                                }

                        requestMethodInput =
                            Input.select
                                { name = "requestMethod"
                                , label = lg "integration.request.method" appState
                                , value = data.requestMethod
                                , options = httpMethodOptions
                                , onChange = createTypeEditEvent setRequestMethod
                                }

                        requestHeadersInput =
                            Input.headers appState
                                { label = lg "integration.request.headers" appState
                                , headers = data.requestHeaders
                                , onEdit = createTypeEditEvent setRequestHeaders
                                }

                        requestBodyInput =
                            Input.textarea
                                { name = "requestBody"
                                , label = lg "integration.request.body" appState
                                , value = data.requestBody
                                , onInput = createTypeEditEvent setRequestBody
                                }

                        requestEmptySearchInput =
                            Input.checkbox
                                { name = "requestEmptySearch"
                                , label = lg "integration.request.emptySearch" appState
                                , value = data.requestEmptySearch
                                , onInput = createTypeEditEvent setRequestEmptySearch
                                }

                        responseItemId =
                            Input.string
                                { name = "responseItemId"
                                , label = lg "integration.response.idField" appState
                                , value = data.responseItemId
                                , onInput = createTypeEditEvent setResponseItemId
                                }

                        responseListFieldInput =
                            Input.string
                                { name = "responseListField"
                                , label = lg "integration.response.listField" appState
                                , value = data.responseListField
                                , onInput = createTypeEditEvent setResponseListField
                                }

                        responseItemTemplate =
                            Input.textarea
                                { name = "responseItemTemplate"
                                , label = lg "integration.response.itemTemplate" appState
                                , value = data.responseItemTemplate
                                , onInput = createTypeEditEvent setResponseItemTemplate
                                }
                    in
                    [ div [ class "card card-border-light mb-5" ]
                        [ div [ class "card-header" ] [ lgx "integration.request" appState ]
                        , div [ class "card-body" ]
                            [ Markdown.toHtml [ class "alert alert-info mb-5" ] (l_ "integration.request.description" appState)
                            , requestUrlInput
                            , FormExtra.mdAfter (l_ "integration.requestUrl.description" appState)
                            , requestMethodInput
                            , requestHeadersInput
                            , requestBodyInput
                            , requestEmptySearchInput
                            , FormExtra.mdAfter (l_ "integration.requestEmptySearch.description" appState)
                            ]
                        ]
                    , div [ class "card card-border-light mb-5" ]
                        [ div [ class "card-header" ] [ lgx "integration.response" appState ]
                        , div [ class "card-body" ]
                            [ Markdown.toHtml [ class "alert alert-info mb-5" ] (l_ "integration.response.description" appState)
                            , responseListFieldInput
                            , FormExtra.mdAfter (l_ "integration.responseListField.description" appState)
                            , responseItemId
                            , FormExtra.mdAfter (l_ "integration.responseItemId.description" appState)
                            , responseItemTemplate
                            , FormExtra.mdAfter (l_ "integration.responseItemTemplate.description" appState)
                            ]
                        ]
                    , itemUrl
                    , FormExtra.mdAfter (l_ "integration.itemUrl.api.description" appState)
                    ]

                WidgetIntegration _ data ->
                    let
                        createTypeEditEvent map value =
                            EditIntegrationWidgetEventData.init
                                |> map value
                                |> (EditIntegrationEvent << EditIntegrationWidgetEvent)
                                |> eventMsg parentUuid (Just integrationUuid)

                        widgetUrlInput =
                            Input.string
                                { name = "widgetUrl"
                                , label = lg "integration.widgetUrl" appState
                                , value = data.widgetUrl
                                , onInput = createTypeEditEvent setWidgetUrl
                                }
                    in
                    [ widgetUrlInput
                    , FormExtra.mdAfter (l_ "integration.widgetUrl.description" appState)
                    , itemUrl
                    , FormExtra.mdAfter (l_ "integration.itemUrl.widget.description" appState)
                    ]

        viewQuestionLink question =
            li []
                [ linkTo appState
                    (editorRoute editorBranch (Question.getUuid question))
                    []
                    [ text (Question.getTitle question) ]
                ]

        wrapQuestionsWithIntegration questions =
            if List.isEmpty questions then
                div [] [ i [] [ lx_ "integration.questions.noQuestions" appState ] ]

            else
                ul [] questions

        questionsWithIntegration =
            KnowledgeModel.getAllQuestions editorBranch.branch.knowledgeModel
                |> List.filter ((==) (Just integrationUuid) << Question.getIntegrationUuid)
                |> List.sortBy Question.getTitle
                |> List.map viewQuestionLink
                |> wrapQuestionsWithIntegration

        prefabsView =
            if (not << List.isEmpty) integrationPrefabs && EditorBranch.isEmptyIntegrationEditorUuid integrationUuid editorBranch then
                let
                    viewLogo i =
                        let
                            logo =
                                Integration.getLogo i
                        in
                        if String.isEmpty logo then
                            faSet "km.integration" appState

                        else
                            img [ src logo ] []

                    viewIntegrationButton i =
                        li []
                            [ a [ onClick (createEditEventFromPrefab i) ]
                                [ viewLogo i
                                , span [] [ text (Integration.getName i) ]
                                ]
                            ]
                in
                div [ class "prefab-selection" ]
                    [ strong [] [ lx_ "integration.quickSetup" appState ]
                    , ul [] (List.map viewIntegrationButton <| List.sortBy Integration.getName integrationPrefabs)
                    ]

            else
                emptyNode
    in
    editor ("integration-" ++ integrationUuid)
        ([ integrationEditorTitle
         , prefabsView
         , typeInput
         , idInput
         , FormExtra.mdAfter (l_ "integration.id.description" appState)
         , nameInput
         , FormExtra.mdAfter (l_ "integration.name.description" appState)
         , logoUrlInput
         , FormExtra.mdAfter (l_ "integration.logo.description" appState)
         , propsInput
         , FormExtra.mdAfter (l_ "integration.props.description" appState)
         ]
            ++ integrationTypeInputs
            ++ [ annotationsInput
               , FormGroup.plainGroup questionsWithIntegration (l_ "integration.questions.label" appState)
               ]
        )


viewAnswerEditor : EditorConfig msg -> Answer -> Html msg
viewAnswerEditor { appState, wrapMsg, eventMsg, model, editorBranch } answer =
    let
        parentUuid =
            EditorBranch.getParentUuid answer.uuid editorBranch

        createEditEvent map value =
            EditAnswerEventData.init
                |> map value
                |> EditAnswerEvent
                |> eventMsg parentUuid (Just answer.uuid)

        questionAddEvent =
            AddQuestionEventData.init
                |> AddQuestionEvent
                |> eventMsg answer.uuid Nothing

        answerEditorTitle =
            editorTitle appState
                { title = lg "answer" appState
                , uuid = answer.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just AnswerState
                , mbMovingEntity = Just TreeInput.MovingAnswer
                }

        labelInput =
            Input.string
                { name = "label"
                , label = lg "answer.label" appState
                , value = answer.label
                , onInput = createEditEvent setLabel
                }

        adviceInput =
            Input.markdown appState
                { name = "advice"
                , label = lg "answer.advice" appState
                , value = String.fromMaybe answer.advice
                , onInput = createEditEvent setAdvice << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = answer.uuid
                , markdownPreviews = model.markdownPreviews
                }

        followUpsInput =
            Input.reorderable appState
                { name = "questions"
                , label = lg "followupQuestions" appState
                , items = EditorBranch.filterDeleted editorBranch answer.followUpUuids
                , entityUuid = answer.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setFollowUpUuids
                , getRoute = editorRoute editorBranch
                , getName = KnowledgeModel.getQuestionName editorBranch.branch.knowledgeModel
                , untitledLabel = lg "question.untitled" appState
                , addChildLabel = l_ "answer.addQuestion" appState
                , addChildMsg = questionAddEvent
                , addChildDataCy = "question"
                }

        metrics =
            EditorBranch.filterDeletedWith .uuid editorBranch <|
                KnowledgeModel.getMetrics editorBranch.branch.knowledgeModel

        metricsInput =
            if List.isEmpty metrics then
                emptyNode

            else
                Input.metrics appState
                    { metrics = metrics
                    , metricMeasures = answer.metricMeasures
                    , onChange = createEditEvent setMetricMeasures
                    }

        annotationsInput =
            Input.annotations appState
                { annotations = answer.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("answer-" ++ answer.uuid)
        [ answerEditorTitle
        , labelInput
        , adviceInput
        , followUpsInput
        , metricsInput
        , annotationsInput
        ]


viewChoiceEditor : EditorConfig msg -> Choice -> Html msg
viewChoiceEditor { appState, wrapMsg, eventMsg, editorBranch } choice =
    let
        parentUuid =
            EditorBranch.getParentUuid choice.uuid editorBranch

        createEditEvent map value =
            EditChoiceEventData.init
                |> map value
                |> EditChoiceEvent
                |> eventMsg parentUuid (Just choice.uuid)

        choiceEditorTitle =
            editorTitle appState
                { title = lg "choice" appState
                , uuid = choice.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just ChoiceState
                , mbMovingEntity = Just TreeInput.MovingChoice
                }

        labelInput =
            Input.string
                { name = "label"
                , label = lg "choice.label" appState
                , value = choice.label
                , onInput = createEditEvent setLabel
                }

        annotationsInput =
            Input.annotations appState
                { annotations = choice.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("choice-" ++ choice.uuid)
        [ choiceEditorTitle
        , labelInput
        , annotationsInput
        ]


viewReferenceEditor : EditorConfig msg -> Reference -> Html msg
viewReferenceEditor { appState, wrapMsg, eventMsg, editorBranch } reference =
    let
        referenceUuid =
            Reference.getUuid reference

        parentUuid =
            EditorBranch.getParentUuid (Reference.getUuid reference) editorBranch

        onTypeChange value =
            eventMsg parentUuid (Just referenceUuid) <|
                case value of
                    "ResourcePage" ->
                        EditReferenceResourcePageEventData.init
                            |> (EditReferenceEvent << EditReferenceResourcePageEvent)

                    _ ->
                        EditReferenceURLEventData.init
                            |> (EditReferenceEvent << EditReferenceURLEvent)

        referenceTypeOptions =
            [ ( "ResourcePage", lg "referenceType.resourcePage" appState )
            , ( "URL", lg "referenceType.url" appState )
            ]

        referenceEditorTitle =
            editorTitle appState
                { title = lg "reference" appState
                , uuid = Reference.getUuid reference
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just ReferenceState
                , mbMovingEntity = Just TreeInput.MovingReference
                }

        typeInput =
            Input.select
                { name = "type"
                , label = lg "referenceType" appState
                , value = Reference.getTypeString reference
                , options = referenceTypeOptions
                , onChange = onTypeChange
                }

        referenceTypeInputs =
            case reference of
                ResourcePageReference data ->
                    let
                        createTypeEditEvent map value =
                            EditReferenceResourcePageEventData.init
                                |> map value
                                |> (EditReferenceEvent << EditReferenceResourcePageEvent)
                                |> eventMsg parentUuid (Just referenceUuid)

                        shortUuidInput =
                            Input.string
                                { name = "shortUuid"
                                , label = lg "reference.shortUuid" appState
                                , value = data.shortUuid
                                , onInput = createTypeEditEvent setShortUuid
                                }

                        annotationsInput =
                            Input.annotations appState
                                { annotations = Reference.getAnnotations reference
                                , onEdit = createTypeEditEvent setAnnotations
                                }
                    in
                    [ shortUuidInput, annotationsInput ]

                URLReference data ->
                    let
                        createTypeEditEvent map value =
                            EditReferenceURLEventData.init
                                |> map value
                                |> (EditReferenceEvent << EditReferenceURLEvent)
                                |> eventMsg parentUuid (Just referenceUuid)

                        urlInput =
                            Input.string
                                { name = "url"
                                , label = lg "reference.url" appState
                                , value = data.url
                                , onInput = createTypeEditEvent setUrl
                                }

                        labelInput =
                            Input.string
                                { name = "label"
                                , label = lg "reference.label" appState
                                , value = data.label
                                , onInput = createTypeEditEvent setLabel
                                }

                        annotationsInput =
                            Input.annotations appState
                                { annotations = Reference.getAnnotations reference
                                , onEdit = createTypeEditEvent setAnnotations
                                }
                    in
                    [ urlInput
                    , labelInput
                    , annotationsInput
                    ]

                CrossReference _ ->
                    []
    in
    editor ("reference-" ++ Reference.getUuid reference)
        ([ referenceEditorTitle
         , typeInput
         ]
            ++ referenceTypeInputs
        )


viewExpertEditor : EditorConfig msg -> Expert -> Html msg
viewExpertEditor { appState, wrapMsg, eventMsg, editorBranch } expert =
    let
        parentUuid =
            EditorBranch.getParentUuid expert.uuid editorBranch

        createEditEvent map value =
            EditExpertEventData.init
                |> map value
                |> EditExpertEvent
                |> eventMsg parentUuid (Just expert.uuid)

        expertEditorTitle =
            editorTitle appState
                { title = lg "expert" appState
                , uuid = expert.uuid
                , wrapMsg = wrapMsg
                , mbDeleteModalState = Just ExpertState
                , mbMovingEntity = Just TreeInput.MovingExpert
                }

        nameInput =
            Input.string
                { name = "name"
                , label = lg "expert.name" appState
                , value = expert.name
                , onInput = createEditEvent setName
                }

        emailInput =
            Input.string
                { name = "email"
                , label = lg "expert.email" appState
                , value = expert.email
                , onInput = createEditEvent setEmail
                }

        annotationsInput =
            Input.annotations appState
                { annotations = expert.annotations
                , onEdit = createEditEvent setAnnotations
                }
    in
    editor ("expert-" ++ expert.uuid)
        [ expertEditorTitle
        , nameInput
        , emailInput
        , annotationsInput
        ]


viewEmptyEditor : AppState -> Html msg
viewEmptyEditor appState =
    editor "empty"
        [ Flash.error appState (l_ "empty" appState)
        ]


editor : String -> List (Html msg) -> Html msg
editor editorId =
    div [ id editorId, class "editor-content col-xl-10 col-lg-12" ]


editorRoute : EditorBranch -> String -> Routes.Route
editorRoute editorBranch entityUuidString =
    Routes.kmEditorEditor editorBranch.branch.uuid (EditorBranch.getEditUuid entityUuidString editorBranch)


type alias EditorTitleConfig msg =
    { title : String
    , uuid : String
    , wrapMsg : Msg -> msg
    , mbDeleteModalState : Maybe (String -> DeleteModalState)
    , mbMovingEntity : Maybe TreeInput.MovingEntity
    }


editorTitle : AppState -> EditorTitleConfig msg -> Html msg
editorTitle appState config =
    let
        copyUuidButton =
            button
                [ class "btn btn-link link-with-icon"
                , title <| l_ "editorTitle.copyUuid" appState
                , onClick <| config.wrapMsg <| CopyUuid config.uuid
                ]
                [ faSet "kmEditor.copyUuid" appState
                , small [] [ text <| String.slice 0 8 config.uuid ]
                ]

        moveButton =
            case config.mbMovingEntity of
                Just movingEntity ->
                    button
                        [ class "btn btn-outline-secondary link-with-icon"
                        , onClick <| config.wrapMsg <| OpenMoveModal movingEntity config.uuid
                        , dataCy "km-editor_move-button"
                        ]
                        [ faSet "kmEditor.move" appState
                        , lx_ "editorTitle.move" appState
                        ]

                Nothing ->
                    emptyNode

        deleteButton =
            case config.mbDeleteModalState of
                Just deleteModalState ->
                    button
                        [ class "btn btn-outline-danger link-with-icon"
                        , dataCy "km-editor_delete-button"
                        , onClick <| config.wrapMsg <| SetDeleteModalState <| deleteModalState config.uuid
                        ]
                        [ faSet "_global.delete" appState
                        , lx_ "editorTitle.delete" appState
                        ]

                Nothing ->
                    emptyNode
    in
    div [ class "editor-title" ]
        [ h3 [] [ text config.title ]
        , div [ class "editor-title-buttons" ]
            [ copyUuidButton
            , moveButton
            , deleteButton
            ]
        ]



-- DELETE MODAL


deleteModal : AppState -> (Msg -> msg) -> EventMsg msg -> EditorBranch -> DeleteModalState -> Html msg
deleteModal appState wrapMsg eventMsg editorBranch deleteModalState =
    let
        createEvent event uuid =
            eventMsg (EditorBranch.getParentUuid uuid editorBranch) (Just uuid) event

        ( visible, content ) =
            case deleteModalState of
                ChapterState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.chapter" appState)
                        (createEvent DeleteChapterEvent uuid)
                    )

                QuestionState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.question" appState)
                        (createEvent DeleteQuestionEvent uuid)
                    )

                MetricState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.metric" appState)
                        (createEvent DeleteMetricEvent uuid)
                    )

                PhaseState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.phase" appState)
                        (createEvent DeletePhaseEvent uuid)
                    )

                TagState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.tag" appState)
                        (createEvent DeleteTagEvent uuid)
                    )

                IntegrationState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.integration" appState)
                        (createEvent DeleteIntegrationEvent uuid)
                    )

                AnswerState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.answer" appState)
                        (createEvent DeleteAnswerEvent uuid)
                    )

                ChoiceState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.choice" appState)
                        (createEvent DeleteChoiceEvent uuid)
                    )

                ReferenceState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.reference" appState)
                        (createEvent DeleteReferenceEvent uuid)
                    )

                ExpertState uuid ->
                    ( True
                    , getContent
                        (l_ "deleteModal.expert" appState)
                        (createEvent DeleteExpertEvent uuid)
                    )

                Closed ->
                    ( False, [ emptyNode ] )

        getContent contentText onDelete =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ lx_ "deleteModal.title" appState ]
                ]
            , div [ class "modal-body" ]
                [ text contentText ]
            , div [ class "modal-footer" ]
                [ button
                    [ class "btn btn-danger"
                    , dataCy "modal_action-button"
                    , onClick onDelete
                    ]
                    [ lx_ "deleteModal.delete" appState ]
                , button
                    [ class "btn btn-secondary"
                    , dataCy "modal_cancel-button"
                    , onClick <| wrapMsg <| SetDeleteModalState Closed
                    ]
                    [ lx_ "deleteModal.cancel" appState ]
                ]
            ]

        modalConfig =
            { modalContent = content
            , visible = visible
            , dataCy = "km-editor-delete"
            }
    in
    Modal.simple modalConfig



-- MOVE MODAL


moveModal : AppState -> (Msg -> msg) -> EventMsg msg -> EditorBranch -> Maybe MoveModalState -> Html msg
moveModal appState wrapMsg eventMsg editorBranch mbMoveModalState =
    let
        content =
            case mbMoveModalState of
                Just moveModalState ->
                    let
                        parentUuid =
                            EditorBranch.getParentUuid moveModalState.movingUuid editorBranch

                        selectedUuid =
                            moveModalState.treeInputModel.selected

                        createEvent event =
                            eventMsg parentUuid (Just moveModalState.movingUuid) (event { targetUuid = selectedUuid })

                        viewProps =
                            { editorBranch = editorBranch
                            , movingUuid = moveModalState.movingUuid
                            , movingParentUuid = parentUuid
                            , movingEntity = moveModalState.movingEntity
                            }

                        onMove =
                            case moveModalState.movingEntity of
                                TreeInput.MovingQuestion ->
                                    createEvent MoveQuestionEvent

                                TreeInput.MovingAnswer ->
                                    createEvent MoveAnswerEvent

                                TreeInput.MovingChoice ->
                                    createEvent MoveChoiceEvent

                                TreeInput.MovingReference ->
                                    createEvent MoveReferenceEvent

                                TreeInput.MovingExpert ->
                                    createEvent MoveExpertEvent
                    in
                    [ div [ class "modal-header" ]
                        [ h5 [ class "modal-title" ] [ lx_ "moveModal.title" appState ]
                        ]
                    , div [ class "modal-body" ]
                        [ label [] [ lx_ "moveModal.label" appState ]
                        , Html.map (wrapMsg << MoveModalMsg) <| TreeInput.view appState viewProps moveModalState.treeInputModel
                        ]
                    , div [ class "modal-footer" ]
                        [ button
                            [ class "btn btn-primary"
                            , onClick onMove
                            , disabled (String.isEmpty selectedUuid)
                            , dataCy "modal_action-button"
                            ]
                            [ lx_ "moveModal.move" appState ]
                        , button
                            [ class "btn btn-secondary"
                            , onClick <| wrapMsg CloseMoveModal
                            , dataCy "modal_cancel-button"
                            ]
                            [ lx_ "moveModal.cancel" appState ]
                        ]
                    ]

                Nothing ->
                    []

        modalConfig =
            { modalContent = content
            , visible = Maybe.isJust mbMoveModalState
            , dataCy = "km-editor-move"
            }
    in
    Modal.simple modalConfig
