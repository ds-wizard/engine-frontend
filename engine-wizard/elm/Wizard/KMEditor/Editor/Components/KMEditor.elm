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
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h3, h5, i, img, label, li, small, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, src)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Html.Keyed
import List.Extra as List
import Maybe.Extra as Maybe
import Reorderable
import Set
import Shared.Common.ByteUnits as ByteUnits
import Shared.Components.Badge as Badge
import Shared.Components.FontAwesome exposing (faDelete, faKmEditorCopyUuid, faKmEditorMove, faKmIntegration, faQuestionnaireExpand, faQuestionnaireShrink, faWarning)
import Shared.Copy as Copy
import Shared.Markdown as Markdown
import Shared.Utils exposing (compose2, flip, httpMethodOptions, nilUuid)
import SplitPane
import String.Extra as String
import String.Format as String
import Task.Extra as Task
import Uuid
import Wizard.Api.Models.Event exposing (Event(..))
import Wizard.Api.Models.Event.AddAnswerEventData as AddAnswerEventData
import Wizard.Api.Models.Event.AddChapterEventData as AddChapterEventData
import Wizard.Api.Models.Event.AddChoiceEventData as AddChoiceEventData
import Wizard.Api.Models.Event.AddExpertEventData as AddExpertEventData
import Wizard.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Wizard.Api.Models.Event.AddMetricEventData as AddMetricEventData
import Wizard.Api.Models.Event.AddPhaseEventData as AddPhaseEventData
import Wizard.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Wizard.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Wizard.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData
import Wizard.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData
import Wizard.Api.Models.Event.AddTagEventData as AddTagEventData
import Wizard.Api.Models.Event.CommonEventData exposing (CommonEventData)
import Wizard.Api.Models.Event.EditAnswerEventData as EditAnswerEventData
import Wizard.Api.Models.Event.EditChapterEventData as EditChapterEventData
import Wizard.Api.Models.Event.EditChoiceEventData as EditChoiceEventData
import Wizard.Api.Models.Event.EditEventSetters exposing (setAbbreviation, setAdvice, setAnnotations, setAnswerUuids, setChapterUuids, setChoiceUuids, setColor, setContent, setDescription, setEmail, setExpertUuids, setFileTypes, setFollowUpUuids, setId, setIntegrationUuid, setIntegrationUuids, setItemTemplateQuestionUuids, setItemUrl, setLabel, setListQuestionUuid, setLogo, setMaxSize, setMetricMeasures, setMetricUuids, setName, setPhaseUuids, setProps, setQuestionUuids, setReferenceUuids, setRequestBody, setRequestEmptySearch, setRequestHeaders, setRequestMethod, setRequestUrl, setRequiredPhaseUuid, setResourceCollectionUuids, setResourcePageUuid, setResourcePageUuids, setResponseItemId, setResponseItemTemplate, setResponseListField, setTagUuids, setText, setTitle, setUrl, setValidations, setValueType, setWidgetUrl)
import Wizard.Api.Models.Event.EditExpertEventData as EditExpertEventData
import Wizard.Api.Models.Event.EditIntegrationApiEventData as EditIntegrationApiEventData
import Wizard.Api.Models.Event.EditIntegrationEventData exposing (EditIntegrationEventData(..))
import Wizard.Api.Models.Event.EditIntegrationWidgetEventData as EditIntegrationWidgetEventData
import Wizard.Api.Models.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData
import Wizard.Api.Models.Event.EditMetricEventData as EditMetricEventData
import Wizard.Api.Models.Event.EditPhaseEventData as EditPhaseEventData
import Wizard.Api.Models.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Wizard.Api.Models.Event.EditQuestionFileEventData as EditQuestionFileEventData
import Wizard.Api.Models.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData
import Wizard.Api.Models.Event.EditQuestionItemSelectData as EditQuestionItemSelectEventData
import Wizard.Api.Models.Event.EditQuestionListEventData as EditQuestionListEventData
import Wizard.Api.Models.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData
import Wizard.Api.Models.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData
import Wizard.Api.Models.Event.EditQuestionValueEventData as EditQuestionValueEventData
import Wizard.Api.Models.Event.EditReferenceEventData exposing (EditReferenceEventData(..))
import Wizard.Api.Models.Event.EditReferenceResourcePageEventData as EditReferenceResourcePageEventData
import Wizard.Api.Models.Event.EditReferenceURLEventData as EditReferenceURLEventData
import Wizard.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData
import Wizard.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData
import Wizard.Api.Models.Event.EditTagEventData as EditTagEventData
import Wizard.Api.Models.Event.EventField as EventField
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration(..))
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks exposing (GuideLinks)
import Wizard.Common.Html exposing (guideLink, linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor.Breadcrumbs as Breadcrumbs
import Wizard.KMEditor.Editor.Components.KMEditor.Input as Input
import Wizard.KMEditor.Editor.Components.KMEditor.Tree as Tree
import Wizard.KMEditor.Editor.Components.KMEditor.TreeInput as TreeInput
import Wizard.Ports as Ports



-- MODEL


type alias Model =
    { splitPane : SplitPane.State
    , markdownPreviews : List String
    , reorderableStates : Dict String Reorderable.State
    , deleteModalState : DeleteModalState
    , moveModalState : Maybe MoveModalState
    , warningsPanelOpen : Bool
    }


type DeleteModalState
    = ChapterState String
    | MetricState String
    | PhaseState String
    | TagState String
    | IntegrationState String
    | ResourceCollectionState String
    | ResourcePageState String
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
    , warningsPanelOpen = False
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
    | SetWarningPanelsOpen Bool


update : (Bool -> msg) -> Msg -> Model -> EditorBranch -> ( EditorBranch, Model, Cmd msg )
update setFullscreenMsg msg model editorBranch =
    case msg of
        SplitPaneMsg splitPaneMsg ->
            ( editorBranch, { model | splitPane = SplitPane.update splitPaneMsg model.splitPane }, Cmd.none )

        SetFullscreen fullscreen ->
            ( editorBranch, model, Task.dispatch (setFullscreenMsg fullscreen) )

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
            let
                scrollCmd =
                    Ports.scrollIntoViewCenter "[data-km-editor_move-modal_item_current]"
            in
            ( editorBranch
            , { model
                | moveModalState =
                    Just
                        { movingEntity = movingEntity
                        , movingUuid = movingUuid
                        , treeInputModel = TreeInput.initialModel (Set.fromList editorBranch.openNodeUuids)
                        }
              }
            , scrollCmd
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

        SetWarningPanelsOpen open ->
            ( editorBranch, { model | warningsPanelOpen = open }, Cmd.none )



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
    Bool -> Maybe String -> String -> Maybe String -> (CommonEventData -> Event) -> msg


view : AppState -> (Msg -> msg) -> EventMsg msg -> Model -> List Integration -> EditorBranch -> Html msg
view appState wrapMsg eventMsg model integrationPrefabs editorBranch =
    let
        ( expandIcon, expandMsg ) =
            if AppState.isFullscreen appState then
                ( faQuestionnaireShrink, wrapMsg <| SetFullscreen False )

            else
                ( faQuestionnaireExpand, wrapMsg <| SetFullscreen True )

        treeViewProps =
            { expandAll = wrapMsg ExpandAll
            , collapseAll = wrapMsg CollapseAll
            , setTreeOpen = compose2 wrapMsg SetTreeOpen
            , createEvents =
                { createChapter = \kmUuid -> eventMsg False Nothing kmUuid Nothing (AddChapterEvent AddChapterEventData.init)
                , createQuestion = \parentUuid -> eventMsg False Nothing parentUuid Nothing (AddQuestionEvent AddQuestionEventData.init)
                , createAnswer = \questionUuid -> eventMsg False Nothing questionUuid Nothing (AddAnswerEvent AddAnswerEventData.init)
                , createChoice = \questionUuid -> eventMsg False Nothing questionUuid Nothing (AddChoiceEvent AddChoiceEventData.init)
                , createExpert = \questionUuid -> eventMsg False Nothing questionUuid Nothing (AddExpertEvent AddExpertEventData.init)
                , createReference = \questionUuid -> eventMsg False Nothing questionUuid Nothing (AddReferenceEvent AddReferenceEventData.init)
                , createResourceCollection = \questionUuid -> eventMsg False Nothing questionUuid Nothing (AddResourceCollectionEvent AddResourceCollectionEventData.init)
                , createResourcePage = \referenceUuid -> eventMsg False Nothing referenceUuid Nothing (AddResourcePageEvent AddResourcePageEventData.init)
                , createIntegration = \kmUuid -> eventMsg False Nothing kmUuid Nothing (AddIntegrationEvent AddIntegrationEventData.init)
                , createTag = \kmUuid -> eventMsg False Nothing kmUuid Nothing (AddTagEvent AddTagEventData.init)
                , createMetric = \kmUuid -> eventMsg False Nothing kmUuid Nothing (AddMetricEvent AddMetricEventData.init)
                , createPhase = \kmUuid -> eventMsg False Nothing kmUuid Nothing (AddPhaseEvent AddPhaseEventData.init)
                }
            }

        splitPaneConfig =
            SplitPane.createViewConfig
                { toMsg = wrapMsg << SplitPaneMsg
                , customSplitter = Nothing
                }

        warningsCount =
            List.length editorBranch.warnings

        warningsButton =
            if warningsCount > 0 then
                a
                    [ class "item"
                    , classList [ ( "selected", model.warningsPanelOpen ) ]
                    , onClick (wrapMsg (SetWarningPanelsOpen (not model.warningsPanelOpen)))
                    ]
                    [ text (gettext "Warnings" appState.locale)
                    , Badge.danger [ class "rounded-pill" ] [ text (String.fromInt warningsCount) ]
                    ]

            else
                Html.nothing

        warningsPanel =
            if warningsCount > 0 && model.warningsPanelOpen then
                Html.map wrapMsg <|
                    viewWarningsPanel appState editorBranch

            else
                Html.nothing
    in
    div [ class "KMEditor__Editor__KMEditor", dataCy "km-editor_km" ]
        [ div [ class "editor-breadcrumbs" ]
            [ Breadcrumbs.view appState editorBranch
            , warningsButton
            , a [ class "breadcrumb-button", onClick expandMsg ] [ expandIcon ]
            ]
        , div [ class "editor-body" ]
            [ SplitPane.view splitPaneConfig
                (Tree.view treeViewProps appState editorBranch)
                (viewEditor appState wrapMsg eventMsg model integrationPrefabs editorBranch)
                model.splitPane
            , warningsPanel
            ]
        , deleteModal appState wrapMsg eventMsg editorBranch model.deleteModalState
        , moveModal appState wrapMsg eventMsg editorBranch model.moveModalState
        ]


viewWarningsPanel : AppState -> EditorBranch -> Html Msg
viewWarningsPanel appState editorBranch =
    let
        viewWarning warning =
            li [] [ linkTo (EditorBranch.editorRoute editorBranch warning.editorUuid) [] [ text warning.message ] ]

        warnings =
            if List.isEmpty editorBranch.warnings then
                Flash.info (gettext "There are no more warnings." appState.locale)

            else
                ul [] (List.map viewWarning editorBranch.warnings)
    in
    div [ class "editor-right-panel" ]
        [ warnings ]


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

        resourceCollectionEditor =
            createEditor viewResourceCollectionEditor KnowledgeModel.getResourceCollection

        resourcePageEditor =
            createEditor viewResourcePageEditor KnowledgeModel.getResourcePage

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
                |> Maybe.orElse resourceCollectionEditor
                |> Maybe.orElse resourcePageEditor
                |> Maybe.orElse answerEditor
                |> Maybe.orElse choiceEditor
                |> Maybe.orElse referenceEditor
                |> Maybe.orElse expertEditor
                |> Maybe.map (Tuple.pair editorBranch.activeUuid)
                |> Maybe.withDefault emptyEditor
    in
    Html.Keyed.node "div"
        [ class "editor-form-view", id "editor-view", attribute "data-editor-uuid" editorBranch.activeUuid ]
        [ editorContent ]


viewKnowledgeModelEditor : EditorConfig msg -> KnowledgeModel -> Html msg
viewKnowledgeModelEditor { appState, wrapMsg, eventMsg, model, editorBranch } km =
    let
        kmUuid =
            Uuid.toString km.uuid

        kmEditorTitle =
            editorTitle appState
                { title = gettext "Knowledge Model" appState.locale
                , uuid = kmUuid
                , wrapMsg = wrapMsg
                , copyUuidButton = False
                , mbDeleteModalState = Nothing
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditKnowledgeModelEventData.init
                |> map value
                |> EditKnowledgeModelEvent
                |> eventMsg True selector nilUuid (Just kmUuid)

        addChapterEvent =
            AddChapterEvent AddChapterEventData.init
                |> eventMsg False Nothing kmUuid Nothing

        addMetricEvent =
            AddMetricEvent AddMetricEventData.init
                |> eventMsg False Nothing kmUuid Nothing

        addPhaseEvent =
            AddPhaseEvent AddPhaseEventData.init
                |> eventMsg False Nothing kmUuid Nothing

        addTagEvent =
            AddTagEvent AddTagEventData.init
                |> eventMsg False Nothing kmUuid Nothing

        addIntegrationEvent =
            AddIntegrationEvent AddIntegrationEventData.init
                |> eventMsg False Nothing kmUuid Nothing

        addResourceCollectionEvent =
            AddResourceCollectionEvent AddResourceCollectionEventData.init
                |> eventMsg False Nothing kmUuid Nothing

        chaptersInput =
            Input.reorderable
                { name = "chapters"
                , label = gettext "Chapters" appState.locale
                , items = EditorBranch.filterDeleted editorBranch km.chapterUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setChapterUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getChapterName km
                , untitledLabel = gettext "Untitled chapter" appState.locale
                , addChildLabel = gettext "Add chapter" appState.locale
                , addChildMsg = addChapterEvent
                , addChildDataCy = "chapter"
                }

        metricsInput =
            Input.reorderable
                { name = "metrics"
                , label = gettext "Metrics" appState.locale
                , items = EditorBranch.filterDeleted editorBranch km.metricUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setMetricUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getMetricName km
                , untitledLabel = gettext "Untitled metric" appState.locale
                , addChildLabel = gettext "Add metric" appState.locale
                , addChildMsg = addMetricEvent
                , addChildDataCy = "metric"
                }

        phasesInput =
            Input.reorderable
                { name = "phases"
                , label = gettext "Phases" appState.locale
                , items = EditorBranch.filterDeleted editorBranch km.phaseUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setPhaseUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getPhaseName km
                , untitledLabel = gettext "Untitled phase" appState.locale
                , addChildLabel = gettext "Add phase" appState.locale
                , addChildMsg = addPhaseEvent
                , addChildDataCy = "phase"
                }

        tagsInput =
            Input.reorderable
                { name = "tags"
                , label = gettext "Question Tags" appState.locale
                , items = EditorBranch.filterDeleted editorBranch km.tagUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setTagUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getTagName km
                , untitledLabel = gettext "Untitled tag" appState.locale
                , addChildLabel = gettext "Add tag" appState.locale
                , addChildMsg = addTagEvent
                , addChildDataCy = "tag"
                }

        integrationsInput =
            Input.reorderable
                { name = "integrations"
                , label = gettext "Integrations" appState.locale
                , items = EditorBranch.filterDeleted editorBranch km.integrationUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setIntegrationUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getIntegrationName km
                , untitledLabel = gettext "Untitled integration" appState.locale
                , addChildLabel = gettext "Add integration" appState.locale
                , addChildMsg = addIntegrationEvent
                , addChildDataCy = "integration"
                }

        resourceCollectionsInput =
            Input.reorderable
                { name = "resourceCollections"
                , label = gettext "Resource Collections" appState.locale
                , items = EditorBranch.filterDeleted editorBranch km.resourceCollectionUuids
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setResourceCollectionUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getResourceCollectionName km
                , untitledLabel = gettext "Untitled resource collection" appState.locale
                , addChildLabel = gettext "Add resource collection" appState.locale
                , addChildMsg = addResourceCollectionEvent
                , addChildDataCy = "resource-collection"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = km.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
                }
    in
    editor ("km-" ++ kmUuid)
        [ kmEditorTitle
        , chaptersInput
        , metricsInput
        , phasesInput
        , tagsInput
        , integrationsInput
        , resourceCollectionsInput
        , annotationsInput
        ]


viewChapterEditor : EditorConfig msg -> Chapter -> Html msg
viewChapterEditor { appState, wrapMsg, eventMsg, model, editorBranch } chapter =
    let
        parentUuid =
            EditorBranch.getParentUuid chapter.uuid editorBranch

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditChapterEventData.init
                |> map value
                |> EditChapterEvent
                |> eventMsg True selector parentUuid (Just chapter.uuid)

        questionAddEvent =
            AddQuestionEventData.init
                |> AddQuestionEvent
                |> eventMsg False Nothing chapter.uuid Nothing

        chapterEditorTitle =
            editorTitle appState
                { title = gettext "Chapter" appState.locale
                , uuid = chapter.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just ChapterState
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = gettext "Title" appState.locale
                , value = chapter.title
                , onInput = createEditEvent setTitle
                }

        textInput =
            Input.markdown appState
                { name = "text"
                , label = gettext "Text" appState.locale
                , value = Maybe.withDefault "" chapter.text
                , onInput = createEditEvent setText << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = chapter.uuid
                , markdownPreviews = model.markdownPreviews
                }

        questionsInput =
            Input.reorderable
                { name = "questions"
                , label = gettext "Questions" appState.locale
                , items = EditorBranch.filterDeleted editorBranch chapter.questionUuids
                , entityUuid = chapter.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setQuestionUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getQuestionName editorBranch.branch.knowledgeModel
                , untitledLabel = gettext "Untitled question" appState.locale
                , addChildLabel = gettext "Add question" appState.locale
                , addChildMsg = questionAddEvent
                , addChildDataCy = "question"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = chapter.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
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

        createEditEvent setOptions setList setValue setIntegration setMultiChoice setItemSelect setFile value =
            createEditEventWithFocusSelector setOptions setList setValue setIntegration setMultiChoice setItemSelect setFile Nothing value

        createEditEventWithFocusSelector setOptions setList setValue setIntegration setMultiChoice setItemSelect setFile selector value =
            eventMsg True selector parentUuid (Just questionUuid) <|
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

                        ItemSelectQuestion _ _ ->
                            EditQuestionItemSelectEventData.init
                                |> setItemSelect value
                                |> EditQuestionItemSelectEvent

                        FileQuestion _ _ ->
                            EditQuestionFileEventData.init
                                |> setFile value
                                |> EditQuestionFileEvent

        onTypeChange value =
            eventMsg False Nothing parentUuid (Just questionUuid) <|
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

                    "ItemSelect" ->
                        EditQuestionItemSelectEventData.init
                            |> EditQuestionItemSelectEvent
                            |> EditQuestionEvent

                    "File" ->
                        EditQuestionFileEventData.init
                            |> EditQuestionFileEvent
                            |> EditQuestionEvent

                    _ ->
                        EditQuestionOptionsEventData.init
                            |> EditQuestionOptionsEvent
                            |> EditQuestionEvent

        addReferenceEvent =
            AddReferenceEventData.init
                |> AddReferenceEvent
                |> eventMsg False Nothing questionUuid Nothing

        expertAddEvent =
            AddExpertEventData.init
                |> AddExpertEvent
                |> eventMsg False Nothing questionUuid Nothing

        questionTypeOptions =
            [ ( "Options", gettext "Options" appState.locale )
            , ( "List", gettext "List of Items" appState.locale )
            , ( "Value", gettext "Value" appState.locale )
            , ( "Integration", gettext "Integration" appState.locale )
            , ( "MultiChoice", gettext "Multi-Choice" appState.locale )
            , ( "ItemSelect", gettext "Item Select" appState.locale )
            , ( "File", gettext "File" appState.locale )
            ]

        requiredPhaseUuidOptions =
            KnowledgeModel.getPhases editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith .uuid editorBranch
                |> List.map (\phase -> ( phase.uuid, phase.title ))
                |> (::) ( "", gettext "Never" appState.locale )

        questionEditorTitle =
            editorTitle appState
                { title = gettext "Question" appState.locale
                , uuid = Question.getUuid question
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just QuestionState
                , mbMovingEntity = Just TreeInput.MovingQuestion
                , mbGuideLink = Nothing
                }

        typeInput =
            Input.select
                { name = "type"
                , label = gettext "Type" appState.locale
                , value = Question.getTypeString question
                , options = questionTypeOptions
                , onChange = onTypeChange
                , extra = Nothing
                }

        typeWarning =
            case question of
                OptionsQuestion _ _ ->
                    if List.isEmpty (EditorBranch.filterDeleted editorBranch <| Question.getAnswerUuids question) then
                        Html.nothing

                    else
                        FormExtra.blockAfter
                            [ faWarning
                            , text (gettext "Changing a question type will remove all answers." appState.locale)
                            ]

                ListQuestion _ _ ->
                    if List.isEmpty (EditorBranch.filterDeleted editorBranch <| Question.getItemTemplateQuestionUuids question) then
                        Html.nothing

                    else
                        FormExtra.blockAfter
                            [ faWarning
                            , text (gettext "Changing a question type will remove all item questions." appState.locale)
                            ]

                MultiChoiceQuestion _ _ ->
                    if List.isEmpty (EditorBranch.filterDeleted editorBranch <| Question.getChoiceUuids question) then
                        Html.nothing

                    else
                        FormExtra.blockAfter
                            [ faWarning
                            , text (gettext "Changing a question type will remove all choices." appState.locale)
                            ]

                _ ->
                    Html.nothing

        titleInput =
            Input.string
                { name = "title"
                , label = gettext "Title" appState.locale
                , value = Question.getTitle question
                , onInput = createEditEvent setTitle setTitle setTitle setTitle setTitle setTitle setTitle
                }

        textInput =
            Input.markdown appState
                { name = "text"
                , label = gettext "Text" appState.locale
                , value = Maybe.withDefault "" (Question.getText question)
                , onInput = createEditEvent setText setText setText setText setText setText setText << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = questionUuid
                , markdownPreviews = model.markdownPreviews
                }

        requiredPhaseUuidInput =
            Input.select
                { name = "requiredPhaseUuid"
                , label = gettext "When does this question become desirable?" appState.locale
                , value = String.fromMaybe <| Question.getRequiredPhaseUuid question
                , options = requiredPhaseUuidOptions
                , onChange = createEditEvent setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid setRequiredPhaseUuid << String.toMaybe
                , extra = Nothing
                }

        tagUuidsInput =
            Input.tags appState
                { label = gettext "Question Tags" appState.locale
                , tags = EditorBranch.filterDeletedWith .uuid editorBranch <| KnowledgeModel.getTags editorBranch.branch.knowledgeModel
                , selected = Question.getTagUuids question
                , onChange = createEditEvent setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids
                }

        referencesInput =
            Input.reorderable
                { name = "references"
                , label = gettext "References" appState.locale
                , items = EditorBranch.filterDeleted editorBranch <| Question.getReferenceUuids question
                , entityUuid = questionUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getReferenceName editorBranch.branch.knowledgeModel
                , untitledLabel = gettext "Untitled reference" appState.locale
                , addChildLabel = gettext "Add reference" appState.locale
                , addChildMsg = addReferenceEvent
                , addChildDataCy = "reference"
                }

        expertsInput =
            Input.reorderable
                { name = "experts"
                , label = gettext "Experts" appState.locale
                , items = EditorBranch.filterDeleted editorBranch <| Question.getExpertUuids question
                , entityUuid = questionUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getExpertName editorBranch.branch.knowledgeModel
                , untitledLabel = gettext "Untitled expert" appState.locale
                , addChildLabel = gettext "Add expert" appState.locale
                , addChildMsg = expertAddEvent
                , addChildDataCy = "expert"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = Question.getAnnotations question
                , onEdit = createEditEventWithFocusSelector setAnnotations setAnnotations setAnnotations setAnnotations setAnnotations setAnnotations setAnnotations
                }

        questionTypeInputs =
            case question of
                OptionsQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionOptionsEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionOptionsEvent)
                                |> eventMsg False Nothing parentUuid (Just questionUuid)

                        addAnswerEvent =
                            AddAnswerEventData.init
                                |> AddAnswerEvent
                                |> eventMsg False Nothing questionUuid Nothing

                        answersInput =
                            Input.reorderable
                                { name = "answers"
                                , label = gettext "Answers" appState.locale
                                , items = EditorBranch.filterDeleted editorBranch <| Question.getAnswerUuids question
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setAnswerUuids
                                , getRoute = EditorBranch.editorRoute editorBranch
                                , getName = KnowledgeModel.getAnswerName editorBranch.branch.knowledgeModel
                                , untitledLabel = gettext "Untitled answer" appState.locale
                                , addChildLabel = gettext "Add answer" appState.locale
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
                                |> eventMsg False Nothing parentUuid (Just questionUuid)

                        addItemTemplateQuestionEvent =
                            AddQuestionEventData.init
                                |> AddQuestionEvent
                                |> eventMsg False Nothing questionUuid Nothing

                        itemTemplateQuestionsInput =
                            Input.reorderable
                                { name = "questions"
                                , label = gettext "Questions" appState.locale
                                , items = EditorBranch.filterDeleted editorBranch <| Question.getItemTemplateQuestionUuids question
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setItemTemplateQuestionUuids
                                , getRoute = EditorBranch.editorRoute editorBranch
                                , getName = KnowledgeModel.getQuestionName editorBranch.branch.knowledgeModel
                                , untitledLabel = gettext "Untitled question" appState.locale
                                , addChildLabel = gettext "Add question" appState.locale
                                , addChildMsg = addItemTemplateQuestionEvent
                                , addChildDataCy = "question"
                                }
                    in
                    [ div [ class "form-group" ]
                        [ div [ class "card card-border-light card-item-template" ]
                            [ div [ class "card-header" ]
                                [ text (gettext "Item Template" appState.locale) ]
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
                                |> eventMsg False Nothing parentUuid (Just questionUuid)

                        questionValueTypeOptions =
                            [ ( "StringQuestionValueType", gettext "String" appState.locale )
                            , ( "NumberQuestionValueType", gettext "Number" appState.locale )
                            , ( "DateQuestionValueType", gettext "Date" appState.locale )
                            , ( "DateTimeQuestionValueType", gettext "Date Time" appState.locale )
                            , ( "TimeQuestionValueType", gettext "Time" appState.locale )
                            , ( "TextQuestionValueType", gettext "Text" appState.locale )
                            , ( "EmailQuestionValueType", gettext "Email" appState.locale )
                            , ( "UrlQuestionValueType", gettext "URL" appState.locale )
                            , ( "ColorQuestionValueType", gettext "Color" appState.locale )
                            ]

                        valueTypeInput =
                            Input.select
                                { name = "valueType"
                                , label = gettext "Value Type" appState.locale
                                , value = QuestionValueType.toString <| Maybe.withDefault QuestionValueType.default <| Question.getValueType question
                                , options = questionValueTypeOptions
                                , onChange = createTypeEditEvent setValueType << Maybe.withDefault QuestionValueType.default << QuestionValueType.fromString
                                , extra = Nothing
                                }

                        validationsInput =
                            Input.questionValidations appState
                                { label = gettext "Validations" appState.locale
                                , valueType = Maybe.withDefault QuestionValueType.default <| Question.getValueType question
                                , validations = Maybe.withDefault [] <| Question.getValidations question
                                , onChange = createTypeEditEvent setValidations
                                }
                    in
                    [ valueTypeInput
                    , validationsInput
                    ]

                IntegrationQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionIntegrationEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionIntegrationEvent)
                                |> eventMsg False Nothing parentUuid (Just questionUuid)

                        integrationUuidOptions =
                            KnowledgeModel.getIntegrations editorBranch.branch.knowledgeModel
                                |> EditorBranch.filterDeletedWith Integration.getUuid editorBranch
                                |> List.map (\integration -> ( Integration.getUuid integration, String.withDefault (gettext "Untitled integration" appState.locale) (Integration.getVisibleName integration) ))
                                |> (::) ( Uuid.toString Uuid.nil, gettext "- select integration -" appState.locale )

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
                                        [ div [ class "card-header" ] [ text (gettext "Integration Configuration" appState.locale) ]
                                        , div [ class "card-body" ]
                                            (List.map propInput selectedIntegrationProps)
                                        ]
                                    ]

                            else
                                Html.nothing

                        integrationLink integrationUuid =
                            if Uuid.toString Uuid.nil == integrationUuid then
                                Nothing

                            else
                                Just <|
                                    div [ class "mt-1" ]
                                        [ linkTo (EditorBranch.editorRoute editorBranch integrationUuid) [] [ text (gettext "Go to integration" appState.locale) ]
                                        ]

                        integrationUuidInput =
                            Input.select
                                { name = "integrationUuid"
                                , label = gettext "Integration" appState.locale
                                , value = String.fromMaybe <| Question.getIntegrationUuid question
                                , options = integrationUuidOptions
                                , onChange = createTypeEditEvent setIntegrationUuid
                                , extra = Maybe.andThen integrationLink (Question.getIntegrationUuid question)
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
                                |> eventMsg False Nothing parentUuid (Just questionUuid)

                        addChoiceEvent =
                            AddChoiceEventData.init
                                |> AddChoiceEvent
                                |> eventMsg False Nothing questionUuid Nothing

                        choicesInput =
                            Input.reorderable
                                { name = "choices"
                                , label = gettext "Choices" appState.locale
                                , items = EditorBranch.filterDeleted editorBranch <| Question.getChoiceUuids question
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setChoiceUuids
                                , getRoute = EditorBranch.editorRoute editorBranch
                                , getName = KnowledgeModel.getChoiceName editorBranch.branch.knowledgeModel
                                , untitledLabel = gettext "Untitled choice" appState.locale
                                , addChildLabel = gettext "Add choice" appState.locale
                                , addChildMsg = addChoiceEvent
                                , addChildDataCy = "choice"
                                }
                    in
                    [ choicesInput ]

                ItemSelectQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionItemSelectEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionItemSelectEvent)
                                |> eventMsg False Nothing parentUuid (Just questionUuid)

                        listQuestionUuidOptions =
                            KnowledgeModel.getAllQuestions editorBranch.branch.knowledgeModel
                                |> List.filter (not << flip EditorBranch.isQuestionDeletedInHierarchy editorBranch << Question.getUuid)
                                |> List.filter Question.isList
                                |> List.sortBy Question.getTitle
                                |> List.map (\q -> ( Question.getUuid q, Question.getTitle q ))
                                |> (::) ( "", gettext "- select list question -" appState.locale )

                        listQuestionUuidInput =
                            Input.select
                                { name = "listQuestionUuid"
                                , label = gettext "List Question" appState.locale
                                , value = String.fromMaybe <| Question.getListQuestionUuid question
                                , options = listQuestionUuidOptions
                                , onChange = createTypeEditEvent setListQuestionUuid << String.toMaybe
                                , extra =
                                    case Question.getListQuestionUuid question of
                                        Just listQuestionUuid ->
                                            if EditorBranch.isQuestionDeletedInHierarchy listQuestionUuid editorBranch then
                                                Nothing

                                            else
                                                Just <|
                                                    div [ class "mt-1" ]
                                                        [ linkTo (EditorBranch.editorRoute editorBranch listQuestionUuid) [] [ text (gettext "Go to list question" appState.locale) ]
                                                        ]

                                        Nothing ->
                                            Nothing
                                }
                    in
                    [ listQuestionUuidInput
                    ]

                FileQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionFileEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionFileEvent)
                                |> eventMsg True Nothing parentUuid (Just questionUuid)

                        fileTypesInput =
                            Input.string
                                { name = "fileTypes"
                                , label = gettext "File Types" appState.locale
                                , value = Maybe.withDefault "" <| Question.getFileTypes question
                                , onInput = createTypeEditEvent setFileTypes << String.toMaybe
                                }

                        maxSizeInput =
                            Input.fileSize
                                { name = "maxSize"
                                , label = gettext "Max Size (bytes)" appState.locale
                                , value = Maybe.unwrap "" String.fromInt (Question.getMaxSize question)
                                , onInput = createTypeEditEvent setMaxSize << Maybe.andThen String.toInt << String.toMaybe
                                , maxFileSize = appState.maxUploadFileSize
                                }
                    in
                    [ fileTypesInput
                    , FormExtra.mdAfter (gettext "You can limit file type selection by providing comma separated list of extensions, mime types, or combination. For example, `application/pdf` or `.xls,.xlsx`." appState.locale)
                    , maxSizeInput
                    , FormExtra.mdAfter (String.format (gettext "Uploaded files cannot be larger than %s, but you can set a smaller limit." appState.locale) [ ByteUnits.toReadable appState.maxUploadFileSize ])
                    ]

        wrapQuestionsWithIntegration questions =
            if List.isEmpty questions then
                Html.nothing

            else
                FormGroup.plainGroup (ul [] questions) (gettext "Item select questions using this list question" appState.locale)

        itemSelectQuestionsWithListQuestion =
            case question of
                ListQuestion _ _ ->
                    KnowledgeModel.getAllQuestions editorBranch.branch.knowledgeModel
                        |> EditorBranch.filterDeletedWith Question.getUuid editorBranch
                        |> List.filter ((==) (Just questionUuid) << Question.getListQuestionUuid)
                        |> List.filter (EditorBranch.isReachable editorBranch << Question.getUuid)
                        |> List.sortBy Question.getTitle
                        |> List.map (viewQuestionLink appState editorBranch)
                        |> wrapQuestionsWithIntegration

                _ ->
                    Html.nothing
    in
    editor ("question-" ++ questionUuid)
        ([ questionEditorTitle
         , typeInput
         , typeWarning
         , titleInput
         , textInput
         , requiredPhaseUuidInput
         , tagUuidsInput
         ]
            ++ questionTypeInputs
            ++ [ referencesInput
               , expertsInput
               , annotationsInput
               , itemSelectQuestionsWithListQuestion
               ]
        )


viewMetricEditor : EditorConfig msg -> Metric -> Html msg
viewMetricEditor { appState, wrapMsg, eventMsg, model, editorBranch } metric =
    let
        parentUuid =
            EditorBranch.getParentUuid metric.uuid editorBranch

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditMetricEventData.init
                |> map value
                |> EditMetricEvent
                |> eventMsg True selector parentUuid (Just metric.uuid)

        metricEditorTitle =
            editorTitle appState
                { title = gettext "Metric" appState.locale
                , uuid = metric.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just MetricState
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = gettext "Title" appState.locale
                , value = metric.title
                , onInput = createEditEvent setTitle
                }

        abbreviationInput =
            Input.string
                { name = "abbreviation"
                , label = gettext "Abbreviation" appState.locale
                , value = Maybe.withDefault "" metric.abbreviation
                , onInput = createEditEvent setAbbreviation << String.toMaybe
                }

        descriptionInput =
            Input.markdown appState
                { name = "description"
                , label = gettext "Description" appState.locale
                , value = Maybe.withDefault "" metric.description
                , onInput = createEditEvent setDescription << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = metric.uuid
                , markdownPreviews = model.markdownPreviews
                }

        annotationsInput =
            Input.annotations appState
                { annotations = metric.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
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
viewPhaseEditor { appState, wrapMsg, eventMsg, editorBranch } phase =
    let
        parentUuid =
            EditorBranch.getParentUuid phase.uuid editorBranch

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditPhaseEventData.init
                |> map value
                |> EditPhaseEvent
                |> eventMsg True selector parentUuid (Just phase.uuid)

        phaseEditorTitle =
            editorTitle appState
                { title = gettext "Phase" appState.locale
                , uuid = phase.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just PhaseState
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = gettext "Title" appState.locale
                , value = phase.title
                , onInput = createEditEvent setTitle
                }

        descriptionInput =
            Input.textarea
                { name = "description"
                , label = gettext "Description" appState.locale
                , value = Maybe.withDefault "" phase.description
                , onInput = createEditEvent setDescription << String.toMaybe
                }

        annotationsInput =
            Input.annotations appState
                { annotations = phase.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
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
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditTagEventData.init
                |> map value
                |> EditTagEvent
                |> eventMsg True selector parentUuid (Just tag.uuid)

        tagEditorTitle =
            editorTitle appState
                { title = gettext "Tag" appState.locale
                , uuid = tag.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just TagState
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        nameInput =
            Input.string
                { name = "name"
                , label = gettext "Name" appState.locale
                , value = tag.name
                , onInput = createEditEvent setName
                }

        descriptionInput =
            Input.textarea
                { name = "description"
                , label = gettext "Description" appState.locale
                , value = Maybe.withDefault "" tag.description
                , onInput = createEditEvent setDescription << String.toMaybe
                }

        colorInput =
            Input.color
                { name = "color"
                , label = gettext "Color" appState.locale
                , value = tag.color
                , onInput = createEditEvent setColor
                }

        annotationsInput =
            Input.annotations appState
                { annotations = tag.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
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
            createEditEventWithFocusSelector setApi setWidget Nothing value

        createEditEventWithFocusSelector setApi setWidget selector value =
            eventMsg True selector parentUuid (Just integrationUuid) <|
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
            eventMsg False Nothing parentUuid (Just integrationUuid) <|
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
            eventMsg False Nothing parentUuid (Just integrationUuid) <|
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
            [ ( "Api", gettext "API" appState.locale )
            , ( "Widget", gettext "Widget" appState.locale )
            ]

        integrationEditorTitle =
            editorTitle appState
                { title = gettext "Integration" appState.locale
                , uuid = integrationUuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just IntegrationState
                , mbMovingEntity = Nothing
                , mbGuideLink = Just GuideLinks.kmEditorIntegrationQuestion
                }

        typeInput =
            Input.select
                { name = "type"
                , label = gettext "Type" appState.locale
                , value = Integration.getTypeString integration
                , options = integrationTypeOptions
                , onChange = onTypeChange
                , extra = Nothing
                }

        idInput =
            Input.string
                { name = "id"
                , label = gettext "ID" appState.locale
                , value = Integration.getId integration
                , onInput = createEditEvent setId setId
                }

        nameInput =
            Input.string
                { name = "name"
                , label = gettext "Name" appState.locale
                , value = Integration.getName integration
                , onInput = createEditEvent setName setName
                }

        logoUrlInput =
            Input.string
                { name = "logo"
                , label = gettext "Logo URL" appState.locale
                , value = String.fromMaybe (Integration.getLogo integration)
                , onInput = createEditEvent setLogo setLogo << String.toMaybe
                }

        propsInput =
            Input.props appState
                { label = gettext "Props" appState.locale
                , values = Integration.getProps integration
                , onChange = createEditEventWithFocusSelector setProps setProps
                }

        itemUrl =
            Input.string
                { name = "itemUrl"
                , label = gettext "Item URL" appState.locale
                , value = String.fromMaybe (Integration.getItemUrl integration)
                , onInput = createEditEvent setItemUrl setItemUrl << String.toMaybe
                }

        annotationsInput =
            Input.annotations appState
                { annotations = Integration.getAnnotations integration
                , onEdit = createEditEventWithFocusSelector setAnnotations setAnnotations
                }

        integrationTypeInputs =
            case integration of
                ApiIntegration _ data ->
                    let
                        createTypeEditEvent map value =
                            createTypeEditEventWithFocusSelector map Nothing value

                        createTypeEditEventWithFocusSelector map selector value =
                            EditIntegrationApiEventData.init
                                |> map value
                                |> (EditIntegrationEvent << EditIntegrationApiEvent)
                                |> eventMsg True selector parentUuid (Just integrationUuid)

                        requestUrlInput =
                            Input.string
                                { name = "requestUrl"
                                , label = gettext "Request URL" appState.locale
                                , value = data.requestUrl
                                , onInput = createTypeEditEvent setRequestUrl
                                }

                        requestMethodInput =
                            Input.select
                                { name = "requestMethod"
                                , label = gettext "Request HTTP Method" appState.locale
                                , value = data.requestMethod
                                , options = httpMethodOptions
                                , onChange = createTypeEditEvent setRequestMethod
                                , extra = Nothing
                                }

                        requestHeadersInput =
                            Input.headers appState
                                { label = gettext "Request HTTP Headers" appState.locale
                                , headers = data.requestHeaders
                                , onEdit = createTypeEditEventWithFocusSelector setRequestHeaders
                                }

                        requestBodyInput =
                            Input.textarea
                                { name = "requestBody"
                                , label = gettext "Request HTTP Body" appState.locale
                                , value = data.requestBody
                                , onInput = createTypeEditEvent setRequestBody
                                }

                        requestEmptySearchInput =
                            Input.checkbox
                                { name = "requestEmptySearch"
                                , label = gettext "Allow Empty Search" appState.locale
                                , value = data.requestEmptySearch
                                , onInput = createTypeEditEvent setRequestEmptySearch
                                }

                        responseItemId =
                            Input.string
                                { name = "responseItemId"
                                , label = gettext "Response Item ID" appState.locale
                                , value = String.fromMaybe data.responseItemId
                                , onInput = createTypeEditEvent setResponseItemId << String.toMaybe
                                }

                        responseListFieldInput =
                            Input.string
                                { name = "responseListField"
                                , label = gettext "Response List Field" appState.locale
                                , value = String.fromMaybe data.responseListField
                                , onInput = createTypeEditEvent setResponseListField << String.toMaybe
                                }

                        responseItemTemplate =
                            Input.textarea
                                { name = "responseItemTemplate"
                                , label = gettext "Response Item Template" appState.locale
                                , value = data.responseItemTemplate
                                , onInput = createTypeEditEvent setResponseItemTemplate
                                }
                    in
                    [ div [ class "card card-border-light mb-5" ]
                        [ div [ class "card-header" ] [ text (gettext "Request" appState.locale) ]
                        , div [ class "card-body" ]
                            [ Markdown.toHtml [ class "alert alert-info mb-5" ] (gettext "Use this section to configure the search request. The service you want to integrate has to provide a search HTTP API where you send a search string and it returns a JSON with found items." appState.locale)
                            , requestUrlInput
                            , FormExtra.mdAfter (gettext "The full API endpoint used for search. Use `${q}` to insert the user's search term (for example, *https://example.com/search?q=${q}*), and `${propName}` to reference fields defined in props." appState.locale)
                            , requestMethodInput
                            , requestHeadersInput
                            , FormExtra.mdAfter (gettext "Optional headers to include in the API request. You can use `${q}` for the search term and `${propName}` for values from props." appState.locale)
                            , requestBodyInput
                            , FormExtra.mdAfter (gettext "Optional request body for POST or PUT methods. Supports the same variables: `${q}` for the search term and `${propName}` for props." appState.locale)
                            , requestEmptySearchInput
                            , FormExtra.mdAfter (gettext "Turn this off if the API cannot handle empty search requests. In that case, the requests will be made only after users type something." appState.locale)
                            ]
                        ]
                    , div [ class "card card-border-light mb-5" ]
                        [ div [ class "card-header" ] [ text (gettext "Response" appState.locale) ]
                        , div [ class "card-body" ]
                            [ Markdown.toHtml [ class "alert alert-info mb-5" ] (gettext "Use this section to configure how to process the response returned from the search API." appState.locale)
                            , responseListFieldInput
                            , FormExtra.mdAfter (gettext "If the returned JSON is not an array of items directly but the items are nested, use this to define the name or path to the field in the response that contains the list of items. Keep empty otherwise." appState.locale)
                            , responseItemId
                            , FormExtra.mdAfter (gettext "Use this to define an identifier for the item. This will be used in **Item URL** as `${id}` to compose a URL to the found item. You can use properties from the returned item in Jinja2 notation. For example, if the item has a field `id` use `{{item.id}}` here. You can also compose multiple fields together, e.g., `{{item.field1}}-{{item.field2}}`." appState.locale)
                            , responseItemTemplate
                            , FormExtra.mdAfter
                                (String.format (gettext "This defines how the found items will be displayed for the user. You can use properties from the returned item in Jinja2 notation, you can also use [Markdown](%s) for some formatting. For example, if the returned item has a field called name, you can use `**{{item.name}}**` to display the name in bold." appState.locale)
                                    [ GuideLinks.markdownCheatsheet appState.guideLinks ]
                                )
                            ]
                        ]
                    , itemUrl
                    , FormExtra.mdAfter (gettext "Defines the URL to the selected item. Use `${id}` to get the value defined in **Response Item ID** field, for example `https://example.com/${id}`." appState.locale)
                    ]

                WidgetIntegration _ data ->
                    let
                        createTypeEditEvent map value =
                            EditIntegrationWidgetEventData.init
                                |> map value
                                |> (EditIntegrationEvent << EditIntegrationWidgetEvent)
                                |> eventMsg True Nothing parentUuid (Just integrationUuid)

                        widgetUrlInput =
                            Input.string
                                { name = "widgetUrl"
                                , label = gettext "Widget URL" appState.locale
                                , value = data.widgetUrl
                                , onInput = createTypeEditEvent setWidgetUrl
                                }
                    in
                    [ widgetUrlInput
                    , FormExtra.mdAfter (gettext "The URL of the widget implemented using [DSW Integration SDK](https://github.com/ds-wizard/dsw-integration-sdk)." appState.locale)
                    , itemUrl
                    , FormExtra.mdAfter (gettext "Defines the URL to the selected item. Use `${id}` value returned from the widget, for example `https://example.com/${id}`." appState.locale)
                    ]

        wrapQuestionsWithIntegration questions =
            if List.isEmpty questions then
                div [] [ i [] [ text (gettext "No questions" appState.locale) ] ]

            else
                ul [] questions

        questionsWithIntegration =
            KnowledgeModel.getAllQuestions editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith Question.getUuid editorBranch
                |> List.filter ((==) (Just integrationUuid) << Question.getIntegrationUuid)
                |> List.filter (EditorBranch.isReachable editorBranch << Question.getUuid)
                |> List.sortBy Question.getTitle
                |> List.map (viewQuestionLink appState editorBranch)
                |> wrapQuestionsWithIntegration

        prefabsView =
            if (not << List.isEmpty) integrationPrefabs && EditorBranch.isEmptyIntegrationEditorUuid integrationUuid editorBranch then
                let
                    viewLogo i =
                        case Integration.getLogo i of
                            Just logo ->
                                img [ src logo ] []

                            Nothing ->
                                faKmIntegration

                    viewIntegrationButton i =
                        li []
                            [ a [ onClick (createEditEventFromPrefab i) ]
                                [ viewLogo i
                                , span [] [ text (Integration.getVisibleName i) ]
                                ]
                            ]
                in
                div [ class "prefab-selection" ]
                    [ strong [] [ text (gettext "Quick setup" appState.locale) ]
                    , ul [] (List.map viewIntegrationButton <| List.sortBy Integration.getVisibleName integrationPrefabs)
                    ]

            else
                case List.find ((==) (Integration.getId integration) << Integration.getId) integrationPrefabs of
                    Just usedPrefab ->
                        let
                            differFromPrefab =
                                Integration.getName integration
                                    /= Integration.getName usedPrefab
                                    || Integration.getResponseItemTemplate integration
                                    /= Integration.getResponseItemTemplate usedPrefab
                        in
                        if differFromPrefab then
                            div [ class "alert alert-info" ]
                                [ text (gettext "This integration was created from a template and now differs." appState.locale)
                                , button
                                    [ class "btn btn-primary ms-2"
                                    , onClick (createEditEventFromPrefab usedPrefab)
                                    ]
                                    [ text (gettext "Update" appState.locale) ]
                                ]

                        else
                            Html.nothing

                    Nothing ->
                        Html.nothing
    in
    editor ("integration-" ++ integrationUuid)
        ([ integrationEditorTitle
         , prefabsView
         , typeInput
         , idInput
         , FormExtra.mdAfter (gettext "A string that identifies the integration. It has to be unique for each integration." appState.locale)
         , nameInput
         , FormExtra.mdAfter (gettext "A name visible everywhere else in the knowledge model Editor, such as when choosing the integration for a question." appState.locale)
         , logoUrlInput
         , FormExtra.mdAfter (gettext "Logo is displayed next to the link to the selected item in questionnaires. It can be either URL or base64 image." appState.locale)
         , propsInput
         , FormExtra.mdAfter (gettext "Props can be used to parametrize the integration for each question. Use this to define the props whose value can be filled on the questions using this integration. The props can then be used in the URL configuration. For example, if you define prop named *type*, you can use it as `${type}`, such as *ht&#8203;tps://example.com/${type}*." appState.locale)
         ]
            ++ integrationTypeInputs
            ++ [ annotationsInput
               , FormGroup.plainGroup questionsWithIntegration (gettext "Questions using this integration" appState.locale)
               ]
        )


viewAnswerEditor : EditorConfig msg -> Answer -> Html msg
viewAnswerEditor { appState, wrapMsg, eventMsg, model, editorBranch } answer =
    let
        parentUuid =
            EditorBranch.getParentUuid answer.uuid editorBranch

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditAnswerEventData.init
                |> map value
                |> EditAnswerEvent
                |> eventMsg True selector parentUuid (Just answer.uuid)

        questionAddEvent =
            AddQuestionEventData.init
                |> AddQuestionEvent
                |> eventMsg False Nothing answer.uuid Nothing

        answerEditorTitle =
            editorTitle appState
                { title = gettext "Answer" appState.locale
                , uuid = answer.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just AnswerState
                , mbMovingEntity = Just TreeInput.MovingAnswer
                , mbGuideLink = Nothing
                }

        labelInput =
            Input.string
                { name = "label"
                , label = gettext "Label" appState.locale
                , value = answer.label
                , onInput = createEditEvent setLabel
                }

        adviceInput =
            Input.markdown appState
                { name = "advice"
                , label = gettext "Advice" appState.locale
                , value = String.fromMaybe answer.advice
                , onInput = createEditEvent setAdvice << String.toMaybe
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = answer.uuid
                , markdownPreviews = model.markdownPreviews
                }

        followUpsInput =
            Input.reorderable
                { name = "questions"
                , label = gettext "Follow-Up Questions" appState.locale
                , items = EditorBranch.filterDeleted editorBranch answer.followUpUuids
                , entityUuid = answer.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setFollowUpUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getQuestionName editorBranch.branch.knowledgeModel
                , untitledLabel = gettext "Untitled question" appState.locale
                , addChildLabel = gettext "Add question" appState.locale
                , addChildMsg = questionAddEvent
                , addChildDataCy = "question"
                }

        metrics =
            EditorBranch.filterDeletedWith .uuid editorBranch <|
                KnowledgeModel.getMetrics editorBranch.branch.knowledgeModel

        metricsInput =
            if List.isEmpty metrics then
                Html.nothing

            else
                Input.metrics appState
                    { metrics = metrics
                    , metricMeasures = answer.metricMeasures
                    , onChange = createEditEvent setMetricMeasures
                    }

        annotationsInput =
            Input.annotations appState
                { annotations = answer.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
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
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditChoiceEventData.init
                |> map value
                |> EditChoiceEvent
                |> eventMsg True selector parentUuid (Just choice.uuid)

        choiceEditorTitle =
            editorTitle appState
                { title = gettext "Choice" appState.locale
                , uuid = choice.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just ChoiceState
                , mbMovingEntity = Just TreeInput.MovingChoice
                , mbGuideLink = Nothing
                }

        labelInput =
            Input.string
                { name = "label"
                , label = gettext "Label" appState.locale
                , value = choice.label
                , onInput = createEditEvent setLabel
                }

        annotationsInput =
            Input.annotations appState
                { annotations = choice.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
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
            eventMsg False Nothing parentUuid (Just referenceUuid) <|
                case value of
                    "ResourcePage" ->
                        EditReferenceResourcePageEventData.init
                            |> (EditReferenceEvent << EditReferenceResourcePageEvent)

                    _ ->
                        EditReferenceURLEventData.init
                            |> (EditReferenceEvent << EditReferenceURLEvent)

        referenceTypeOptions =
            [ ( "ResourcePage", gettext "Resource Page" appState.locale )
            , ( "URL", gettext "URL" appState.locale )
            ]

        referenceEditorTitle =
            editorTitle appState
                { title = gettext "Reference" appState.locale
                , uuid = Reference.getUuid reference
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just ReferenceState
                , mbMovingEntity = Just TreeInput.MovingReference
                , mbGuideLink = Nothing
                }

        typeInput =
            Input.select
                { name = "type"
                , label = gettext "Reference Type" appState.locale
                , value = Reference.getTypeString reference
                , options = referenceTypeOptions
                , onChange = onTypeChange
                , extra = Nothing
                }

        referenceTypeInputs =
            case reference of
                ResourcePageReference data ->
                    let
                        createTypeEditEvent map value =
                            createEditEventWithFocusSelector map Nothing value

                        createEditEventWithFocusSelector map selector value =
                            EditReferenceResourcePageEventData.init
                                |> map value
                                |> (EditReferenceEvent << EditReferenceResourcePageEvent)
                                |> eventMsg True selector parentUuid (Just referenceUuid)

                        resourcePageOption resourcePageUuid =
                            KnowledgeModel.getResourcePage resourcePageUuid editorBranch.branch.knowledgeModel
                                |> Maybe.map
                                    (\rp ->
                                        let
                                            title =
                                                if String.isEmpty rp.title then
                                                    gettext "Untitled resource page" appState.locale

                                                else
                                                    rp.title
                                        in
                                        ( rp.uuid, title )
                                    )

                        resourcePageUuidOptions =
                            KnowledgeModel.getResourceCollections (EditorBranch.getFilteredKM editorBranch)
                                |> List.map (\rc -> ( rc.title, List.filterMap resourcePageOption rc.resourcePageUuids ))

                        resourcePageUuidSelect =
                            Input.selectWithGroups
                                { name = "resourcePageUuid"
                                , label = gettext "Resource Page" appState.locale
                                , value = Maybe.withDefault "" data.resourcePageUuid
                                , defaultOption = ( "", gettext "- select resource page -" appState.locale )
                                , options = resourcePageUuidOptions
                                , onChange = createTypeEditEvent setResourcePageUuid << String.toMaybe
                                }

                        annotationsInput =
                            Input.annotations appState
                                { annotations = Reference.getAnnotations reference
                                , onEdit = createEditEventWithFocusSelector setAnnotations
                                }
                    in
                    [ resourcePageUuidSelect, annotationsInput ]

                URLReference data ->
                    let
                        createTypeEditEvent map value =
                            createEditEventWithFocusSelector map Nothing value

                        createEditEventWithFocusSelector map selector value =
                            EditReferenceURLEventData.init
                                |> map value
                                |> (EditReferenceEvent << EditReferenceURLEvent)
                                |> eventMsg True selector parentUuid (Just referenceUuid)

                        urlInput =
                            Input.string
                                { name = "url"
                                , label = gettext "URL" appState.locale
                                , value = data.url
                                , onInput = createTypeEditEvent setUrl
                                }

                        labelInput =
                            Input.string
                                { name = "label"
                                , label = gettext "Label" appState.locale
                                , value = data.label
                                , onInput = createTypeEditEvent setLabel
                                }

                        annotationsInput =
                            Input.annotations appState
                                { annotations = Reference.getAnnotations reference
                                , onEdit = createEditEventWithFocusSelector setAnnotations
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
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditExpertEventData.init
                |> map value
                |> EditExpertEvent
                |> eventMsg True selector parentUuid (Just expert.uuid)

        expertEditorTitle =
            editorTitle appState
                { title = gettext "Expert" appState.locale
                , uuid = expert.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just ExpertState
                , mbMovingEntity = Just TreeInput.MovingExpert
                , mbGuideLink = Nothing
                }

        nameInput =
            Input.string
                { name = "name"
                , label = gettext "Name" appState.locale
                , value = expert.name
                , onInput = createEditEvent setName
                }

        emailInput =
            Input.string
                { name = "email"
                , label = gettext "Email" appState.locale
                , value = expert.email
                , onInput = createEditEvent setEmail
                }

        annotationsInput =
            Input.annotations appState
                { annotations = expert.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
                }
    in
    editor ("expert-" ++ expert.uuid)
        [ expertEditorTitle
        , nameInput
        , emailInput
        , annotationsInput
        ]


viewResourceCollectionEditor : EditorConfig msg -> ResourceCollection -> Html msg
viewResourceCollectionEditor { appState, wrapMsg, eventMsg, model, editorBranch } resourceCollection =
    let
        parentUuid =
            EditorBranch.getParentUuid resourceCollection.uuid editorBranch

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditResourceCollectionEventData.init
                |> map value
                |> EditResourceCollectionEvent
                |> eventMsg True selector parentUuid (Just resourceCollection.uuid)

        resourcePageAddEvent =
            AddResourcePageEventData.init
                |> AddResourcePageEvent
                |> eventMsg False Nothing resourceCollection.uuid Nothing

        resourceCollectionEditorTitle =
            editorTitle appState
                { title = gettext "Resource Collection" appState.locale
                , uuid = resourceCollection.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just ResourceCollectionState
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = gettext "Title" appState.locale
                , value = resourceCollection.title
                , onInput = createEditEvent setTitle
                }

        resourcePagesInput =
            Input.reorderable
                { name = "resourcePages"
                , label = gettext "Resource Pages" appState.locale
                , items = EditorBranch.filterDeleted editorBranch resourceCollection.resourcePageUuids
                , entityUuid = resourceCollection.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setResourcePageUuids
                , getRoute = EditorBranch.editorRoute editorBranch
                , getName = KnowledgeModel.getResourcePageName editorBranch.branch.knowledgeModel
                , untitledLabel = gettext "Untitled resource page" appState.locale
                , addChildLabel = gettext "Add resource page" appState.locale
                , addChildMsg = resourcePageAddEvent
                , addChildDataCy = "resource-page"
                }

        annotationsInput =
            Input.annotations appState
                { annotations = resourceCollection.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
                }
    in
    editor ("resource-collection-" ++ resourceCollection.uuid)
        [ resourceCollectionEditorTitle
        , titleInput
        , resourcePagesInput
        , annotationsInput
        ]


viewResourcePageEditor : EditorConfig msg -> ResourcePage -> Html msg
viewResourcePageEditor { appState, wrapMsg, eventMsg, model, editorBranch } resourcePage =
    let
        parentUuid =
            EditorBranch.getParentUuid resourcePage.uuid editorBranch

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditResourcePageEventData.init
                |> map value
                |> EditResourcePageEvent
                |> eventMsg True selector parentUuid (Just resourcePage.uuid)

        resourcePageEditorTitle =
            editorTitle appState
                { title = gettext "Resource Page" appState.locale
                , uuid = resourcePage.uuid
                , wrapMsg = wrapMsg
                , copyUuidButton = True
                , mbDeleteModalState = Just ResourcePageState
                , mbMovingEntity = Nothing
                , mbGuideLink = Nothing
                }

        titleInput =
            Input.string
                { name = "title"
                , label = gettext "Title" appState.locale
                , value = resourcePage.title
                , onInput = createEditEvent setTitle
                }

        contentInput =
            Input.markdown appState
                { name = "content"
                , label = gettext "Content" appState.locale
                , value = resourcePage.content
                , onInput = createEditEvent setContent
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = resourcePage.uuid
                , markdownPreviews = model.markdownPreviews
                }

        annotationsInput =
            Input.annotations appState
                { annotations = resourcePage.annotations
                , onEdit = createEditEventWithFocusSelector setAnnotations
                }

        wrapQuestionsWithIntegration questions =
            if List.isEmpty questions then
                div [] [ i [] [ text (gettext "No questions" appState.locale) ] ]

            else
                ul [] questions

        filterQuestionByResourcePageUuid questionUuid =
            KnowledgeModel.getQuestionReferences questionUuid editorBranch.branch.knowledgeModel
                |> List.filterMap Reference.getResourcePageUuid
                |> List.member resourcePage.uuid

        questionsWithResourcePage =
            KnowledgeModel.getAllQuestions editorBranch.branch.knowledgeModel
                |> EditorBranch.filterDeletedWith Question.getUuid editorBranch
                |> List.filter (filterQuestionByResourcePageUuid << Question.getUuid)
                |> List.filter (EditorBranch.isReachable editorBranch << Question.getUuid)
                |> List.sortBy Question.getTitle
                |> List.map (viewQuestionLink appState editorBranch)
                |> wrapQuestionsWithIntegration
    in
    editor ("resource-page-" ++ resourcePage.uuid)
        [ resourcePageEditorTitle
        , titleInput
        , contentInput
        , annotationsInput
        , FormGroup.plainGroup questionsWithResourcePage (gettext "Questions using this resource page" appState.locale)
        ]


viewEmptyEditor : AppState -> Html msg
viewEmptyEditor appState =
    editor "empty"
        [ Flash.error (gettext "The knowledge model entity you are trying to open does not exist." appState.locale)
        ]


editor : String -> List (Html msg) -> Html msg
editor editorId =
    div [ id editorId, class "editor-content col-xl-10 col-12" ]


type alias EditorTitleConfig msg =
    { title : String
    , uuid : String
    , wrapMsg : Msg -> msg
    , copyUuidButton : Bool
    , mbDeleteModalState : Maybe (String -> DeleteModalState)
    , mbMovingEntity : Maybe TreeInput.MovingEntity
    , mbGuideLink : Maybe (GuideLinks -> String)
    }


editorTitle : AppState -> EditorTitleConfig msg -> Html msg
editorTitle appState config =
    let
        copyUuidButton =
            if config.copyUuidButton then
                a
                    ([ class "btn btn-link with-icon"
                     , onClick <| config.wrapMsg <| CopyUuid config.uuid
                     ]
                        ++ tooltip (gettext "Click to copy UUID" appState.locale)
                    )
                    [ faKmEditorCopyUuid
                    , small [] [ text <| String.slice 0 8 config.uuid ]
                    ]

            else
                Html.nothing

        moveButton =
            case config.mbMovingEntity of
                Just movingEntity ->
                    button
                        [ class "btn btn-outline-secondary with-icon"
                        , onClick <| config.wrapMsg <| OpenMoveModal movingEntity config.uuid
                        , dataCy "km-editor_move-button"
                        ]
                        [ faKmEditorMove
                        , text (gettext "Move" appState.locale)
                        ]

                Nothing ->
                    Html.nothing

        deleteButton =
            case config.mbDeleteModalState of
                Just deleteModalState ->
                    button
                        [ class "btn btn-outline-danger with-icon"
                        , dataCy "km-editor_delete-button"
                        , onClick <| config.wrapMsg <| SetDeleteModalState <| deleteModalState config.uuid
                        ]
                        [ faDelete
                        , text (gettext "Delete" appState.locale)
                        ]

                Nothing ->
                    Html.nothing

        guideLink_ =
            case config.mbGuideLink of
                Just getGuideLink ->
                    guideLink appState getGuideLink

                Nothing ->
                    Html.nothing
    in
    div [ class "editor-title" ]
        [ h3 [] [ text config.title ]
        , div [ class "editor-title-buttons" ]
            [ copyUuidButton
            , moveButton
            , deleteButton
            , guideLink_
            ]
        ]


viewQuestionLink : AppState -> EditorBranch -> Question -> Html msg
viewQuestionLink appState editorBranch question =
    let
        questionTitle =
            Question.getTitle question

        questionTitleNode =
            if String.isEmpty questionTitle then
                i [] [ text (gettext "Untitled question" appState.locale) ]

            else
                text questionTitle
    in
    li []
        [ linkTo (EditorBranch.editorRoute editorBranch (Question.getUuid question))
            []
            [ questionTitleNode ]
        ]



-- DELETE MODAL


deleteModal : AppState -> (Msg -> msg) -> EventMsg msg -> EditorBranch -> DeleteModalState -> Html msg
deleteModal appState wrapMsg eventMsg editorBranch deleteModalState =
    let
        createEvent event uuid =
            eventMsg False Nothing (EditorBranch.getParentUuid uuid editorBranch) (Just uuid) event

        ( visible, ( content, mbConfirmMsg ) ) =
            case deleteModalState of
                ChapterState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this chapter?" appState.locale)
                        (createEvent DeleteChapterEvent uuid)
                    )

                QuestionState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this question?" appState.locale)
                        (createEvent DeleteQuestionEvent uuid)
                    )

                MetricState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this metric?" appState.locale)
                        (createEvent DeleteMetricEvent uuid)
                    )

                PhaseState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this phase?" appState.locale)
                        (createEvent DeletePhaseEvent uuid)
                    )

                TagState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this question tag?" appState.locale)
                        (createEvent DeleteTagEvent uuid)
                    )

                IntegrationState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this integration?" appState.locale)
                        (createEvent DeleteIntegrationEvent uuid)
                    )

                ResourceCollectionState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this resource collection?" appState.locale)
                        (createEvent DeleteResourceCollectionEvent uuid)
                    )

                ResourcePageState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this resource page?" appState.locale)
                        (createEvent DeleteResourcePageEvent uuid)
                    )

                AnswerState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this answer?" appState.locale)
                        (createEvent DeleteAnswerEvent uuid)
                    )

                ChoiceState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this choice?" appState.locale)
                        (createEvent DeleteChoiceEvent uuid)
                    )

                ReferenceState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this reference?" appState.locale)
                        (createEvent DeleteReferenceEvent uuid)
                    )

                ExpertState uuid ->
                    ( True
                    , getContent
                        (gettext "Are you sure you want to delete this expert?" appState.locale)
                        (createEvent DeleteExpertEvent uuid)
                    )

                Closed ->
                    ( False, ( [ Html.nothing ], Nothing ) )

        getContent contentText onDelete =
            ( [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title" ] [ text (gettext "Heads up!" appState.locale) ]
                    ]
              , div [ class "modal-body" ]
                    [ text contentText ]
              , div [ class "modal-footer" ]
                    [ button
                        [ class "btn btn-danger"
                        , dataCy "modal_action-button"
                        , onClick onDelete
                        ]
                        [ text (gettext "Delete" appState.locale) ]
                    , button
                        [ class "btn btn-secondary"
                        , dataCy "modal_cancel-button"
                        , onClick <| wrapMsg <| SetDeleteModalState Closed
                        ]
                        [ text (gettext "Cancel" appState.locale) ]
                    ]
              ]
            , Just onDelete
            )

        modalConfig =
            { modalContent = content
            , visible = visible
            , enterMsg = mbConfirmMsg
            , escMsg = Just <| wrapMsg <| SetDeleteModalState Closed
            , dataCy = "km-editor-delete"
            }
    in
    Modal.simple modalConfig



-- MOVE MODAL


moveModal : AppState -> (Msg -> msg) -> EventMsg msg -> EditorBranch -> Maybe MoveModalState -> Html msg
moveModal appState wrapMsg eventMsg editorBranch mbMoveModalState =
    let
        ( content, mbConfirmMsg ) =
            case mbMoveModalState of
                Just moveModalState ->
                    let
                        parentUuid =
                            EditorBranch.getParentUuid moveModalState.movingUuid editorBranch

                        selectedUuid =
                            moveModalState.treeInputModel.selected

                        createEvent event =
                            eventMsg False Nothing parentUuid (Just moveModalState.movingUuid) (event { targetUuid = selectedUuid })

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
                    ( [ div [ class "modal-header" ]
                            [ h5 [ class "modal-title" ] [ text (gettext "Move" appState.locale) ]
                            ]
                      , div [ class "modal-body" ]
                            [ label [] [ text (gettext "Select a new parent" appState.locale) ]
                            , Html.map (wrapMsg << MoveModalMsg) <| TreeInput.view appState viewProps moveModalState.treeInputModel
                            ]
                      , div [ class "modal-footer" ]
                            [ button
                                [ class "btn btn-primary"
                                , onClick onMove
                                , disabled (String.isEmpty selectedUuid)
                                , dataCy "modal_action-button"
                                ]
                                [ text (gettext "Move" appState.locale) ]
                            , button
                                [ class "btn btn-secondary"
                                , onClick <| wrapMsg CloseMoveModal
                                , dataCy "modal_cancel-button"
                                ]
                                [ text (gettext "Cancel" appState.locale) ]
                            ]
                      ]
                    , Just onMove
                    )

                Nothing ->
                    ( [], Nothing )

        modalConfig =
            { modalContent = content
            , visible = Maybe.isJust mbMoveModalState
            , enterMsg = mbConfirmMsg
            , escMsg = Just <| wrapMsg CloseMoveModal
            , dataCy = "km-editor-move"
            }
    in
    Modal.simple modalConfig
