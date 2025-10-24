module Wizard.Pages.KMEditor.Editor.Components.KMEditor exposing
    ( CurlImportModalState
    , DeleteModalState
    , EventMsg
    , Model
    , MoveModalState
    , Msg(..)
    , RightPanel
    , UpdateConfig
    , closeAllModals
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ActionButton as ActionButton
import Common.Components.Badge as Badge
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faCopy, faDelete, faKmEditorCopyUuid, faKmEditorMove, faKmIntegration, faQuestionnaireExpand, faQuestionnaireShrink, faWarning, fas)
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.GuideLink as GuideLink
import Common.Components.Modal as Modal
import Common.Components.Tooltip exposing (tooltip, tooltipLeft)
import Common.Ports.Copy as Copy
import Common.Ports.Dom as Dom
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.CurlUtils as CurlUtils
import Common.Utils.GuideLinks exposing (GuideLinks)
import Common.Utils.HttpMethod as HttpMethod
import Common.Utils.HttpStatus as HttpStatus
import Common.Utils.Markdown as Markdown
import Compose exposing (compose2, compose3)
import Dict exposing (Dict)
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, code, div, h3, h5, hr, i, img, label, li, pre, small, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, src, target, value)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick, onInput, onMouseLeave)
import Html.Extra as Html
import Html.Keyed
import Json.Print
import Json.Value as JsonValue
import List.Extra as List
import Maybe.Extra as Maybe
import Reorderable
import Set
import SplitPane
import String.Extra as String
import String.Format as String
import SyntaxHighlight
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
import Wizard.Api.Models.Event.EditEventSetters exposing (setAbbreviation, setAdvice, setAllowCustomReply, setAnnotations, setAnswerUuids, setChapterUuids, setChoiceUuids, setColor, setContent, setDescription, setEmail, setExpertUuids, setFileTypes, setFollowUpUuids, setId, setIntegrationUuid, setIntegrationUuids, setItemTemplateQuestionUuids, setItemUrl, setLabel, setListQuestionUuid, setLogo, setMaxSize, setMetricMeasures, setMetricUuids, setName, setPhaseUuids, setQuestionUuids, setReferenceUuids, setRequestAllowEmptySearch, setRequestBody, setRequestEmptySearch, setRequestHeaders, setRequestMethod, setRequestUrl, setRequiredPhaseUuid, setResourceCollectionUuids, setResourcePageUuid, setResourcePageUuids, setResponseItemId, setResponseItemTemplate, setResponseItemTemplateForSelection, setResponseListField, setTagUuids, setTargetUuid, setTestQ, setTestResponse, setTestVariables, setText, setTitle, setUrl, setValidations, setValueType, setVariables, setWidgetUrl)
import Wizard.Api.Models.Event.EditExpertEventData as EditExpertEventData
import Wizard.Api.Models.Event.EditIntegrationApiEventData as EditIntegrationApiEventData
import Wizard.Api.Models.Event.EditIntegrationApiLegacyEventData as EditIntegrationApiLegacyEventData
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
import Wizard.Api.Models.Event.EditReferenceCrossEventData as EditReferenceCrossEventData
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
import Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiLegacyIntegrationData exposing (ApiLegacyIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair
import Wizard.Api.Models.KnowledgeModel.Integration.WidgetIntegrationData exposing (WidgetIntegrationData)
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Api.Models.TypeHint exposing (TypeHint)
import Wizard.Api.Models.TypeHintRequest as TypeHintRequest
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)
import Wizard.Api.Models.UrlCheckResponse.UrlResult as UrlResult
import Wizard.Api.TypeHints as TypeHintsApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)
import Wizard.Pages.KMEditor.Editor.Components.KMEditor.Breadcrumbs as Breadcrumbs
import Wizard.Pages.KMEditor.Editor.Components.KMEditor.Input as Input
import Wizard.Pages.KMEditor.Editor.Components.KMEditor.Tree as Tree
import Wizard.Pages.KMEditor.Editor.Components.KMEditor.TreeInput as TreeInput
import Wizard.Pages.KMEditor.Editor.Components.KMEditor.UrlChecker as UrlChecker
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks



-- MODEL


type alias Model =
    { splitPane : SplitPane.State
    , markdownPreviews : List String
    , reorderableStates : Dict String Reorderable.State
    , deleteModalState : DeleteModalState
    , moveModalState : Maybe MoveModalState
    , rightPanel : RightPanel
    , integrationTestResults : Dict String (ActionResult TypeHintTestResponse)
    , integrationTestPreviews : Dict String (ActionResult (List TypeHint))
    , lastCopiedString : Maybe String
    , curlImportModalState : CurlImportModalState
    , cursorPositions : Dict String ( Int, Int )
    , urlChecker : UrlChecker.Model
    }


type RightPanel
    = NoRightPanel
    | WarningsRightPanel
    | URLCheckerRightPanel


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


type alias CurlImportModalState =
    { integrationUuid : Maybe String
    , curlString : String
    }


initialModel : Model
initialModel =
    { splitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.2 (Just ( 0.05, 0.7 )))
    , markdownPreviews = []
    , reorderableStates = Dict.empty
    , deleteModalState = Closed
    , moveModalState = Nothing
    , rightPanel = NoRightPanel
    , integrationTestResults = Dict.empty
    , integrationTestPreviews = Dict.empty
    , lastCopiedString = Nothing
    , curlImportModalState = { integrationUuid = Nothing, curlString = "" }
    , cursorPositions = Dict.empty
    , urlChecker = UrlChecker.initialModel
    }


closeAllModals : Model -> Model
closeAllModals model =
    { model | deleteModalState = Closed, moveModalState = Nothing }


getIntegrationTestResult : String -> Model -> ActionResult TypeHintTestResponse
getIntegrationTestResult integrationUuid model =
    Dict.get integrationUuid model.integrationTestResults
        |> Maybe.withDefault ActionResult.Unset



-- UPDATE


type Msg
    = SplitPaneMsg SplitPane.Msg
    | SetFullscreen Bool
    | SetTreeOpen String Bool
    | ExpandAll
    | CollapseAll
    | CopyUuid String
    | CopyString String
    | ClearLastCopiedString
    | ShowHideMarkdownPreview Bool String
    | ReorderableMsg String Reorderable.Msg
    | SetDeleteModalState DeleteModalState
    | OpenMoveModal TreeInput.MovingEntity String
    | MoveModalMsg TreeInput.Msg
    | CloseMoveModal
    | SetRightPanels RightPanel
    | TestIntegrationRequest String String (Dict String String)
    | TestIntegrationRequestCompleted String (Result ApiError TypeHintTestResponse)
    | TestIntegrationPreview String String
    | TestIntegrationPreviewCompleted String (Result ApiError (List TypeHint))
    | CurlImportModalSetIntegration (Maybe String)
    | CurlImportModalUpdateString String
    | CurlImportModalConfirm
    | SetCursorPosition String Int Int
    | UrlCheckerMsg UrlChecker.Msg


type alias EventMsg msg =
    Bool -> Maybe String -> Maybe Int -> String -> Maybe String -> (CommonEventData -> Event) -> msg


type alias UpdateConfig msg =
    { setFullscreenMsg : Bool -> msg
    , wrapMsg : Msg -> msg
    , eventMsg : EventMsg msg
    }


update : AppState -> UpdateConfig msg -> Msg -> ( EditorContext, Model ) -> ( EditorContext, Model, Cmd msg )
update appState cfg msg ( editorContext, model ) =
    let
        showHideMarkdownPreview visible field m =
            if visible then
                { m | markdownPreviews = field :: m.markdownPreviews }

            else
                { m | markdownPreviews = List.filter ((/=) field) m.markdownPreviews }
    in
    case msg of
        SplitPaneMsg splitPaneMsg ->
            ( editorContext, { model | splitPane = SplitPane.update splitPaneMsg model.splitPane }, Cmd.none )

        SetFullscreen fullscreen ->
            ( editorContext, model, Task.dispatch (cfg.setFullscreenMsg fullscreen) )

        SetTreeOpen entityUuid open ->
            ( EditorContext.treeSetNodeOpen entityUuid open editorContext, model, Cmd.none )

        ExpandAll ->
            ( EditorContext.treeExpandAll editorContext, model, Cmd.none )

        CollapseAll ->
            ( EditorContext.treeCollapseAll editorContext, model, Cmd.none )

        CopyUuid uuid ->
            ( editorContext, model, Copy.copyToClipboard uuid )

        CopyString value ->
            ( editorContext, { model | lastCopiedString = Just value }, Copy.copyToClipboard value )

        ClearLastCopiedString ->
            ( editorContext, { model | lastCopiedString = Nothing }, Cmd.none )

        ShowHideMarkdownPreview visible field ->
            ( editorContext, showHideMarkdownPreview visible field model, Cmd.none )

        ReorderableMsg field reorderableMsg ->
            let
                reorderableState =
                    Dict.get field model.reorderableStates
                        |> Maybe.withDefault Reorderable.initialState
                        |> Reorderable.update reorderableMsg
            in
            ( editorContext
            , { model | reorderableStates = Dict.insert field reorderableState model.reorderableStates }
            , Cmd.none
            )

        SetDeleteModalState deleteModalState ->
            ( editorContext, { model | deleteModalState = deleteModalState }, Cmd.none )

        OpenMoveModal movingEntity movingUuid ->
            let
                scrollCmd =
                    Dom.scrollIntoViewCenter "[data-km-editor_move-modal_item_current]"
            in
            ( editorContext
            , { model
                | moveModalState =
                    Just
                        { movingEntity = movingEntity
                        , movingUuid = movingUuid
                        , treeInputModel = TreeInput.initialModel (Set.fromList editorContext.openNodeUuids)
                        }
              }
            , scrollCmd
            )

        MoveModalMsg moveModalMsg ->
            ( editorContext
            , { model
                | moveModalState =
                    case model.moveModalState of
                        Just oldState ->
                            Just { oldState | treeInputModel = TreeInput.update moveModalMsg editorContext oldState.treeInputModel }

                        Nothing ->
                            Nothing
              }
            , Cmd.none
            )

        CloseMoveModal ->
            ( editorContext, { model | moveModalState = Nothing }, Cmd.none )

        SetRightPanels open ->
            ( editorContext, { model | rightPanel = open }, Cmd.none )

        TestIntegrationRequest integrationUuid q variables ->
            let
                cmd =
                    TypeHintsApi.testTypeHints
                        appState
                        editorContext.kmEditor.uuid
                        integrationUuid
                        q
                        variables
                        (cfg.wrapMsg << TestIntegrationRequestCompleted integrationUuid)

                newModel =
                    { model | integrationTestResults = Dict.insert integrationUuid ActionResult.Loading model.integrationTestResults }
            in
            ( editorContext, newModel, cmd )

        TestIntegrationRequestCompleted integrationUuid result ->
            case result of
                Ok typeHintTestResponse ->
                    let
                        newModel =
                            { model
                                | integrationTestResults =
                                    Dict.insert integrationUuid (ActionResult.Success typeHintTestResponse) model.integrationTestResults
                            }

                        setTestResponseMsg =
                            EditIntegrationApiEventData.init
                                |> setTestResponse (Just typeHintTestResponse)
                                |> (EditIntegrationEvent << EditIntegrationApiEvent)
                                |> cfg.eventMsg False Nothing Nothing (EditorContext.getParentUuid integrationUuid editorContext) (Just integrationUuid)
                    in
                    ( editorContext, newModel, Task.dispatch setTestResponseMsg )

                Err error ->
                    let
                        newModel =
                            { model
                                | integrationTestResults =
                                    Dict.insert integrationUuid
                                        (ApiError.toActionResult appState (gettext "Unable to get test result." appState.locale) error)
                                        model.integrationTestResults
                            }
                    in
                    ( editorContext, newModel, Cmd.none )

        TestIntegrationPreview integrationUuid fieldIdentifier ->
            let
                request =
                    TypeHintRequest.fromKmEditorIntegration
                        editorContext.kmEditor.uuid
                        (Uuid.fromUuidString integrationUuid)

                cmd =
                    TypeHintsApi.fetchTypeHints appState
                        request
                        (cfg.wrapMsg << TestIntegrationPreviewCompleted fieldIdentifier)

                newModel =
                    showHideMarkdownPreview True
                        fieldIdentifier
                        { model | integrationTestPreviews = Dict.insert fieldIdentifier ActionResult.Loading model.integrationTestPreviews }
            in
            ( editorContext, newModel, cmd )

        TestIntegrationPreviewCompleted fieldIdentifier result ->
            case result of
                Ok typeHints ->
                    let
                        newModel =
                            { model
                                | integrationTestPreviews =
                                    Dict.insert fieldIdentifier (ActionResult.Success typeHints) model.integrationTestPreviews
                            }
                    in
                    ( editorContext, newModel, Cmd.none )

                Err error ->
                    let
                        newModel =
                            { model
                                | integrationTestPreviews =
                                    Dict.insert fieldIdentifier
                                        (ApiError.toActionResult appState (gettext "Unable to get preview." appState.locale) error)
                                        model.integrationTestPreviews
                            }
                    in
                    ( editorContext, newModel, Cmd.none )

        CurlImportModalSetIntegration integrationUuid ->
            ( editorContext
            , { model | curlImportModalState = { integrationUuid = integrationUuid, curlString = "" } }
            , Cmd.none
            )

        CurlImportModalUpdateString curlString ->
            let
                curlImportModalState =
                    model.curlImportModalState
            in
            ( editorContext, { model | curlImportModalState = { curlImportModalState | curlString = curlString } }, Cmd.none )

        CurlImportModalConfirm ->
            case model.curlImportModalState.integrationUuid of
                Just integrationUuid ->
                    let
                        curlRequest =
                            CurlUtils.parseCurl model.curlImportModalState.curlString

                        setIfNotEmpty setter value =
                            if String.isEmpty value then
                                identity

                            else
                                setter (Just value)

                        setRequestBodyMsg =
                            EditIntegrationApiEventData.init
                                |> setRequestMethod curlRequest.method
                                |> setRequestUrl curlRequest.url
                                |> setRequestHeaders (List.map KeyValuePair.fromTuple curlRequest.headers)
                                |> setIfNotEmpty setRequestBody curlRequest.body
                                |> (EditIntegrationEvent << EditIntegrationApiEvent)
                                |> cfg.eventMsg False Nothing Nothing (EditorContext.getParentUuid integrationUuid editorContext) (Just integrationUuid)

                        newModel =
                            { model
                                | curlImportModalState = { integrationUuid = Nothing, curlString = "" }
                                , markdownPreviews = (integrationUuid ++ ":requestAdvancedConfiguration") :: model.markdownPreviews
                            }
                    in
                    ( editorContext, newModel, Task.dispatch setRequestBodyMsg )

                Nothing ->
                    ( editorContext, model, Cmd.none )

        SetCursorPosition field start end ->
            ( editorContext, { model | cursorPositions = Dict.insert field ( start, end ) model.cursorPositions }, Cmd.none )

        UrlCheckerMsg urlCheckerMsg ->
            let
                ( newUrlChecker, urlCheckerCmd ) =
                    UrlChecker.update appState urlCheckerMsg model.urlChecker
            in
            ( editorContext, { model | urlChecker = newUrlChecker }, Cmd.map (cfg.wrapMsg << UrlCheckerMsg) urlCheckerCmd )



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


view : AppState -> (Msg -> msg) -> EventMsg msg -> Model -> List Integration -> List KnowledgeModelSecret -> EditorContext -> Html msg
view appState wrapMsg eventMsg model integrationPrefabs kmSecrets editorContext =
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
                { createChapter = \kmUuid -> eventMsg False Nothing Nothing kmUuid Nothing (AddChapterEvent AddChapterEventData.init)
                , createQuestion = \parentUuid -> eventMsg False Nothing Nothing parentUuid Nothing (AddQuestionEvent AddQuestionEventData.init)
                , createAnswer = \questionUuid -> eventMsg False Nothing Nothing questionUuid Nothing (AddAnswerEvent AddAnswerEventData.init)
                , createChoice = \questionUuid -> eventMsg False Nothing Nothing questionUuid Nothing (AddChoiceEvent AddChoiceEventData.init)
                , createExpert = \questionUuid -> eventMsg False Nothing Nothing questionUuid Nothing (AddExpertEvent AddExpertEventData.init)
                , createReference = \questionUuid -> eventMsg False Nothing Nothing questionUuid Nothing (AddReferenceEvent AddReferenceEventData.init)
                , createResourceCollection = \questionUuid -> eventMsg False Nothing Nothing questionUuid Nothing (AddResourceCollectionEvent AddResourceCollectionEventData.init)
                , createResourcePage = \referenceUuid -> eventMsg False Nothing Nothing referenceUuid Nothing (AddResourcePageEvent AddResourcePageEventData.init)
                , createIntegration = \kmUuid -> eventMsg False Nothing Nothing kmUuid Nothing (AddIntegrationEvent AddIntegrationEventData.init)
                , createTag = \kmUuid -> eventMsg False Nothing Nothing kmUuid Nothing (AddTagEvent AddTagEventData.init)
                , createMetric = \kmUuid -> eventMsg False Nothing Nothing kmUuid Nothing (AddMetricEvent AddMetricEventData.init)
                , createPhase = \kmUuid -> eventMsg False Nothing Nothing kmUuid Nothing (AddPhaseEvent AddPhaseEventData.init)
                }
            }

        splitPaneConfig =
            SplitPane.createViewConfig
                { toMsg = wrapMsg << SplitPaneMsg
                , customSplitter = Nothing
                }

        allUrlReferences =
            KnowledgeModel.getAllUrlReferences editorContext.kmEditor.knowledgeModel

        urlCheckerButton =
            if Feature.urlChecker appState && List.length allUrlReferences > 0 then
                let
                    newRightPanel =
                        if model.rightPanel == URLCheckerRightPanel then
                            NoRightPanel

                        else
                            URLCheckerRightPanel

                    brokenReferencesCount =
                        UrlChecker.countBrokenReferences allUrlReferences model.urlChecker

                    brokenReferencesBadge =
                        if brokenReferencesCount > 0 then
                            Badge.danger
                                [ class "rounded-pill", dataCy "km-editor_url-checker_problematic-references-badge" ]
                                [ text (String.fromInt brokenReferencesCount) ]

                        else
                            Html.nothing
                in
                a
                    [ class "item"
                    , classList [ ( "selected", model.rightPanel == URLCheckerRightPanel ) ]
                    , onClick (wrapMsg (SetRightPanels newRightPanel))
                    ]
                    [ text (gettext "URL Checker" appState.locale)
                    , brokenReferencesBadge
                    ]

            else
                Html.nothing

        warningsCount =
            List.length editorContext.warnings

        warningsButton =
            if warningsCount > 0 then
                let
                    newRightPanel =
                        if model.rightPanel == WarningsRightPanel then
                            NoRightPanel

                        else
                            WarningsRightPanel
                in
                a
                    [ class "item"
                    , classList [ ( "selected", model.rightPanel == WarningsRightPanel ) ]
                    , onClick (wrapMsg (SetRightPanels newRightPanel))
                    ]
                    [ text (gettext "Warnings" appState.locale)
                    , Badge.danger [ class "rounded-pill" ] [ text (String.fromInt warningsCount) ]
                    ]

            else
                Html.nothing

        rightPanel =
            case model.rightPanel of
                NoRightPanel ->
                    Html.nothing

                WarningsRightPanel ->
                    if warningsCount > 0 then
                        Html.map wrapMsg <|
                            viewWarningsPanel appState editorContext

                    else
                        Html.nothing

                URLCheckerRightPanel ->
                    let
                        viewConfig =
                            { references = KnowledgeModel.getAllUrlReferences editorContext.kmEditor.knowledgeModel
                            , kmEditorUuid = editorContext.kmEditor.uuid
                            }
                    in
                    Html.map (wrapMsg << UrlCheckerMsg) <|
                        UrlChecker.view appState viewConfig model.urlChecker
    in
    div [ class "KMEditor__Editor__KMEditor", dataCy "km-editor_km" ]
        [ div [ class "editor-breadcrumbs" ]
            [ Breadcrumbs.view appState editorContext
            , urlCheckerButton
            , warningsButton
            , a [ class "breadcrumb-button", onClick expandMsg ] [ expandIcon ]
            ]
        , div [ class "editor-body" ]
            [ SplitPane.view splitPaneConfig
                (Tree.view treeViewProps appState editorContext)
                (viewEditor appState wrapMsg eventMsg model integrationPrefabs kmSecrets editorContext)
                model.splitPane
            , rightPanel
            ]
        , deleteModal appState wrapMsg eventMsg editorContext model.deleteModalState
        , moveModal appState wrapMsg eventMsg editorContext model.moveModalState
        , curlImportModal appState wrapMsg model.curlImportModalState
        ]


viewWarningsPanel : AppState -> EditorContext -> Html Msg
viewWarningsPanel appState editorContext =
    let
        viewWarning warning =
            li [] [ linkTo (EditorContext.editorRoute editorContext warning.editorUuid) [] [ text warning.message ] ]

        warnings =
            if List.isEmpty editorContext.warnings then
                Flash.info (gettext "There are no more warnings." appState.locale)

            else
                ul [] (List.map viewWarning editorContext.warnings)
    in
    div [ class "editor-right-panel" ]
        [ warnings ]


type alias EditorConfig msg =
    { appState : AppState
    , wrapMsg : Msg -> msg
    , eventMsg : EventMsg msg
    , model : Model
    , editorContext : EditorContext
    , kmSecrets : List KnowledgeModelSecret
    , integrationPrefabs : List Integration
    }


viewEditor : AppState -> (Msg -> msg) -> EventMsg msg -> Model -> List Integration -> List KnowledgeModelSecret -> EditorContext -> Html msg
viewEditor appState wrapMsg eventMsg model integrationPrefabs kmSecrets editorContext =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        kmUuid =
            Uuid.toString km.uuid

        editorConfig =
            { appState = appState
            , wrapMsg = wrapMsg
            , eventMsg = eventMsg
            , model = model
            , editorContext = editorContext
            , kmSecrets = kmSecrets
            , integrationPrefabs = integrationPrefabs
            }

        kmEditor =
            if editorContext.activeUuid == kmUuid then
                Just <| viewKnowledgeModelEditor editorConfig editorContext.kmEditor.knowledgeModel

            else
                Nothing

        createEditor viewEntityEditor getEntity =
            Maybe.map (viewEntityEditor editorConfig) (getEntity editorContext.activeUuid km)

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
                |> Maybe.map (Tuple.pair editorContext.activeUuid)
                |> Maybe.withDefault emptyEditor
    in
    Html.Keyed.node "div"
        [ class "editor-form-view", id "editor-view", attribute "data-editor-uuid" editorContext.activeUuid ]
        [ editorContent ]



-- KNOWLEDGE MODEL EDITOR -----------------------------------------------------


viewKnowledgeModelEditor : EditorConfig msg -> KnowledgeModel -> Html msg
viewKnowledgeModelEditor { appState, wrapMsg, eventMsg, model, editorContext } km =
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
                |> eventMsg True selector Nothing (Uuid.toString Uuid.nil) (Just kmUuid)

        addChapterEvent =
            AddChapterEvent AddChapterEventData.init
                |> eventMsg False Nothing Nothing kmUuid Nothing

        addMetricEvent =
            AddMetricEvent AddMetricEventData.init
                |> eventMsg False Nothing Nothing kmUuid Nothing

        addPhaseEvent =
            AddPhaseEvent AddPhaseEventData.init
                |> eventMsg False Nothing Nothing kmUuid Nothing

        addTagEvent =
            AddTagEvent AddTagEventData.init
                |> eventMsg False Nothing Nothing kmUuid Nothing

        addIntegrationEvent =
            AddIntegrationEvent AddIntegrationEventData.init
                |> eventMsg False Nothing Nothing kmUuid Nothing

        addResourceCollectionEvent =
            AddResourceCollectionEvent AddResourceCollectionEventData.init
                |> eventMsg False Nothing Nothing kmUuid Nothing

        chaptersInput =
            Input.reorderable
                { name = "chapters"
                , label = gettext "Chapters" appState.locale
                , items =
                    km.chapterUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingChapters editorContext
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setChapterUuids
                , getRoute = EditorContext.editorRoute editorContext
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
                , items =
                    km.metricUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingMetrics editorContext
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setMetricUuids
                , getRoute = EditorContext.editorRoute editorContext
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
                , items =
                    km.phaseUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingPhases editorContext
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setPhaseUuids
                , getRoute = EditorContext.editorRoute editorContext
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
                , items =
                    km.tagUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingTags editorContext
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setTagUuids
                , getRoute = EditorContext.editorRoute editorContext
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
                , items =
                    km.integrationUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingIntegrations editorContext
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setIntegrationUuids
                , getRoute = EditorContext.editorRoute editorContext
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
                , items =
                    km.resourceCollectionUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingResourceCollections editorContext
                , entityUuid = kmUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setResourceCollectionUuids
                , getRoute = EditorContext.editorRoute editorContext
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



-- CHAPTER EDITOR -------------------------------------------------------------


viewChapterEditor : EditorConfig msg -> Chapter -> Html msg
viewChapterEditor { appState, wrapMsg, eventMsg, model, editorContext } chapter =
    let
        parentUuid =
            EditorContext.getParentUuid chapter.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditChapterEventData.init
                |> map value
                |> EditChapterEvent
                |> eventMsg True selector Nothing parentUuid (Just chapter.uuid)

        questionAddEvent =
            AddQuestionEventData.init
                |> AddQuestionEvent
                |> eventMsg False Nothing Nothing chapter.uuid Nothing

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
                , items =
                    chapter.questionUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingQuestions editorContext
                , entityUuid = chapter.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setQuestionUuids
                , getRoute = EditorContext.editorRoute editorContext
                , getName = KnowledgeModel.getQuestionName editorContext.kmEditor.knowledgeModel
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



-- QUESTION EDITOR ------------------------------------------------------------


viewQuestionEditor : EditorConfig msg -> Question -> Html msg
viewQuestionEditor { appState, wrapMsg, eventMsg, model, editorContext } question =
    let
        questionUuid =
            Question.getUuid question

        parentUuid =
            EditorContext.getParentUuid questionUuid editorContext

        createEditEvent setOptions setList setValue setIntegration setMultiChoice setItemSelect setFile value =
            createEditEventWithFocusSelector setOptions setList setValue setIntegration setMultiChoice setItemSelect setFile Nothing value

        createEditEventWithFocusSelector setOptions setList setValue setIntegration setMultiChoice setItemSelect setFile selector value =
            eventMsg True selector Nothing parentUuid (Just questionUuid) <|
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
            eventMsg False Nothing Nothing parentUuid (Just questionUuid) <|
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
                |> eventMsg False Nothing Nothing questionUuid Nothing

        expertAddEvent =
            AddExpertEventData.init
                |> AddExpertEvent
                |> eventMsg False Nothing Nothing questionUuid Nothing

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
            KnowledgeModel.getPhases editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith .uuid editorContext
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
                    if List.isEmpty (EditorContext.filterDeleted editorContext <| Question.getAnswerUuids question) then
                        Html.nothing

                    else
                        FormExtra.blockAfter
                            [ faWarning
                            , text (gettext "Changing a question type will remove all answers." appState.locale)
                            ]

                ListQuestion _ _ ->
                    if List.isEmpty (EditorContext.filterDeleted editorContext <| Question.getItemTemplateQuestionUuids question) then
                        Html.nothing

                    else
                        FormExtra.blockAfter
                            [ faWarning
                            , text (gettext "Changing a question type will remove all item questions." appState.locale)
                            ]

                MultiChoiceQuestion _ _ ->
                    if List.isEmpty (EditorContext.filterDeleted editorContext <| Question.getChoiceUuids question) then
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
                , tags = EditorContext.filterDeletedWith .uuid editorContext <| KnowledgeModel.getTags editorContext.kmEditor.knowledgeModel
                , selected = Question.getTagUuids question
                , onChange = createEditEvent setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids setTagUuids
                }

        referencesInput =
            Input.reorderable
                { name = "references"
                , label = gettext "References" appState.locale
                , items =
                    Question.getReferenceUuids question
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingReferences editorContext
                , entityUuid = questionUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids setReferenceUuids
                , getRoute = EditorContext.editorRoute editorContext
                , getName = KnowledgeModel.getReferenceName editorContext.kmEditor.knowledgeModel
                , untitledLabel = gettext "Untitled reference" appState.locale
                , addChildLabel = gettext "Add reference" appState.locale
                , addChildMsg = addReferenceEvent
                , addChildDataCy = "reference"
                }

        expertsInput =
            Input.reorderable
                { name = "experts"
                , label = gettext "Experts" appState.locale
                , items =
                    Question.getExpertUuids question
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingExperts editorContext
                , entityUuid = questionUuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids setExpertUuids
                , getRoute = EditorContext.editorRoute editorContext
                , getName = KnowledgeModel.getExpertName editorContext.kmEditor.knowledgeModel
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
                                |> eventMsg False Nothing Nothing parentUuid (Just questionUuid)

                        addAnswerEvent =
                            AddAnswerEventData.init
                                |> AddAnswerEvent
                                |> eventMsg False Nothing Nothing questionUuid Nothing

                        answersInput =
                            Input.reorderable
                                { name = "answers"
                                , label = gettext "Answers" appState.locale
                                , items =
                                    Question.getAnswerUuids question
                                        |> EditorContext.filterDeleted editorContext
                                        |> EditorContext.filterExistingAnswers editorContext
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setAnswerUuids
                                , getRoute = EditorContext.editorRoute editorContext
                                , getName = KnowledgeModel.getAnswerName editorContext.kmEditor.knowledgeModel
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
                                |> eventMsg False Nothing Nothing parentUuid (Just questionUuid)

                        addItemTemplateQuestionEvent =
                            AddQuestionEventData.init
                                |> AddQuestionEvent
                                |> eventMsg False Nothing Nothing questionUuid Nothing

                        itemTemplateQuestionsInput =
                            Input.reorderable
                                { name = "questions"
                                , label = gettext "Questions" appState.locale
                                , items =
                                    Question.getItemTemplateQuestionUuids question
                                        |> EditorContext.filterDeleted editorContext
                                        |> EditorContext.filterExistingQuestions editorContext
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setItemTemplateQuestionUuids
                                , getRoute = EditorContext.editorRoute editorContext
                                , getName = KnowledgeModel.getQuestionName editorContext.kmEditor.knowledgeModel
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
                                |> eventMsg False Nothing Nothing parentUuid (Just questionUuid)

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
                                |> eventMsg False Nothing Nothing parentUuid (Just questionUuid)

                        integrationUuidOptions =
                            KnowledgeModel.getIntegrations editorContext.kmEditor.knowledgeModel
                                |> EditorContext.filterDeletedWith Integration.getUuid editorContext
                                |> List.map (\integration -> ( Integration.getUuid integration, String.withDefault (gettext "Untitled integration" appState.locale) (Integration.getVisibleName integration) ))
                                |> (::) ( Uuid.toString Uuid.nil, gettext "- select integration -" appState.locale )

                        selectedIntegrationVariables =
                            Question.getIntegrationUuid question
                                |> Maybe.andThen (flip KnowledgeModel.getIntegration editorContext.kmEditor.knowledgeModel)
                                |> Maybe.unwrap [] Integration.getVariables

                        onVariableInput variable value =
                            let
                                variables =
                                    Question.getVariables question
                                        |> Maybe.unwrap Dict.empty (Dict.insert variable value)
                            in
                            createTypeEditEvent setVariables variables

                        variablesInput =
                            if List.length selectedIntegrationVariables > 0 then
                                let
                                    variableInput variable =
                                        Input.string
                                            { name = "variables-" ++ variable
                                            , label = variable
                                            , value = String.fromMaybe <| Question.getVariablesValue variable question
                                            , onInput = onVariableInput variable
                                            }
                                in
                                div [ class "form-group" ]
                                    [ div [ class "card card-border-light" ]
                                        [ div [ class "card-header" ] [ text (gettext "Integration Configuration" appState.locale) ]
                                        , div [ class "card-body" ]
                                            (List.map variableInput selectedIntegrationVariables)
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
                                        [ linkTo (EditorContext.editorRoute editorContext integrationUuid) [] [ text (gettext "Go to integration" appState.locale) ]
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
                    , variablesInput
                    ]

                MultiChoiceQuestion _ _ ->
                    let
                        createTypeEditEvent map value =
                            EditQuestionMultiChoiceEventData.init
                                |> map value
                                |> (EditQuestionEvent << EditQuestionMultiChoiceEvent)
                                |> eventMsg False Nothing Nothing parentUuid (Just questionUuid)

                        addChoiceEvent =
                            AddChoiceEventData.init
                                |> AddChoiceEvent
                                |> eventMsg False Nothing Nothing questionUuid Nothing

                        choicesInput =
                            Input.reorderable
                                { name = "choices"
                                , label = gettext "Choices" appState.locale
                                , items =
                                    Question.getChoiceUuids question
                                        |> EditorContext.filterDeleted editorContext
                                        |> EditorContext.filterExistingChoices editorContext
                                , entityUuid = questionUuid
                                , getReorderableState = flip Dict.get model.reorderableStates
                                , toMsg = compose2 wrapMsg ReorderableMsg
                                , updateList = createTypeEditEvent setChoiceUuids
                                , getRoute = EditorContext.editorRoute editorContext
                                , getName = KnowledgeModel.getChoiceName editorContext.kmEditor.knowledgeModel
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
                                |> eventMsg False Nothing Nothing parentUuid (Just questionUuid)

                        listQuestionUuidOptgroup ( chapter, questions ) =
                            let
                                filteredQuestions =
                                    questions
                                        |> EditorContext.filterDeletedWith Question.getUuid editorContext
                                        |> List.filter Question.isList
                                        |> List.map (\q -> ( Question.getUuid q, Question.getTitle q ))
                            in
                            if List.isEmpty filteredQuestions then
                                Nothing

                            else
                                Just
                                    ( chapter.title, filteredQuestions )

                        listQuestionUuidOptions =
                            KnowledgeModel.getAllNestedQuestionsByChapter editorContext.kmEditor.knowledgeModel
                                |> List.filter (not << flip EditorContext.isDeleted editorContext << .uuid << Tuple.first)
                                |> List.filterMap listQuestionUuidOptgroup

                        listQuestionUuidInput =
                            Input.selectWithGroups
                                { name = "listQuestionUuid"
                                , label = gettext "List Question" appState.locale
                                , value = String.fromMaybe <| Question.getListQuestionUuid question
                                , defaultOption = ( "", gettext "- select list question -" appState.locale )
                                , options = listQuestionUuidOptions
                                , onChange = createTypeEditEvent setListQuestionUuid << String.toMaybe
                                , extra =
                                    case Question.getListQuestionUuid question of
                                        Just listQuestionUuid ->
                                            if EditorContext.isQuestionDeletedInHierarchy listQuestionUuid editorContext then
                                                Nothing

                                            else
                                                Just <|
                                                    div [ class "mt-1" ]
                                                        [ linkTo (EditorContext.editorRoute editorContext listQuestionUuid) [] [ text (gettext "Go to list question" appState.locale) ]
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
                                |> eventMsg True Nothing Nothing parentUuid (Just questionUuid)

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
                    KnowledgeModel.getAllQuestions editorContext.kmEditor.knowledgeModel
                        |> EditorContext.filterDeletedWith Question.getUuid editorContext
                        |> List.filter ((==) (Just questionUuid) << Question.getListQuestionUuid)
                        |> List.filter (EditorContext.isReachable editorContext << Question.getUuid)
                        |> List.sortBy Question.getTitle
                        |> List.map (viewQuestionLink appState editorContext)
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



-- METRIC EDITOR --------------------------------------------------------------


viewMetricEditor : EditorConfig msg -> Metric -> Html msg
viewMetricEditor { appState, wrapMsg, eventMsg, model, editorContext } metric =
    let
        parentUuid =
            EditorContext.getParentUuid metric.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditMetricEventData.init
                |> map value
                |> EditMetricEvent
                |> eventMsg True selector Nothing parentUuid (Just metric.uuid)

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



-- PHASE EDITOR ---------------------------------------------------------------


viewPhaseEditor : EditorConfig msg -> Phase -> Html msg
viewPhaseEditor { appState, wrapMsg, eventMsg, editorContext } phase =
    let
        parentUuid =
            EditorContext.getParentUuid phase.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditPhaseEventData.init
                |> map value
                |> EditPhaseEvent
                |> eventMsg True selector Nothing parentUuid (Just phase.uuid)

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



-- TAG EDITOR -----------------------------------------------------------------


viewTagEditor : EditorConfig msg -> Tag -> Html msg
viewTagEditor { appState, wrapMsg, eventMsg, editorContext } tag =
    let
        parentUuid =
            EditorContext.getParentUuid tag.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditTagEventData.init
                |> map value
                |> EditTagEvent
                |> eventMsg True selector Nothing parentUuid (Just tag.uuid)

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



-- INTEGRATION EDITOR ---------------------------------------------------------


viewIntegrationEditor : EditorConfig msg -> Integration -> Html msg
viewIntegrationEditor config integration =
    let
        { appState, wrapMsg, eventMsg, integrationPrefabs, editorContext } =
            config

        integrationUuid =
            Integration.getUuid integration

        parentUuid =
            EditorContext.getParentUuid integrationUuid editorContext

        createEditEventWithFocusSelector setApi setApiLegacy setWidget selector value =
            eventMsg True selector Nothing parentUuid (Just integrationUuid) <|
                EditIntegrationEvent <|
                    case integration of
                        ApiIntegration _ ->
                            EditIntegrationApiEventData.init
                                |> setApi value
                                |> EditIntegrationApiEvent

                        ApiLegacyIntegration _ _ ->
                            EditIntegrationApiLegacyEventData.init
                                |> setApiLegacy value
                                |> EditIntegrationApiLegacyEvent

                        WidgetIntegration _ _ ->
                            EditIntegrationWidgetEventData.init
                                |> setWidget value
                                |> EditIntegrationWidgetEvent

        createEditEventFromPrefab integrationPrefab =
            eventMsg False Nothing Nothing parentUuid (Just integrationUuid) <|
                EditIntegrationEvent <|
                    case integrationPrefab of
                        ApiIntegration data ->
                            EditIntegrationApiEvent
                                { allowCustomReply = EventField.create data.allowCustomReply True
                                , annotations = EventField.create data.annotations True
                                , name = EventField.create data.name True
                                , requestAllowEmptySearch = EventField.create data.requestAllowEmptySearch True
                                , requestBody = EventField.create data.requestBody True
                                , requestHeaders = EventField.create data.requestHeaders True
                                , requestMethod = EventField.create data.requestMethod True
                                , requestUrl = EventField.create data.requestUrl True
                                , responseItemTemplate = EventField.create data.responseItemTemplate True
                                , responseItemTemplateForSelection = EventField.create data.responseItemTemplateForSelection True
                                , responseListField = EventField.create data.responseListField True
                                , testQ = EventField.create data.testQ True
                                , testResponse = EventField.create data.testResponse True
                                , testVariables = EventField.create data.testVariables True
                                , variables = EventField.create data.variables True
                                }

                        ApiLegacyIntegration commonData apiData ->
                            EditIntegrationApiLegacyEvent
                                { id = EventField.create commonData.id True
                                , name = EventField.create commonData.name True
                                , variables = EventField.create commonData.variables True
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
                                , variables = EventField.create commonData.variables True
                                , logo = EventField.create commonData.logo True
                                , itemUrl = EventField.create commonData.itemUrl True
                                , annotations = EventField.create commonData.annotations True
                                , widgetUrl = EventField.create widgetData.widgetUrl True
                                }

        onTypeChange value =
            eventMsg False Nothing Nothing parentUuid (Just integrationUuid) <|
                case value of
                    "Api" ->
                        EditIntegrationApiEventData.init
                            |> EditIntegrationApiEvent
                            |> EditIntegrationEvent

                    "Widget" ->
                        EditIntegrationWidgetEventData.init
                            |> EditIntegrationWidgetEvent
                            |> EditIntegrationEvent

                    _ ->
                        EditIntegrationApiLegacyEventData.init
                            |> EditIntegrationApiLegacyEvent
                            |> EditIntegrationEvent

        integrationTypeOptions =
            [ ( "Api", gettext "API" appState.locale )
            , ( "ApiLegacy", gettext "API (Legacy)" appState.locale )
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
                , mbGuideLink = Just WizardGuideLinks.kmEditorIntegrationQuestion
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

        annotationsInput =
            Input.annotations appState
                { annotations = Integration.getAnnotations integration
                , onEdit = createEditEventWithFocusSelector setAnnotations setAnnotations setAnnotations
                }

        integrationTypeInputs =
            case integration of
                ApiIntegration data ->
                    viewIntegrationEditorApi config parentUuid integrationUuid integration data

                ApiLegacyIntegration _ data ->
                    viewIntegrationEditorApiLegacy config parentUuid integrationUuid integration data

                WidgetIntegration _ data ->
                    viewIntegrationEditorWidget config parentUuid integrationUuid integration data

        wrapQuestionsWithIntegration questions =
            if List.isEmpty questions then
                div [] [ i [] [ text (gettext "No questions" appState.locale) ] ]

            else
                ul [] questions

        questionsWithIntegration =
            KnowledgeModel.getAllQuestions editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith Question.getUuid editorContext
                |> List.filter ((==) (Just integrationUuid) << Question.getIntegrationUuid)
                |> List.filter (EditorContext.isReachable editorContext << Question.getUuid)
                |> List.sortBy Question.getTitle
                |> List.map (viewQuestionLink appState editorContext)
                |> wrapQuestionsWithIntegration

        prefabsView =
            if (not << List.isEmpty) integrationPrefabs && EditorContext.isEmptyIntegrationEditorUuid integrationUuid editorContext then
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
         ]
            ++ integrationTypeInputs
            ++ [ annotationsInput
               , FormGroup.plainGroup questionsWithIntegration (gettext "Questions using this integration" appState.locale)
               ]
        )


viewIntegrationEditorApi : EditorConfig msg -> String -> String -> Integration -> ApiIntegrationData -> List (Html msg)
viewIntegrationEditorApi config parentUuid integrationUuid integration data =
    let
        { appState, eventMsg, kmSecrets, model, wrapMsg } =
            config

        createTypeEditEvent map value =
            createTypeEditEventWithFocusSelector map Nothing value

        createTypeEditEventWithFocusSelector map selector value =
            createTypeEditEventWithFocusSelectorAndCursorPos map selector Nothing value

        createTypeEditEventWithFocusSelectorAndCursorPos map selector mbCursorPos value =
            EditIntegrationApiEventData.init
                |> map value
                |> (EditIntegrationEvent << EditIntegrationApiEvent)
                |> eventMsg True selector mbCursorPos parentUuid (Just integrationUuid)

        allowCustomReplyGroup =
            [ Input.checkbox
                { name = "allowCustomReply"
                , label = gettext "Allow Custom Reply" appState.locale
                , value = data.allowCustomReply
                , onInput = createTypeEditEvent setAllowCustomReply
                }
            , FormExtra.mdAfter (gettext "If enabled, users can type their own custom reply. If disabled, they must select a reply from the integration." appState.locale)
            ]

        variablesGroup =
            [ Input.variables appState
                { label = gettext "Variables" appState.locale
                , values = Integration.getVariables integration
                , onChange = createTypeEditEventWithFocusSelector setVariables
                , copyableInput = copyableJinjaVariable config "variables"
                }
            , FormExtra.mdAfter (gettext "Variables can be used to parametrize the integration for each question. Use this to define the variables whose value can be filled on the questions using this integration. The variables can then be used in the request configuration. For example, if you define a variable named *type*, you can use it as `{{ variables.type }}`, such as *ht&#8203;tps://example.com/{{ variables.type }}*." appState.locale)
            ]

        secretsGroup =
            let
                viewSecret secret =
                    div [ class "d-flex align-items-center variables-input mb-2" ]
                        [ div [ class "form-control bg-light" ] [ text secret.name ]
                        , copyableJinjaVariable config "secrets" secret.name
                        ]

                secrets =
                    List.map viewSecret kmSecrets

                secretsView =
                    if List.isEmpty secrets then
                        div [ class "mb-2" ] [ i [] [ text (gettext "There are no secrets configured." appState.locale) ] ]

                    else
                        div [] secrets
            in
            [ div [ class "form-group" ]
                [ label [] [ text (gettext "Secrets" appState.locale) ]
                , secretsView
                , linkTo Routes.knowledgeModelSecrets
                    [ target "_blank" ]
                    [ text (gettext "Configure secrets" appState.locale)
                    , fas "fa-external-link-alt ms-2"
                    ]
                , Markdown.toHtml [ class "text-muted mt-1" ] (gettext "Secrets can store sensitive information used for API calls, such as tokens, that you don't want to share with the knowledge models. You can use them as `{{ secrets.secret_name }}` anywhere in the request configuration." appState.locale)
                ]
            ]

        advancedIntegrationConfiguration =
            Input.foldableGroup
                { identifier = "advancedConfiguration"
                , openLabel = gettext "Advanced Integration Configuration" appState.locale
                , content = allowCustomReplyGroup ++ variablesGroup ++ secretsGroup
                , markdownPreviews = model.markdownPreviews
                , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                , entityUuid = integrationUuid
                }

        requestInputGroup =
            let
                requestAdvancedConfigurationContent =
                    [ Input.headers appState
                        { label = gettext "Request HTTP Headers" appState.locale
                        , headers = data.requestHeaders
                        , onEdit = createTypeEditEventWithFocusSelector setRequestHeaders
                        }
                    , FormExtra.mdAfter (gettext "HTTP headers to include in the API request. You can use `{{ q }}` for the search term and `{{ variables.name }}` for referencing variables and `{{ secrets.name }}` for secrets." appState.locale)
                    , Input.textarea
                        { name = "requestBody"
                        , label = gettext "Request HTTP Body" appState.locale
                        , value = Maybe.withDefault "" data.requestBody
                        , onInput = createTypeEditEvent (setRequestBody << String.toMaybe)
                        }
                    , FormExtra.mdAfter (gettext "Optional request body of the API request. You can again use `{{ q }}` for the search term and `{{ variables.name }}` for referencing variables and `{{ secrets.name }}` for secrets." appState.locale)
                    , Input.checkbox
                        { name = "requestEmptySearch"
                        , label = gettext "Allow Empty Search" appState.locale
                        , value = data.requestAllowEmptySearch
                        , onInput = createTypeEditEvent setRequestAllowEmptySearch
                        }
                    , FormExtra.mdAfter (gettext "If enabled, the API request will be sent even if the user does not enter a search term. This is useful for APIs that return a default set of results when no search term is provided." appState.locale)
                    ]

                requestAdvancedConfiguration =
                    Input.foldableGroup
                        { identifier = "requestAdvancedConfiguration"
                        , openLabel = gettext "Advanced Request Configuration" appState.locale
                        , content = requestAdvancedConfigurationContent
                        , markdownPreviews = model.markdownPreviews
                        , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                        , entityUuid = integrationUuid
                        }
            in
            div [ class "card card-border-light mb-5" ]
                [ div [ class "card-header d-flex justify-content-between" ]
                    [ text (gettext "Request" appState.locale)
                    , a (onClick (wrapMsg (CurlImportModalSetIntegration (Just data.uuid))) :: tooltipLeft (gettext "Import from cURL string" appState.locale))
                        [ fas "fa-download me-1"
                        , text (gettext "Import" appState.locale)
                        ]
                    ]
                , div [ class "card-body" ]
                    [ div [ class "form-group" ]
                        [ div [ class "input-group input-group-http-request" ]
                            [ Input.selectRaw
                                { name = "requestMethod"
                                , value = data.requestMethod
                                , options = [ ( "GET", "GET" ), ( "POST", "POST" ) ]
                                , onChange = createTypeEditEvent setRequestMethod
                                }
                            , Input.stringRaw
                                { name = "requestUrl"
                                , value = data.requestUrl
                                , onInput = createTypeEditEvent setRequestUrl
                                , placeholder = Just (gettext "Enter request URL..." appState.locale)
                                }
                            ]
                        ]
                    , FormExtra.mdAfter (gettext "The full API endpoint used for search. Use `{{ q }}` to insert the user's search term (for example, *https://example.com/search?q={{ q }}*), and `{{ variables.name }}` for referencing variables and `{{ secrets.name }}` for secrets." appState.locale)
                    , requestAdvancedConfiguration
                    ]
                ]

        testInputGroup =
            let
                createTestVariables key value =
                    setTestVariables (Dict.insert key value data.testVariables)

                viewTestVariableInput key =
                    div [ class "input-group input-group-http-request mb-2" ]
                        [ span [ class "input-group-text" ] [ text key ]
                        , Input.stringRaw
                            { name = "testVariable-" ++ key
                            , placeholder = Nothing
                            , value = Maybe.withDefault "" (ApiIntegrationData.getTestVariableValue key data)
                            , onInput = createTypeEditEvent (createTestVariables key)
                            }
                        ]

                testVariablesInputs =
                    Html.viewIf (not (List.isEmpty data.variables)) <|
                        div [ class "form-group" ]
                            (label [] [ text (gettext "Test Variables" appState.locale) ]
                                :: List.map viewTestVariableInput (List.filter (not << String.isEmpty) data.variables)
                            )

                testIntegrationRequestMsg =
                    wrapMsg <| TestIntegrationRequest integrationUuid data.testQ data.testVariables

                ( hasErrorResult, testResultError ) =
                    case Dict.get integrationUuid model.integrationTestResults of
                        Just (ActionResult.Error err) ->
                            ( True, Flash.error err )

                        _ ->
                            ( False, Html.nothing )

                ( hasSuccessResult, resultView ) =
                    case data.testResponse of
                        Just response ->
                            let
                                requestDetailsGroupContent =
                                    let
                                        requestUrl =
                                            response.request.method ++ " " ++ response.request.url

                                        headers =
                                            response.request.headers
                                                |> List.map (\{ key, value } -> key ++ ": " ++ value)
                                                |> String.join "\n"
                                                |> String.toMaybe

                                        content =
                                            [ Just requestUrl, headers, response.request.body ]
                                                |> List.filterMap identity
                                                |> String.join "\n\n--\n\n"
                                    in
                                    [ pre [ class "bg-light px-4 py-2 m-0" ] [ text content ] ]

                                requestDetails =
                                    Input.foldableGroup
                                        { identifier = "requestDetails"
                                        , openLabel = gettext "Request Details" appState.locale
                                        , content = requestDetailsGroupContent
                                        , markdownPreviews = model.markdownPreviews
                                        , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                                        , entityUuid = integrationUuid
                                        }

                                responseDetails =
                                    let
                                        showResponseData responseData =
                                            let
                                                statusBadgeClass =
                                                    if HttpStatus.isSuccessful responseData.status then
                                                        "bg-success"

                                                    else
                                                        "bg-danger"

                                                statusBadge =
                                                    span [ class ("badge me-2 " ++ statusBadgeClass) ]
                                                        [ text (String.fromInt responseData.status ++ " " ++ HttpStatus.statusName responseData.status)
                                                        ]

                                                ( contentTypeBadgeClass, contentTypeTooltip ) =
                                                    if TypeHintTestResponse.supportedContentType responseData then
                                                        ( "bg-light", [] )

                                                    else
                                                        ( "bg-warning", tooltip (gettext "This content type is not supported." appState.locale) )

                                                contentTypeBadge =
                                                    span (class ("badge text-dark " ++ contentTypeBadgeClass) :: contentTypeTooltip)
                                                        [ text ("Content Type: " ++ responseData.contentType) ]

                                                defaultContent =
                                                    pre [] [ code [] [ text responseData.body ] ]

                                                responseBody =
                                                    case Json.Print.prettyString { indent = 4, columns = 100 } responseData.body of
                                                        Ok jsonResult ->
                                                            div []
                                                                [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                                                                , SyntaxHighlight.json jsonResult
                                                                    |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                                                                    |> Result.withDefault defaultContent
                                                                ]

                                                        Err _ ->
                                                            div []
                                                                [ SyntaxHighlight.useTheme SyntaxHighlight.gitHub
                                                                , SyntaxHighlight.noLang responseData.body
                                                                    |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                                                                    |> Result.withDefault defaultContent
                                                                ]
                                            in
                                            div []
                                                [ strong [ class "me-2" ] [ text (gettext "Response" appState.locale) ]
                                                , statusBadge
                                                , contentTypeBadge
                                                , div [ class "mt-2 response-code" ] [ responseBody ]
                                                ]
                                    in
                                    case response.response of
                                        TypeHintTestResponse.SuccessTypeHintResponse responseData ->
                                            showResponseData responseData

                                        TypeHintTestResponse.RemoteErrorTypeHintResponse responseData ->
                                            showResponseData responseData

                                        TypeHintTestResponse.RequestFailedTypeHintResponse errorData ->
                                            Flash.error errorData.message
                            in
                            ( True, div [] [ requestDetails, responseDetails ] )

                        _ ->
                            ( False, Html.nothing )

                anyResult =
                    hasSuccessResult || hasErrorResult
            in
            div [ class "card card-border-light mb-5" ]
                [ div [ class "card-header" ] [ text (gettext "Test" appState.locale) ]
                , div [ class "card-body" ]
                    [ Input.string
                        { name = "testQ"
                        , label = gettext "Test Search Query" appState.locale
                        , value = data.testQ
                        , onInput = createTypeEditEvent setTestQ
                        }
                    , FormExtra.mdAfter (gettext "This is what users would write in the questionnaire when filling in the integration question." appState.locale)
                    , testVariablesInputs
                    , ActionButton.button
                        { label = gettext "Load" appState.locale
                        , result = getIntegrationTestResult integrationUuid model
                        , msg = testIntegrationRequestMsg
                        , dangerous = False
                        }
                    , Html.viewIf anyResult <| hr [ class "mb-4" ] []
                    , testResultError
                    , resultView
                    ]
                ]

        responseGroup =
            case Maybe.map .response data.testResponse of
                Just (TypeHintTestResponse.SuccessTypeHintResponse responseData) ->
                    case responseData.bodyJson of
                        Just jsonData ->
                            let
                                responseListFieldVisible =
                                    not (String.isEmpty (String.fromMaybe data.responseListField))
                                        || (case jsonData of
                                                JsonValue.ArrayValue _ ->
                                                    False

                                                _ ->
                                                    True
                                           )

                                responseListField =
                                    if responseListFieldVisible then
                                        let
                                            viewListFieldSuggestion suggestion =
                                                a
                                                    [ class "btn btn-outline-primary btn-sm py-0"
                                                    , onClick (createTypeEditEventWithFocusSelector setResponseListField (Just "#responseListField") (String.toMaybe suggestion))
                                                    ]
                                                    [ text suggestion ]

                                            fieldSuggestionButtons =
                                                TypeHintTestResponse.getSuggestedListFieldProperties responseData
                                                    |> List.map viewListFieldSuggestion

                                            fieldSuggestions =
                                                Html.viewIf (not (List.isEmpty fieldSuggestionButtons)) <|
                                                    div [ class "mb-1" ] fieldSuggestionButtons
                                        in
                                        [ Input.string
                                            { name = "responseListField"
                                            , label = gettext "Response List Field" appState.locale
                                            , value = Maybe.withDefault "" data.responseListField
                                            , onInput = createTypeEditEvent (setResponseListField << String.toMaybe)
                                            }
                                        , FormExtra.blockAfter
                                            [ fieldSuggestions
                                            , Markdown.toHtml [ class "mt-1" ] (gettext "If the returned JSON is not an array of items directly but the items are nested, use this to define the name or path to the field in the response that contains the list of items. Keep empty otherwise." appState.locale)
                                            ]
                                        ]

                                    else
                                        []

                                responseItemTemplate =
                                    [ Input.itemTemplateEditor appState
                                        { name = "responseItemTemplate"
                                        , label = gettext "Response Item Template" appState.locale
                                        , value = data.responseItemTemplate
                                        , onInput = createTypeEditEventWithFocusSelectorAndCursorPos setResponseItemTemplate
                                        , onBlurWithSelection = compose3 wrapMsg SetCursorPosition
                                        , showPreviewMsg = wrapMsg << TestIntegrationPreview integrationUuid
                                        , showTemplateMsg = wrapMsg << ShowHideMarkdownPreview False
                                        , entityUuid = integrationUuid
                                        , markdownPreviews = model.markdownPreviews
                                        , integrationTestPreviews = model.integrationTestPreviews
                                        , cursorPositions = model.cursorPositions
                                        , fieldSuggestions = TypeHintTestResponse.getSuggestedItemProperties (String.fromMaybe data.responseListField) responseData
                                        , toPreview = .value
                                        }
                                    ]

                                responseAdvancedConfigurationContent =
                                    [ Input.itemTemplateEditor appState
                                        { name = "responseItemTemplateForSelection"
                                        , label = gettext "Response Item Template for Selection" appState.locale
                                        , value = Maybe.withDefault "" data.responseItemTemplateForSelection
                                        , onInput = \selector mbCursorPos value -> createTypeEditEventWithFocusSelectorAndCursorPos setResponseItemTemplateForSelection selector mbCursorPos (String.toMaybe value)
                                        , onBlurWithSelection = compose3 wrapMsg SetCursorPosition
                                        , showPreviewMsg = wrapMsg << TestIntegrationPreview integrationUuid
                                        , showTemplateMsg = wrapMsg << ShowHideMarkdownPreview False
                                        , entityUuid = integrationUuid
                                        , markdownPreviews = model.markdownPreviews
                                        , integrationTestPreviews = model.integrationTestPreviews
                                        , cursorPositions = model.cursorPositions
                                        , fieldSuggestions = TypeHintTestResponse.getSuggestedItemProperties (String.fromMaybe data.responseListField) responseData
                                        , toPreview = String.fromMaybe << .valueForSelection
                                        }
                                    ]

                                responseAdvancedConfiguration =
                                    [ Input.foldableGroup
                                        { identifier = "responseAdvancedConfiguration"
                                        , openLabel = gettext "Advanced Response Configuration" appState.locale
                                        , content = responseAdvancedConfigurationContent
                                        , markdownPreviews = model.markdownPreviews
                                        , previewMsg = compose2 wrapMsg ShowHideMarkdownPreview
                                        , entityUuid = integrationUuid
                                        }
                                    ]
                            in
                            div [ class "card card-border-light mb-5" ]
                                [ div [ class "card-header" ] [ text (gettext "Response" appState.locale) ]
                                , div [ class "card-body" ]
                                    (responseListField ++ responseItemTemplate ++ responseAdvancedConfiguration)
                                ]

                        Nothing ->
                            div [ class "mb-5" ]
                                [ Flash.warning (gettext "Response is not a valid JSON." appState.locale) ]

                _ ->
                    div [ class "mb-5" ]
                        [ Flash.info (gettext "Run a successful test to configure the response." appState.locale)
                        ]
    in
    integrationNameInput appState integration (createTypeEditEvent setName)
        ++ [ advancedIntegrationConfiguration
           , requestInputGroup
           , testInputGroup
           , responseGroup
           ]


copyableJinjaVariable : EditorConfig msg -> String -> String -> Html msg
copyableJinjaVariable { appState, model, wrapMsg } domain variable =
    let
        isDisabled =
            String.isEmpty variable

        jinjaValue =
            Input.toJinja domain variable

        tooltipText =
            if model.lastCopiedString == Just jinjaValue then
                gettext "Copied!" appState.locale

            else
                gettext "Click to copy code" appState.locale
    in
    span
        (class "btn btn-link"
            :: classList [ ( "disabled", isDisabled ) ]
            :: disabled isDisabled
            :: onClick (wrapMsg (CopyString jinjaValue))
            :: onMouseLeave (wrapMsg ClearLastCopiedString)
            :: tooltip tooltipText
        )
        [ faCopy ]


viewIntegrationEditorApiLegacy : EditorConfig msg -> String -> String -> Integration -> ApiLegacyIntegrationData -> List (Html msg)
viewIntegrationEditorApiLegacy { appState, eventMsg } parentUuid integrationUuid integration data =
    let
        createTypeEditEvent map value =
            createTypeEditEventWithFocusSelector map Nothing value

        createTypeEditEventWithFocusSelector map selector value =
            EditIntegrationApiLegacyEventData.init
                |> map value
                |> (EditIntegrationEvent << EditIntegrationApiLegacyEvent)
                |> eventMsg True selector Nothing parentUuid (Just integrationUuid)

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
                , options = HttpMethod.options
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
    integrationIdInput appState integration (createTypeEditEvent setId)
        ++ integrationNameInput appState integration (createTypeEditEvent setName)
        ++ integrationLogoUrlInput appState integration (createTypeEditEvent (setLogo << String.toMaybe))
        ++ integrationVariablesLegacyInput appState integration (createTypeEditEventWithFocusSelector setVariables)
        ++ [ div [ class "card card-border-light mb-5" ]
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
                            [ WizardGuideLinks.markdownCheatsheet appState.guideLinks ]
                        )
                    ]
                ]
           , integrationItemUrlInput appState integration (createTypeEditEvent setItemUrl << String.toMaybe)
           , FormExtra.mdAfter (gettext "Defines the URL to the selected item. Use `${id}` to get the value defined in **Response Item ID** field, for example `https://example.com/${id}`." appState.locale)
           ]


viewIntegrationEditorWidget : EditorConfig msg -> String -> String -> Integration -> WidgetIntegrationData -> List (Html msg)
viewIntegrationEditorWidget { appState, eventMsg } parentUuid integrationUuid integration data =
    let
        createTypeEditEvent map value =
            EditIntegrationWidgetEventData.init
                |> map value
                |> (EditIntegrationEvent << EditIntegrationWidgetEvent)
                |> eventMsg True Nothing Nothing parentUuid (Just integrationUuid)

        createTypeEditEventWithFocusSelector map selector value =
            EditIntegrationWidgetEventData.init
                |> map value
                |> (EditIntegrationEvent << EditIntegrationWidgetEvent)
                |> eventMsg True selector Nothing parentUuid (Just integrationUuid)

        widgetUrlInput =
            Input.string
                { name = "widgetUrl"
                , label = gettext "Widget URL" appState.locale
                , value = data.widgetUrl
                , onInput = createTypeEditEvent setWidgetUrl
                }
    in
    integrationIdInput appState integration (createTypeEditEvent setId)
        ++ integrationNameInput appState integration (createTypeEditEvent setName)
        ++ integrationLogoUrlInput appState integration (createTypeEditEvent (setLogo << String.toMaybe))
        ++ integrationVariablesLegacyInput appState integration (createTypeEditEventWithFocusSelector setVariables)
        ++ [ widgetUrlInput
           , FormExtra.mdAfter (gettext "The URL of the widget implemented using [DSW Integration SDK](https://github.com/ds-wizard/dsw-integration-sdk)." appState.locale)
           , integrationItemUrlInput appState integration (createTypeEditEvent setItemUrl << String.toMaybe)
           , FormExtra.mdAfter (gettext "Defines the URL to the selected item. Use `${id}` value returned from the widget, for example `https://example.com/${id}`." appState.locale)
           ]


integrationIdInput : AppState -> Integration -> (String -> msg) -> List (Html msg)
integrationIdInput appState integration onInput =
    [ Input.string
        { name = "id"
        , label = gettext "ID" appState.locale
        , value = Integration.getId integration
        , onInput = onInput
        }
    , FormExtra.mdAfter (gettext "A string that identifies the integration. It has to be unique for each integration." appState.locale)
    ]


integrationNameInput : AppState -> Integration -> (String -> msg) -> List (Html msg)
integrationNameInput appState integration onInput =
    [ Input.string
        { name = "name"
        , label = gettext "Name" appState.locale
        , value = Integration.getName integration
        , onInput = onInput
        }
    , FormExtra.mdAfter (gettext "A name visible everywhere else in the knowledge model Editor, such as when choosing the integration for a question." appState.locale)
    ]


integrationLogoUrlInput : AppState -> Integration -> (String -> msg) -> List (Html msg)
integrationLogoUrlInput appState integration onInput =
    [ Input.string
        { name = "logo"
        , label = gettext "Logo URL" appState.locale
        , value = String.fromMaybe (Integration.getLogo integration)
        , onInput = onInput
        }
    , FormExtra.mdAfter (gettext "Logo is displayed next to the link to the selected item in questionnaires. It can be either URL or base64 image." appState.locale)
    ]


integrationVariablesLegacyInput : AppState -> Integration -> (Maybe String -> List String -> msg) -> List (Html msg)
integrationVariablesLegacyInput appState integration onInput =
    [ Input.variables appState
        { label = gettext "Variables" appState.locale
        , values = Integration.getVariables integration
        , onChange = onInput
        , copyableInput = always Html.nothing
        }
    , FormExtra.mdAfter (gettext "Variables can be used to parametrize the integration for each question. Use this to define the variables whose value can be filled on the questions using this integration. The variables can then be used in the URL configuration. For example, if you define a variable named *type*, you can use it as `${type}`, such as *ht&#8203;tps://example.com/${type}*." appState.locale)
    ]


integrationItemUrlInput : AppState -> Integration -> (String -> msg) -> Html msg
integrationItemUrlInput appState integration onInput =
    Input.string
        { name = "itemUrl"
        , label = gettext "Item URL" appState.locale
        , value = String.fromMaybe (Integration.getItemUrl integration)
        , onInput = onInput
        }



-- ANSWER EDITOR --------------------------------------------------------------


viewAnswerEditor : EditorConfig msg -> Answer -> Html msg
viewAnswerEditor { appState, wrapMsg, eventMsg, model, editorContext } answer =
    let
        parentUuid =
            EditorContext.getParentUuid answer.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditAnswerEventData.init
                |> map value
                |> EditAnswerEvent
                |> eventMsg True selector Nothing parentUuid (Just answer.uuid)

        questionAddEvent =
            AddQuestionEventData.init
                |> AddQuestionEvent
                |> eventMsg False Nothing Nothing answer.uuid Nothing

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
                , items =
                    answer.followUpUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingQuestions editorContext
                , entityUuid = answer.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setFollowUpUuids
                , getRoute = EditorContext.editorRoute editorContext
                , getName = KnowledgeModel.getQuestionName editorContext.kmEditor.knowledgeModel
                , untitledLabel = gettext "Untitled question" appState.locale
                , addChildLabel = gettext "Add question" appState.locale
                , addChildMsg = questionAddEvent
                , addChildDataCy = "question"
                }

        metrics =
            EditorContext.filterDeletedWith .uuid editorContext <|
                KnowledgeModel.getMetrics editorContext.kmEditor.knowledgeModel

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



-- CHOICE EDITOR --------------------------------------------------------------


viewChoiceEditor : EditorConfig msg -> Choice -> Html msg
viewChoiceEditor { appState, wrapMsg, eventMsg, editorContext } choice =
    let
        parentUuid =
            EditorContext.getParentUuid choice.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditChoiceEventData.init
                |> map value
                |> EditChoiceEvent
                |> eventMsg True selector Nothing parentUuid (Just choice.uuid)

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
viewReferenceEditor { appState, model, wrapMsg, eventMsg, editorContext } reference =
    let
        referenceUuid =
            Reference.getUuid reference

        parentUuid =
            EditorContext.getParentUuid (Reference.getUuid reference) editorContext

        onTypeChange value =
            eventMsg False Nothing Nothing parentUuid (Just referenceUuid) <|
                case value of
                    "ResourcePage" ->
                        EditReferenceResourcePageEventData.init
                            |> (EditReferenceEvent << EditReferenceResourcePageEvent)

                    "URL" ->
                        EditReferenceURLEventData.init
                            |> (EditReferenceEvent << EditReferenceURLEvent)

                    _ ->
                        EditReferenceCrossEventData.init
                            |> (EditReferenceEvent << EditReferenceCrossEvent)

        referenceTypeOptions =
            [ ( "ResourcePage", gettext "Resource Page" appState.locale )
            , ( "URL", gettext "URL" appState.locale )
            , ( "Cross", gettext "Cross Reference" appState.locale )
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
                                |> eventMsg True selector Nothing parentUuid (Just referenceUuid)

                        resourcePageOption resourcePageUuid =
                            KnowledgeModel.getResourcePage resourcePageUuid editorContext.kmEditor.knowledgeModel
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
                            KnowledgeModel.getResourceCollections (EditorContext.getFilteredKM editorContext)
                                |> List.map (\rc -> ( rc.title, List.filterMap resourcePageOption rc.resourcePageUuids ))

                        resourcePageUuidSelect =
                            Input.selectWithGroups
                                { name = "resourcePageUuid"
                                , label = gettext "Resource Page" appState.locale
                                , value = Maybe.withDefault "" data.resourcePageUuid
                                , defaultOption = ( "", gettext "- select resource page -" appState.locale )
                                , options = resourcePageUuidOptions
                                , onChange = createTypeEditEvent setResourcePageUuid << String.toMaybe
                                , extra = Nothing
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
                                |> eventMsg True selector Nothing parentUuid (Just referenceUuid)

                        urlInput =
                            Input.string
                                { name = "url"
                                , label = gettext "URL" appState.locale
                                , value = data.url
                                , onInput = createTypeEditEvent setUrl
                                }

                        urlError =
                            UrlChecker.getResultByUrl data.url model.urlChecker
                                |> Maybe.andThen (UrlResult.toReadableErrorString appState.locale)
                                |> Maybe.unwrap Html.nothing (FormExtra.blockAfter << List.singleton << Flash.error)

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
                    , urlError
                    , labelInput
                    , annotationsInput
                    ]

                CrossReference data ->
                    let
                        createTypeEditEvent map value =
                            EditReferenceCrossEventData.init
                                |> map value
                                |> (EditReferenceEvent << EditReferenceCrossEvent)
                                |> eventMsg False Nothing Nothing parentUuid (Just referenceUuid)

                        targetQuestionUuidOptgroup ( chapter, questions ) =
                            let
                                filteredQuestions =
                                    questions
                                        |> EditorContext.filterDeletedWith Question.getUuid editorContext
                                        |> List.filter (\q -> Question.getUuid q /= parentUuid)
                                        |> List.map (\q -> ( Question.getUuid q, Question.getTitle q ))
                            in
                            if List.isEmpty filteredQuestions then
                                Nothing

                            else
                                Just
                                    ( chapter.title, filteredQuestions )

                        targetQuestionUuidOptions =
                            KnowledgeModel.getAllNestedQuestionsByChapter editorContext.kmEditor.knowledgeModel
                                |> List.filter (not << flip EditorContext.isDeleted editorContext << .uuid << Tuple.first)
                                |> List.filterMap targetQuestionUuidOptgroup

                        targetQuestionUuidInput =
                            Input.selectWithGroups
                                { name = "targetQuestionUuid"
                                , label = gettext "Question" appState.locale
                                , value = String.fromMaybe <| Reference.getTargetUuid reference
                                , defaultOption = ( "", gettext "- select related question -" appState.locale )
                                , options = targetQuestionUuidOptions
                                , onChange = createTypeEditEvent setTargetUuid
                                , extra =
                                    case Reference.getTargetUuid reference of
                                        Just listQuestionUuid ->
                                            if EditorContext.isQuestionDeletedInHierarchy listQuestionUuid editorContext then
                                                Nothing

                                            else
                                                Just <|
                                                    div [ class "mt-1" ]
                                                        [ linkTo (EditorContext.editorRoute editorContext listQuestionUuid)
                                                            []
                                                            [ text (gettext "Go to related question" appState.locale) ]
                                                        ]

                                        Nothing ->
                                            Nothing
                                }

                        descriptionInput =
                            Input.string
                                { name = "description"
                                , label = gettext "Description" appState.locale
                                , value = data.description
                                , onInput = createTypeEditEvent setDescription
                                }
                    in
                    [ targetQuestionUuidInput
                    , descriptionInput
                    ]
    in
    editor ("reference-" ++ Reference.getUuid reference)
        ([ referenceEditorTitle
         , typeInput
         ]
            ++ referenceTypeInputs
        )



-- EXPERT EDITOR --------------------------------------------------------------


viewExpertEditor : EditorConfig msg -> Expert -> Html msg
viewExpertEditor { appState, wrapMsg, eventMsg, editorContext } expert =
    let
        parentUuid =
            EditorContext.getParentUuid expert.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditExpertEventData.init
                |> map value
                |> EditExpertEvent
                |> eventMsg True selector Nothing parentUuid (Just expert.uuid)

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



-- RESOURCE COLLECTION EDITOR -------------------------------------------------


viewResourceCollectionEditor : EditorConfig msg -> ResourceCollection -> Html msg
viewResourceCollectionEditor { appState, wrapMsg, eventMsg, model, editorContext } resourceCollection =
    let
        parentUuid =
            EditorContext.getParentUuid resourceCollection.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditResourceCollectionEventData.init
                |> map value
                |> EditResourceCollectionEvent
                |> eventMsg True selector Nothing parentUuid (Just resourceCollection.uuid)

        resourcePageAddEvent =
            AddResourcePageEventData.init
                |> AddResourcePageEvent
                |> eventMsg False Nothing Nothing resourceCollection.uuid Nothing

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
                , items =
                    resourceCollection.resourcePageUuids
                        |> EditorContext.filterDeleted editorContext
                        |> EditorContext.filterExistingResourcePages editorContext
                , entityUuid = resourceCollection.uuid
                , getReorderableState = flip Dict.get model.reorderableStates
                , toMsg = compose2 wrapMsg ReorderableMsg
                , updateList = createEditEvent setResourcePageUuids
                , getRoute = EditorContext.editorRoute editorContext
                , getName = KnowledgeModel.getResourcePageName editorContext.kmEditor.knowledgeModel
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



-- RESOURCE PAGE EDITOR -------------------------------------------------------


viewResourcePageEditor : EditorConfig msg -> ResourcePage -> Html msg
viewResourcePageEditor { appState, wrapMsg, eventMsg, model, editorContext } resourcePage =
    let
        parentUuid =
            EditorContext.getParentUuid resourcePage.uuid editorContext

        createEditEvent map value =
            createEditEventWithFocusSelector map Nothing value

        createEditEventWithFocusSelector map selector value =
            EditResourcePageEventData.init
                |> map value
                |> EditResourcePageEvent
                |> eventMsg True selector Nothing parentUuid (Just resourcePage.uuid)

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
            KnowledgeModel.getQuestionReferences questionUuid editorContext.kmEditor.knowledgeModel
                |> List.filterMap Reference.getResourcePageUuid
                |> List.member resourcePage.uuid

        questionsWithResourcePage =
            KnowledgeModel.getAllQuestions editorContext.kmEditor.knowledgeModel
                |> EditorContext.filterDeletedWith Question.getUuid editorContext
                |> List.filter (filterQuestionByResourcePageUuid << Question.getUuid)
                |> List.filter (EditorContext.isReachable editorContext << Question.getUuid)
                |> List.sortBy Question.getTitle
                |> List.map (viewQuestionLink appState editorContext)
                |> wrapQuestionsWithIntegration
    in
    editor ("resource-page-" ++ resourcePage.uuid)
        [ resourcePageEditorTitle
        , titleInput
        , contentInput
        , annotationsInput
        , FormGroup.plainGroup questionsWithResourcePage (gettext "Questions using this resource page" appState.locale)
        ]



-- EDITOR HELPERS -------------------------------------------------------------


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
                    GuideLink.guideLink (AppState.toGuideLinkConfig appState getGuideLink)

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


viewQuestionLink : AppState -> EditorContext -> Question -> Html msg
viewQuestionLink appState editorContext question =
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
        [ linkTo (EditorContext.editorRoute editorContext (Question.getUuid question))
            []
            [ questionTitleNode ]
        ]



-- DELETE MODAL


deleteModal : AppState -> (Msg -> msg) -> EventMsg msg -> EditorContext -> DeleteModalState -> Html msg
deleteModal appState wrapMsg eventMsg editorContext deleteModalState =
    let
        createEvent event uuid =
            eventMsg False Nothing Nothing (EditorContext.getParentUuid uuid editorContext) (Just uuid) event

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


moveModal : AppState -> (Msg -> msg) -> EventMsg msg -> EditorContext -> Maybe MoveModalState -> Html msg
moveModal appState wrapMsg eventMsg editorContext mbMoveModalState =
    let
        ( content, mbConfirmMsg ) =
            case mbMoveModalState of
                Just moveModalState ->
                    let
                        parentUuid =
                            EditorContext.getParentUuid moveModalState.movingUuid editorContext

                        selectedUuid =
                            moveModalState.treeInputModel.selected

                        createEvent event =
                            eventMsg False Nothing Nothing parentUuid (Just moveModalState.movingUuid) (event { targetUuid = selectedUuid })

                        viewProps =
                            { editorContext = editorContext
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



-- cURL Import Modal


curlImportModal : AppState -> (Msg -> msg) -> CurlImportModalState -> Html msg
curlImportModal appState wrapMsg curlImportModalState =
    let
        content =
            [ textarea
                [ class "form-control"
                , placeholder (gettext "Paste your cURL command here..." appState.locale)
                , value curlImportModalState.curlString
                , onInput (wrapMsg << CurlImportModalUpdateString)
                ]
                []
            ]

        curlImportModalConfig =
            Modal.confirmConfig (gettext "Import from cURL" appState.locale)
                |> Modal.confirmConfigVisible (Maybe.isJust curlImportModalState.integrationUuid)
                |> Modal.confirmConfigAction (gettext "Import" appState.locale) (wrapMsg CurlImportModalConfirm)
                |> Modal.confirmConfigCancelMsg (wrapMsg (CurlImportModalSetIntegration Nothing))
                |> Modal.confirmConfigContent content
    in
    Modal.confirm appState curlImportModalConfig
