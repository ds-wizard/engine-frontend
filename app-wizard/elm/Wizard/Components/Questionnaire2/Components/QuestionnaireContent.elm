module Wizard.Components.Questionnaire2.Components.QuestionnaireContent exposing
    ( ItemEventData
    , Model
    , Msg
    , TypeHints
    , UpdateConfig
    , ViewConfig
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import CharIdentifier
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.Badge as Badge
import Common.Components.DatePicker as DatePicker
import Common.Components.FileDownloader as FileDownloader
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (fa, faAdd, faDelete, faError, faInfo, faKmQuestion, faNext, faPrev, faQuestionnaireClearAnswer, faQuestionnaireComments, faQuestionnaireCopyLink, faQuestionnaireDesirable, faQuestionnaireExperts, faQuestionnaireFollowUpsIndication, faQuestionnaireItemCollapse, faQuestionnaireItemCollapseAll, faQuestionnaireItemExpand, faQuestionnaireItemExpandAll, faQuestionnaireItemMoveDown, faQuestionnaireItemMoveUp, faQuestionnaireResourcePageReferences, faQuestionnaireUrlReferences, faRemove, faSearch, faSpinner)
import Common.Components.Modal as Modal
import Common.Components.Tooltip exposing (tooltip, tooltipLeft, tooltipRight)
import Common.Ports.Copy as Copy
import Common.Ports.Dom as Dom
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.FileIcon as FileIcon
import Common.Utils.Markdown as Markdown
import Common.Utils.RegexPatterns as RegexPatterns
import Common.Utils.TimeDistance as TimeDistance
import Common.Utils.TimeUtils as TimeUtils
import Debounce exposing (Debounce)
import Dict
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, a, button, div, h2, i, input, label, li, option, p, select, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, href, selected, target, type_, value)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown, onMouseOut)
import Html.Events.Extra exposing (onChange)
import Html.Extra as Html
import Html.Keyed
import Html.Lazy as Lazy
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import Set exposing (Set)
import String.Extra as String
import String.Format as String
import Task.Extra as Task
import Time.Distance as Time
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Wizard.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration
import Wizard.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent
import Wizard.Api.Models.ProjectDetail.Reply as Reply exposing (Reply)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Wizard.Api.Models.ProjectFileSimple as ProjectFileSimple
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Api.Models.TypeHint exposing (TypeHint)
import Wizard.Api.Models.TypeHintRequest as TypeHintRequest
import Wizard.Api.Models.User as User
import Wizard.Api.ProjectFiles as ProjectFilesApi
import Wizard.Api.TypeHints as TypeHintsApi
import Wizard.Components.Html exposing (resizableTextarea)
import Wizard.Components.Questionnaire2.Components.FileUploadModal as FileUploadModal
import Wizard.Components.Questionnaire2.QuestionViewFlags as QuestionViewFlags
import Wizard.Components.Questionnaire2.QuestionnaireRightPanel as QuestionnaireRightPanel exposing (PluginQuestionActionData, QuestionnaireRightPanel)
import Wizard.Components.Questionnaire2.QuestionnaireUpdateReturnData as QuestionnaireUpdateReturnData exposing (QuestionnaireUpdateReturnData)
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Components.Questionnaire2.QuestionnaireVirtualization exposing (ChapterLinksNodeData, ChapterNodeData, ContentNode(..), ItemFooterNodeData, ItemHeaderNodeData, ItemsEndNodeData, NestingType(..), QuestionExtraCrossReference, QuestionExtraData, QuestionExtraResourceCollection, QuestionExtraResourcePage, QuestionExtraUrlReference, QuestionNodeData, QuestionSpecificNodeData(..))
import Wizard.Components.Tag as Tag
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Plugins.Plugin exposing (Plugin, ProjectQuestionActionConnector, ProjectQuestionActionConnectorType(..))
import Wizard.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Wizard.Routes as Routes
import Wizard.Routing as Routing
import Wizard.Utils.Feature as Feature


type alias Model =
    { removeItem : Maybe ItemEventData
    , typeHints : Maybe TypeHints
    , typeHintsDebounce : Debounce ( String, String, String )
    , typeHintsNothing : Maybe TypeHints
    , recentlyCopiedLink : Bool
    , projectQuestionActionPlugins : List ( Plugin, ProjectQuestionActionConnector )
    }


type alias ItemEventData =
    { path : String
    , itemUuids : List String
    , itemUuid : String
    }


type alias TypeHints =
    { path : String
    , searchValue : String
    , hints : ActionResult (List TypeHint)
    }


init : List ( Plugin, ProjectQuestionActionConnector ) -> Model
init projectQuestionActionPlugins =
    { removeItem = Nothing
    , typeHints = Nothing
    , typeHintsDebounce = Debounce.init
    , typeHintsNothing = Nothing
    , recentlyCopiedLink = False
    , projectQuestionActionPlugins = projectQuestionActionPlugins
    }


type Msg
    = OpenChapter String
    | CollapsePaths (List String)
    | ExpandPaths (List String)
    | ScrollToPath String
    | ScrollToQuestion String
    | SetReply String ReplyValue
    | ClearReply String
    | AddItem String (List String)
    | RemoveItemOpen ItemEventData
    | RemoveItemConfirm
    | RemoveItemCancel
    | MoveItemUp ItemEventData
    | MoveItemDown ItemEventData
    | OpenFileUpload String FileUploadModal.FileConfig
    | DeleteFile Uuid String String
    | DownloadFile Uuid
    | FileDownloaderMsg FileDownloader.Msg
    | ShowTypeHints String Bool String String
    | HideTypeHints
    | TypeHintInput String Bool Bool String ReplyValue
    | TypeHintDebounceMsg Debounce.Msg
    | TypeHintsLoaded String String (Result ApiError (List TypeHint))
    | SetPluginReply String String
    | CopyLinkToQuestion String
    | ClearRecentlyCopied
    | AddTodo String
    | RemoveTodo String
    | OpenComments String
    | CloseRightPanel
    | OpenPluginQuestionActionModal Uuid PluginElement Question String
    | OpenPluginQuestionActionRightPanel PluginQuestionActionData


type alias UpdateConfig msg =
    { addTodoCmd : String -> Cmd msg
    , chapterUuid : String
    , closeRightPanelCmd : Cmd msg
    , collapsedPaths : Set String
    , deleteFileCmd : Uuid -> String -> String -> Cmd msg
    , knowledgeModelParentMap : KnowledgeModel.ParentMap
    , mbKmEditorUuid : Maybe Uuid
    , openChapterCmd : String -> Cmd msg
    , openCommentsCmd : String -> Cmd msg
    , openFileUploadCmd : String -> FileUploadModal.FileConfig -> Cmd msg
    , openPluginQuestionActionModalCmd : Uuid -> PluginElement -> Question -> String -> Cmd msg
    , openPluginQuestionActionRightPanelCmd : PluginQuestionActionData -> Cmd msg
    , questionnaire : ProjectQuestionnaire
    , removeTodoCmd : String -> Cmd msg
    , scrollToPathCmd : Bool -> String -> Cmd msg
    , updateCollapsedPathsCmd : Set String -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> QuestionnaireUpdateReturnData Model msg
update appState cfg msg model =
    case msg of
        OpenChapter chapterUuid ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.openChapterCmd chapterUuid)

        CollapsePaths paths ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.updateCollapsedPathsCmd (List.foldl Set.insert cfg.collapsedPaths paths))

        ExpandPaths paths ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.updateCollapsedPathsCmd (List.foldl Set.remove cfg.collapsedPaths paths))

        ScrollToPath path ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.scrollToPathCmd False path)

        ScrollToQuestion questionUuid ->
            case ProjectQuestionnaire.getClosestQuestionParentPath cfg.questionnaire cfg.knowledgeModelParentMap questionUuid of
                Just path ->
                    QuestionnaireUpdateReturnData.fromModelCmd appState
                        model
                        (cfg.scrollToPathCmd True path)

                Nothing ->
                    QuestionnaireUpdateReturnData.fromModel appState model

        SetReply path replyValue ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                setReplyEvent =
                    ProjectEvent.SetReply
                        { uuid = newUuid
                        , path = path
                        , value = replyValue
                        , createdAt = appState.currentTime
                        , createdBy = getCreatedBy appState
                        }
            in
            { seed = newSeed
            , model = model
            , cmd = Cmd.none
            , event = Just setReplyEvent
            }

        ClearReply path ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                clearReplyEvent =
                    ProjectEvent.ClearReply
                        { uuid = newUuid
                        , path = path
                        , createdAt = appState.currentTime
                        , createdBy = getCreatedBy appState
                        }
            in
            { seed = newSeed
            , model = model
            , cmd = Cmd.none
            , event = Just clearReplyEvent
            }

        AddItem path originalItemUuids ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                addItemEvent =
                    ProjectEvent.SetReply
                        { uuid = newUuid
                        , path = path
                        , value = ReplyValue.ItemListReply (originalItemUuids ++ [ Uuid.toString newUuid ])
                        , createdAt = appState.currentTime
                        , createdBy = getCreatedBy appState
                        }

                itemPath =
                    path ++ "." ++ Uuid.toString newUuid ++ "-header"

                scrollCmd =
                    Dom.scrollIntoView ("[data-path=\"" ++ itemPath ++ "\"]")
            in
            { seed = newSeed
            , model = model
            , cmd = scrollCmd
            , event = Just addItemEvent
            }

        RemoveItemOpen itemEventData ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | removeItem = Just itemEventData }

        RemoveItemConfirm ->
            case model.removeItem of
                Just itemEventData ->
                    let
                        ( newUuid, newSeed ) =
                            Uuid.step appState.seed

                        newItems =
                            List.filter ((/=) itemEventData.itemUuid) itemEventData.itemUuids

                        removeItemEvent =
                            ProjectEvent.SetReply
                                { uuid = newUuid
                                , path = itemEventData.path
                                , value = ReplyValue.ItemListReply newItems
                                , createdAt = appState.currentTime
                                , createdBy = getCreatedBy appState
                                }
                    in
                    { seed = newSeed
                    , model = { model | removeItem = Nothing }
                    , cmd = Cmd.none
                    , event = Just removeItemEvent
                    }

                Nothing ->
                    QuestionnaireUpdateReturnData.fromModel appState { model | removeItem = Nothing }

        RemoveItemCancel ->
            QuestionnaireUpdateReturnData.fromModel appState { model | removeItem = Nothing }

        MoveItemUp itemEventData ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                newItems =
                    case List.elemIndex itemEventData.itemUuid itemEventData.itemUuids of
                        Just index ->
                            if index > 0 then
                                List.swapAt index (index - 1) itemEventData.itemUuids

                            else
                                itemEventData.itemUuids

                        Nothing ->
                            itemEventData.itemUuids

                moveItemUpEvent =
                    ProjectEvent.SetReply
                        { uuid = newUuid
                        , path = itemEventData.path
                        , value = ReplyValue.ItemListReply newItems
                        , createdAt = appState.currentTime
                        , createdBy = getCreatedBy appState
                        }
            in
            { seed = newSeed
            , model = model
            , cmd = Cmd.none
            , event = Just moveItemUpEvent
            }

        MoveItemDown itemEventData ->
            let
                ( newUuid, newSeed ) =
                    Uuid.step appState.seed

                newItems =
                    case List.elemIndex itemEventData.itemUuid itemEventData.itemUuids of
                        Just index ->
                            if index < List.length itemEventData.itemUuids - 1 then
                                List.swapAt index (index + 1) itemEventData.itemUuids

                            else
                                itemEventData.itemUuids

                        Nothing ->
                            itemEventData.itemUuids

                moveItemDownEvent =
                    ProjectEvent.SetReply
                        { uuid = newUuid
                        , path = itemEventData.path
                        , value = ReplyValue.ItemListReply newItems
                        , createdAt = appState.currentTime
                        , createdBy = getCreatedBy appState
                        }
            in
            { seed = newSeed
            , model = model
            , cmd = Cmd.none
            , event = Just moveItemDownEvent
            }

        OpenFileUpload questionPath fileConfig ->
            { seed = appState.seed
            , model = model
            , cmd = cfg.openFileUploadCmd questionPath fileConfig
            , event = Nothing
            }

        DeleteFile fileUuid questionPath fileName ->
            { seed = appState.seed
            , model = model
            , cmd = cfg.deleteFileCmd fileUuid questionPath fileName
            , event = Nothing
            }

        DownloadFile fileUuid ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (Cmd.map (cfg.wrapMsg << FileDownloaderMsg)
                    (FileDownloader.fetchFile (AppState.toServerInfo appState) (ProjectFilesApi.fileUrl cfg.questionnaire.uuid fileUuid))
                )

        FileDownloaderMsg fileDownloaderMsg ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (Cmd.map (cfg.wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg))

        ShowTypeHints path emptySearch questionUuid value ->
            if not emptySearch && String.isEmpty value then
                QuestionnaireUpdateReturnData.fromModel appState model

            else
                let
                    typeHints =
                        Just
                            { path = path
                            , searchValue = value
                            , hints = ActionResult.Loading
                            }

                    cmd =
                        Cmd.map cfg.wrapMsg <|
                            loadTypeHints appState cfg.mbKmEditorUuid cfg.questionnaire.uuid path questionUuid value
                in
                QuestionnaireUpdateReturnData.fromModelCmd appState
                    { model | typeHints = typeHints }
                    cmd

        HideTypeHints ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | typeHints = Nothing }

        TypeHintInput path allowCustomReply emptySearch questionUuid replyValue ->
            let
                ( ( debounce, debounceCmd ), newTypeHints ) =
                    case ( emptySearch, replyValue ) of
                        ( False, IntegrationReply (IntegrationReplyType.PlainType "") ) ->
                            ( ( model.typeHintsDebounce, Cmd.none ), Nothing )

                        _ ->
                            let
                                replyValueString =
                                    ReplyValue.getStringReply replyValue

                                updatedTypeHints =
                                    Just
                                        { path = path
                                        , searchValue = replyValueString
                                        , hints = ActionResult.Loading
                                        }
                            in
                            ( Debounce.push
                                debounceConfig
                                ( path, questionUuid, replyValueString )
                                model.typeHintsDebounce
                            , updatedTypeHints
                            )

                dispatchCmd =
                    if allowCustomReply then
                        Task.dispatch (cfg.wrapMsg (SetReply path replyValue))

                    else
                        Cmd.none
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                { model | typeHints = newTypeHints, typeHintsDebounce = debounce }
                (Cmd.batch [ Cmd.map cfg.wrapMsg debounceCmd, dispatchCmd ])

        TypeHintDebounceMsg debounceMsg ->
            let
                load ( path, questionUuid, value ) =
                    loadTypeHints appState cfg.mbKmEditorUuid cfg.questionnaire.uuid path questionUuid value

                ( debounce, debounceCmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast load) debounceMsg model.typeHintsDebounce
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                { model | typeHintsDebounce = debounce }
                (Cmd.map cfg.wrapMsg debounceCmd)

        TypeHintsLoaded path value result ->
            case model.typeHints of
                Just typeHints ->
                    if typeHints.path == path && typeHints.searchValue == value then
                        case result of
                            Ok hints ->
                                QuestionnaireUpdateReturnData.fromModel appState
                                    { model | typeHints = Just { typeHints | hints = ActionResult.Success hints } }

                            Err _ ->
                                QuestionnaireUpdateReturnData.fromModel appState
                                    { model | typeHints = Just { typeHints | hints = ActionResult.Error (gettext "Unable to get type hints." appState.locale) } }

                    else
                        QuestionnaireUpdateReturnData.fromModel appState model

                Nothing ->
                    QuestionnaireUpdateReturnData.fromModel appState model

        SetPluginReply path replyString ->
            case D.decodeString ReplyValue.decoder replyString of
                Ok replyValue ->
                    let
                        ( newUuid, newSeed ) =
                            Uuid.step appState.seed

                        setReplyEvent =
                            ProjectEvent.SetReply
                                { uuid = newUuid
                                , path = path
                                , value = replyValue
                                , createdAt = appState.currentTime
                                , createdBy = getCreatedBy appState
                                }
                    in
                    { seed = newSeed
                    , model = model
                    , cmd = Cmd.none
                    , event = Just setReplyEvent
                    }

                Err _ ->
                    QuestionnaireUpdateReturnData.fromModel appState model

        CopyLinkToQuestion questionPath ->
            let
                questionLink =
                    Routes.projectsDetailQuestionnaire cfg.questionnaire.uuid (Just questionPath) Nothing
                        |> Routing.toUrl
                        |> (++) (AppState.getClientUrlRoot appState)
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                { model | recentlyCopiedLink = True }
                (Copy.copyToClipboard questionLink)

        ClearRecentlyCopied ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | recentlyCopiedLink = False }

        AddTodo questionPath ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.addTodoCmd questionPath)

        RemoveTodo questionPath ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.removeTodoCmd questionPath)

        OpenComments questionPath ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.openCommentsCmd questionPath)

        CloseRightPanel ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                cfg.closeRightPanelCmd

        OpenPluginQuestionActionModal pluginUuid pluginElement question questionPath ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.openPluginQuestionActionModalCmd pluginUuid pluginElement question questionPath)

        OpenPluginQuestionActionRightPanel pluginQuestionActionData ->
            QuestionnaireUpdateReturnData.fromModelCmd appState
                model
                (cfg.openPluginQuestionActionRightPanelCmd pluginQuestionActionData)


getCreatedBy : AppState -> Maybe UserSuggestion
getCreatedBy appState =
    Maybe.map UserConfig.toUserSuggestion appState.config.user


loadTypeHints : AppState -> Maybe Uuid -> Uuid -> String -> String -> String -> Cmd Msg
loadTypeHints appState mbKmEditorUuid questionnaireUuid path questionUuidStr value =
    let
        questionUuid =
            Uuid.fromUuidString questionUuidStr

        typeHintRequest =
            case mbKmEditorUuid of
                Just kmEditorUuid ->
                    TypeHintRequest.fromKmEditorQuestion kmEditorUuid questionUuid value

                Nothing ->
                    TypeHintRequest.fromProject questionnaireUuid questionUuid value
    in
    TypeHintsApi.fetchTypeHints appState typeHintRequest (TypeHintsLoaded path value)


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = TypeHintDebounceMsg
    }


type alias ViewConfig =
    { collapsedPaths : Set String
    , content : List ContentNode
    , featuresEnabled : Bool
    , mbHighlightedPath : Maybe String
    , questionnaire : ProjectQuestionnaire
    , rightPanel : QuestionnaireRightPanel
    , showActions : Bool
    , readonly : Bool
    , viewSettings : QuestionnaireViewSettings
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    let
        commentsEnabled =
            Feature.projectCommentAdd appState cfg.questionnaire

        questionViewFlags =
            QuestionViewFlags.fromQuestionnaireViewSettings
                cfg.viewSettings
                commentsEnabled
                cfg.showActions
                cfg.readonly
                model.recentlyCopiedLink
                |> QuestionViewFlags.toInt

        questionnaireContent =
            List.map (viewNode appState cfg model questionViewFlags) cfg.content

        otherComponents =
            [ ( "remove-modal", viewRemoveItemModal appState cfg model )
            ]
    in
    Html.Keyed.node "div"
        [ class "questionnaireContent container" ]
        (questionnaireContent ++ otherComponents)


viewNode : AppState -> ViewConfig -> Model -> Int -> ContentNode -> ( String, Html Msg )
viewNode appState cfg model questionViewFlags contentNode =
    case contentNode of
        ChapterNode chapterData ->
            viewChapterNode cfg chapterData

        ChapterEmptyNode ->
            viewChapterEmptyNode appState

        ChapterLinksNode chapterLinksData ->
            viewChapterLinksNode appState chapterLinksData

        QuestionNode questionData ->
            viewQuestionNode appState cfg model questionViewFlags questionData

        ItemHeaderNode itemData ->
            viewItemHeaderNode appState cfg questionViewFlags itemData

        ItemFooterNode itemData ->
            viewItemFooterNode appState itemData

        ItemsEndNode itemData ->
            viewItemsEndNode appState cfg questionViewFlags itemData



-- CHAPTER


viewChapterNode : ViewConfig -> ChapterNodeData -> ( String, Html msg )
viewChapterNode cfg chapterNodeData =
    let
        isHighlighted =
            cfg.mbHighlightedPath == Just chapterNodeData.chapter.uuid
    in
    ( chapterNodeData.chapter.uuid
    , Lazy.lazy2 viewChapterNodeLazy chapterNodeData isHighlighted
    )


viewChapterNodeLazy : ChapterNodeData -> Bool -> Html msg
viewChapterNodeLazy { chapter, chapterNumber } isHighlighted =
    div
        [ class "questionnaireContent__chapter"
        , classList [ ( "questionnaireContent__scrollTargetHighlight", isHighlighted ) ]
        ]
        [ h2 [] [ text (chapterNumber ++ ". " ++ chapter.title) ]
        , questionnaireMarkdown [] (Maybe.withDefault "" chapter.text)
        ]


viewChapterEmptyNode : AppState -> ( String, Html msg )
viewChapterEmptyNode appState =
    ( "chapter-empty"
    , Flash.info (gettext "This chapter contains no questions." appState.locale)
    )


viewChapterLinksNode : AppState -> ChapterLinksNodeData -> ( String, Html Msg )
viewChapterLinksNode appState chapterLinksNodeData =
    ( "chapter-links-" ++ chapterLinksNodeData.chapterUuid
    , Lazy.lazy2 viewChapterLinksNodeLazy appState.locale chapterLinksNodeData
    )


viewChapterLinksNodeLazy : Gettext.Locale -> ChapterLinksNodeData -> Html Msg
viewChapterLinksNodeLazy locale nodeData =
    let
        viewPrevChapterLink =
            viewChapterLink "questionnaireContent__chapterLink--prev"
                (gettext "Previous chapter" locale)
                faPrev

        viewNextChapterLink =
            viewChapterLink "questionnaireContent__chapterLink--next"
                (gettext "Next chapter" locale)
                faNext

        prevChapterLink =
            Maybe.unwrap Html.nothing viewPrevChapterLink nodeData.previousChapter

        nextChapterLink =
            Maybe.unwrap Html.nothing viewNextChapterLink nodeData.nextChapter
    in
    div [ class "mt-5 pt-3 pb-3 d-flex flex-gap-2" ] [ prevChapterLink, nextChapterLink ]


viewChapterLink : String -> String -> Html Msg -> Chapter -> Html Msg
viewChapterLink cls label icon c =
    div
        [ class ("questionnaireContent__chapterLink rounded-3 py-3 " ++ cls)
        , onClick (OpenChapter c.uuid)
        ]
        [ div [ class "text-lighter" ] [ text label ]
        , text c.title
        , icon
        ]



-- QUESTION


viewQuestionNode : AppState -> ViewConfig -> Model -> Int -> QuestionNodeData -> ( String, Html Msg )
viewQuestionNode appState cfg model questionViewFlags questionNodeData =
    let
        mbReply =
            Dict.get questionNodeData.questionPath cfg.questionnaire.replies

        newQuestionViewFlags =
            questionViewFlags
                |> QuestionViewFlags.addHasCommentsOpen (QuestionnaireRightPanel.commentsOpen questionNodeData.questionPath cfg.rightPanel)
                |> QuestionViewFlags.addHasTodo (ProjectQuestionnaire.hasTodo cfg.questionnaire questionNodeData.questionPath)
                |> QuestionViewFlags.addIsHighlighted (cfg.mbHighlightedPath == Just questionNodeData.questionPath)

        commentCount =
            ProjectQuestionnaire.getUnresolvedCommentCount questionNodeData.questionPath cfg.questionnaire

        questionContent =
            case questionNodeData.question of
                Question.OptionsQuestion _ _ ->
                    viewQuestionOptions appState model newQuestionViewFlags questionNodeData mbReply commentCount

                Question.ListQuestion _ _ ->
                    viewQuestionList appState model newQuestionViewFlags questionNodeData mbReply commentCount

                Question.ValueQuestion _ _ ->
                    viewQuestionValue appState model newQuestionViewFlags questionNodeData mbReply commentCount

                Question.MultiChoiceQuestion _ _ ->
                    viewQuestionMultiChoice appState model newQuestionViewFlags questionNodeData mbReply commentCount

                Question.ItemSelectQuestion _ _ ->
                    viewQuestionItemSelect appState model cfg newQuestionViewFlags questionNodeData mbReply commentCount

                Question.FileQuestion _ _ ->
                    viewQuestionFile appState model cfg newQuestionViewFlags questionNodeData mbReply commentCount

                Question.IntegrationQuestion _ _ ->
                    viewQuestionIntegration appState model newQuestionViewFlags questionNodeData mbReply commentCount
    in
    ( questionNodeData.questionPath
    , questionContent
    )


type alias ViewQuestionWrapperProps =
    { commentCount : Int
    , isAnswered : Bool
    , locale : Gettext.Locale
    , pluginActions : List ( Plugin, ProjectQuestionActionConnector )
    , questionNodeData : QuestionNodeData
    , questionViewFlags : Int
    }


type QuestionViewState
    = Answered
    | Desirable
    | Default


viewQuestionWrapper : ViewQuestionWrapperProps -> List (Html Msg) -> Html Msg
viewQuestionWrapper props content =
    let
        questionState =
            if props.isAnswered then
                Answered

            else if props.questionNodeData.isDesirable then
                Desirable

            else
                Default

        ( icon, tooltipText ) =
            case questionState of
                Answered ->
                    ( "fas fa-check", gettext "This question has been answered" props.locale )

                Desirable ->
                    ( "fas fa-pen", gettext "This question should be answered now" props.locale )

                Default ->
                    ( "far fa-hourglass", gettext "This question can be answered later" props.locale )

        questionText =
            Maybe.unwrap Html.nothing (questionnaireMarkdown [ class "text-muted mt-1", dataCy "questionnaire_question-text" ]) (Question.getText props.questionNodeData.question)

        questionTags =
            if QuestionViewFlags.showTags props.questionViewFlags && not (List.isEmpty props.questionNodeData.tags) then
                Tag.viewList { showDescription = False } props.questionNodeData.tags

            else
                Html.nothing
    in
    wrapNestingType props.questionNodeData.nestingType
        [ div
            [ class "questionnaireContent__questionHeader"
            , classList [ ( "questionnaireContent__scrollTargetHighlight", QuestionViewFlags.isHighlighted props.questionViewFlags ) ]
            , attribute "data-path" props.questionNodeData.questionPath
            ]
            [ div [ class "d-flex align-items-baseline mb-1" ]
                [ div [ class "flex-grow-1" ]
                    [ Badge.badge
                        [ class "mb-1 me-2 py-1 px-2 fs-6"
                        , classList
                            [ ( "bg-success", questionState == Answered )
                            , ( "bg-danger", questionState == Desirable )
                            , ( "bg-secondary", questionState == Default )
                            ]
                        ]
                        [ span (tooltipRight tooltipText)
                            [ fa (icon ++ " fa-fw") ]
                        , text (String.join "." props.questionNodeData.humanIdentifier)
                        ]
                    , strong
                        [ class "questionnaireContent__questionTitle"
                        , classList
                            [ ( "text-success", questionState == Answered )
                            , ( "text-danger", questionState == Desirable )
                            , ( "text-secondary", questionState == Default )
                            ]
                        , dataCy "questionnaire_question-title"
                        ]
                        [ text (Question.getTitle props.questionNodeData.question) ]
                    ]
                , viewQuestionActions
                    { commentCount = props.commentCount
                    , locale = props.locale
                    , pluginActions = props.pluginActions
                    , questionNodeData = props.questionNodeData
                    , questionViewFlags = props.questionViewFlags
                    }
                ]
            , questionTags
            , questionText
            , viewQuestionExtra
                { locale = props.locale
                , questionViewFlags = props.questionViewFlags
                , questionExtraData = props.questionNodeData.questionExtraData
                }
            ]
        , div [ class "questionnaireContent__questionInputs" ]
            content
        ]


type alias ViewQuestionActionsProps =
    { commentCount : Int
    , locale : Gettext.Locale
    , pluginActions : List ( Plugin, ProjectQuestionActionConnector )
    , questionNodeData : QuestionNodeData
    , questionViewFlags : Int
    }


viewQuestionActions : ViewQuestionActionsProps -> Html Msg
viewQuestionActions props =
    let
        copyText =
            if QuestionViewFlags.recentlyCopiedLink props.questionViewFlags then
                gettext "Copied!" props.locale

            else
                gettext "Copy link" props.locale

        copyLinkAction =
            a
                (class "questionnaireContent__questionAction"
                    :: onClick (CopyLinkToQuestion props.questionNodeData.questionPath)
                    :: onMouseOut ClearRecentlyCopied
                    :: tooltipLeft copyText
                )
                [ faQuestionnaireCopyLink ]

        todoAction =
            Html.viewIf (not (QuestionViewFlags.isReadOnly props.questionViewFlags)) <|
                if QuestionViewFlags.hasTodo props.questionViewFlags then
                    a
                        [ class "questionnaireContent__questionAction questionnaireContent__questionAction--todo"
                        , onClick (RemoveTodo props.questionNodeData.questionPath)
                        ]
                        [ span [] [ text (gettext "TODO" props.locale) ]
                        , a (class "text-danger" :: tooltip (gettext "Remove TODO" props.locale))
                            [ faRemove ]
                        ]

                else
                    a
                        [ class "questionnaireContent__questionAction questionnaireContent__questionAction--addTodo"
                        , onClick (AddTodo props.questionNodeData.questionPath)
                        ]
                        [ faAdd
                        , span [] [ span [] [ text (gettext "Add TODO" props.locale) ] ]
                        ]

        commentsOpen =
            QuestionViewFlags.hasCommentsOpen props.questionViewFlags

        commentsActionTooltip =
            if commentsOpen then
                []

            else if props.commentCount > 0 then
                tooltip <| String.format (ngettext ( "View 1 comment", "View %s comments" ) props.commentCount props.locale) [ String.fromInt props.commentCount ]

            else
                tooltip <| gettext "Add comment" props.locale

        commentsOnClickMsg =
            if commentsOpen then
                CloseRightPanel

            else
                OpenComments props.questionNodeData.questionPath

        commentsAction =
            Html.viewIf (QuestionViewFlags.commentsEnabled props.questionViewFlags) <|
                a
                    (class "questionnaireContent__questionAction"
                        :: classList
                            [ ( "questionnaireContent__questionAction--comments", props.commentCount > 0 )
                            , ( "questionnaireContent__questionAction--open", commentsOpen )
                            ]
                        :: onClick commentsOnClickMsg
                        :: dataCy "questionnaire_question-action_comment"
                        :: commentsActionTooltip
                    )
                    [ faQuestionnaireComments
                    , Html.viewIf (props.commentCount > 0) <|
                        text (String.format (ngettext ( "1 comment", "%s comments" ) props.commentCount props.locale) [ String.fromInt props.commentCount ])
                    ]

        pluginQuestionActions =
            if QuestionViewFlags.isReadOnly props.questionViewFlags then
                []

            else
                viewPluginQuestionActions
                    { locale = props.locale
                    , pluginActions = props.pluginActions
                    , questionNodeData = props.questionNodeData
                    }
    in
    Html.viewIf (QuestionViewFlags.showActions props.questionViewFlags) <|
        div [ class "d-flex" ]
            ([ todoAction, commentsAction ] ++ pluginQuestionActions ++ [ copyLinkAction ])


type alias ViewPluginQuestionActionsProps =
    { locale : Gettext.Locale
    , pluginActions : List ( Plugin, ProjectQuestionActionConnector )
    , questionNodeData : QuestionNodeData
    }


viewPluginQuestionActions : ViewPluginQuestionActionsProps -> List (Html Msg)
viewPluginQuestionActions props =
    let
        viewPluginButton ( plugin, connector ) =
            let
                isOpen =
                    case props.questionNodeData.pluginOpen of
                        Just ( pluginUuid, pluginConnector ) ->
                            (pluginUuid == plugin.uuid) && (pluginConnector == connector)

                        Nothing ->
                            False

                clickAction =
                    case connector.type_ of
                        ModalProjectQuestionAction ->
                            OpenPluginQuestionActionModal
                                plugin.uuid
                                connector.element
                                props.questionNodeData.question
                                props.questionNodeData.questionPath

                        SidebarProjectQuestionAction ->
                            if isOpen then
                                CloseRightPanel

                            else
                                OpenPluginQuestionActionRightPanel
                                    { plugin = plugin
                                    , connector = connector
                                    , question = props.questionNodeData.question
                                    , questionPath = props.questionNodeData.questionPath
                                    }
            in
            a
                (class "questionnaireContent__questionAction"
                    :: classList [ ( "questionnaireContent__questionAction--open", isOpen ) ]
                    :: onClick clickAction
                    :: tooltip (gettext connector.action.name props.locale)
                )
                [ fa connector.action.icon ]
    in
    List.map viewPluginButton props.pluginActions


type alias ViewQuestionExtraProps =
    { locale : Gettext.Locale
    , questionViewFlags : Int
    , questionExtraData : QuestionExtraData
    }


viewQuestionExtra : ViewQuestionExtraProps -> Html Msg
viewQuestionExtra props =
    let
        isEmpty =
            List.isEmpty props.questionExtraData.resourceCollections
                && List.isEmpty props.questionExtraData.urlReferences
                && List.isEmpty props.questionExtraData.crossReferences
                && List.isEmpty props.questionExtraData.experts
                && Maybe.isNothing props.questionExtraData.requiredPhase

        viewRequiredPhase =
            case ( QuestionViewFlags.showPhases props.questionViewFlags, props.questionExtraData.requiredPhase ) of
                ( True, Just phase ) ->
                    span []
                        [ span [ class "caption" ]
                            [ faQuestionnaireDesirable
                            , text (gettext "Desirable" props.locale)
                            , text ": "
                            , span [] [ text phase.title ]
                            ]
                        ]

                _ ->
                    Html.nothing

        viewResourceCollections =
            List.map viewResourceCollection props.questionExtraData.resourceCollections

        viewUrlReferences =
            viewExtraItems
                { icon = faQuestionnaireUrlReferences
                , label = gettext "External links" props.locale
                , viewItem = viewUrlReference
                }
                props.questionExtraData.urlReferences

        viewCrossReferences =
            viewExtraItems
                { icon = faKmQuestion
                , label = gettext "Related questions" props.locale
                , viewItem = viewCrossReference
                }
                props.questionExtraData.crossReferences

        viewExperts =
            viewExtraItems
                { icon = faQuestionnaireExperts
                , label = gettext "Experts" props.locale
                , viewItem = viewExpert
                }
                props.questionExtraData.experts
    in
    Html.viewIf (not isEmpty) <|
        div
            [ class "questionnaireContent__questionExtra"
            , dataCy "questionnaire_question-extra"
            ]
            (viewRequiredPhase
                :: viewResourceCollections
                ++ [ viewUrlReferences
                   , viewCrossReferences
                   , viewExperts
                   ]
            )


type alias ViewExtraItemsProps a msg =
    { icon : Html msg
    , label : String
    , viewItem : a -> Html msg
    }


viewExtraItems : ViewExtraItemsProps a msg -> List a -> Html msg
viewExtraItems props list =
    if List.isEmpty list then
        Html.nothing

    else
        let
            items =
                List.map props.viewItem list
                    |> List.intersperse (span [ class "separator" ] [ text "," ])
        in
        span []
            (span [ class "caption" ] [ props.icon, text (props.label ++ ": ") ] :: items)


viewResourceCollection : QuestionExtraResourceCollection -> Html Msg
viewResourceCollection data =
    viewExtraItems
        { icon = faQuestionnaireResourcePageReferences
        , label = data.title
        , viewItem = viewResourcePageReference
        }
        data.resourcePages


viewResourcePageReference : QuestionExtraResourcePage -> Html Msg
viewResourcePageReference data =
    a
        [ href data.url, target "_blank" ]
        [ text data.title ]


viewUrlReference : QuestionExtraUrlReference -> Html Msg
viewUrlReference data =
    let
        urlLabel =
            String.withDefault data.url data.label
    in
    a [ href data.url, target "_blank" ]
        [ text urlLabel ]


viewCrossReference : QuestionExtraCrossReference -> Html Msg
viewCrossReference data =
    span []
        [ a [ onClick (ScrollToQuestion data.targetQuestionUuid) ]
            [ text data.targetQuestionTitle ]
        , Html.viewIf (not (String.isEmpty data.description)) <|
            text (" (" ++ data.description ++ ")")
        ]


viewExpert : Expert -> Html msg
viewExpert expert =
    let
        mail =
            a [ href <| "mailto:" ++ expert.email ] [ text expert.email ]
    in
    span [] <|
        if String.isEmpty expert.name then
            [ mail ]

        else
            [ text expert.name
            , text " ("
            , mail
            , text ")"
            ]


type alias ViewQuestionClearReplyProps =
    { isAnswered : Bool
    , locale : Gettext.Locale
    , questionPath : String
    , readonly : Bool
    }


viewQuestionClearReply : ViewQuestionClearReplyProps -> Html Msg
viewQuestionClearReply props =
    if props.readonly || not props.isAnswered then
        Html.nothing

    else
        div [ class "questionnaireContent__clearReply" ]
            [ a [ onClick (ClearReply props.questionPath) ]
                [ faQuestionnaireClearAnswer
                , text (gettext "Clear answer" props.locale)
                ]
            ]


type alias ViewQuestionAnsweredByProps =
    { locale : Gettext.Locale
    , mbReply : Maybe Reply
    , question : Question
    , replyTime : String
    , questionViewFlags : Int
    }


viewQuestionAnsweredBy : ViewQuestionAnsweredByProps -> Html Msg
viewQuestionAnsweredBy props =
    let
        isVisible =
            QuestionViewFlags.showAnsweredBy props.questionViewFlags && not (Question.isList props.question)
    in
    case ( props.mbReply, questionReplyTimeFromString props.replyTime, isVisible ) of
        ( Just reply, Just ( readableTime, timeDiff ), True ) ->
            let
                userName =
                    case reply.createdBy of
                        Just userInfo ->
                            User.fullName userInfo

                        Nothing ->
                            gettext "anonymous user" props.locale
            in
            div [ class "questionnaireContent__answeredBy", dataCy "questionnaire_answered-by" ]
                (String.formatHtml (gettext "Answered %s by %s." props.locale)
                    [ span (tooltip readableTime) [ text timeDiff ]
                    , text userName
                    ]
                )

        _ ->
            Html.nothing



-- QUESTION OPTIONS


viewQuestionOptions : AppState -> Model -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionOptions appState model questionViewFlags questionNodeData mbReply commentCount =
    Lazy.lazy7 viewQuestionOptionsLazy
        appState.locale
        model.projectQuestionActionPlugins
        questionNodeData
        questionViewFlags
        (getQuestionReplyTime appState mbReply)
        (replyToString mbReply)
        commentCount


viewQuestionOptionsLazy :
    Gettext.Locale
    -> List ( Plugin, ProjectQuestionActionConnector )
    -> QuestionNodeData
    -> Int
    -> String
    -> String
    -> Int
    -> Html Msg
viewQuestionOptionsLazy locale pluginActions questionNodeData questionViewFlags replyTime replyString commentCount =
    let
        { question, questionPath, questionSpecificData } =
            questionNodeData

        mbReply =
            replyFromString replyString

        ( answers, followUpsCollapsed, metrics ) =
            case questionSpecificData of
                OptionsQuestionSpecificNodeData data ->
                    ( data.answers
                    , data.followUpsCollapsed
                    , data.metrics
                    )

                _ ->
                    ( [], False, [] )

        mbSelectedAnswerUuid =
            Maybe.map (ReplyValue.getAnswerUuid << .value) mbReply

        mbAnswer =
            case mbSelectedAnswerUuid of
                Just selectedAnswerUuid ->
                    List.find ((==) selectedAnswerUuid << .uuid) answers

                Nothing ->
                    Nothing

        viewAnswer_ =
            viewAnswer
                { locale = locale
                , mbSelectedAnswerUuid = mbSelectedAnswerUuid
                , metrics = metrics
                , questionPath = questionPath
                , questionViewFlags = questionViewFlags
                }

        answersView =
            List.indexedMap viewAnswer_ answers

        ( followUpsCount, answerPath ) =
            case mbAnswer of
                Just answer ->
                    ( List.length answer.followUpUuids
                    , questionPath ++ "." ++ answer.uuid
                    )

                Nothing ->
                    ( 0, "" )

        isAnswered =
            Maybe.isJust mbSelectedAnswerUuid
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__options" ] answersView
        , viewQuestionClearReply
            { isAnswered = isAnswered
            , locale = locale
            , questionPath = questionPath
            , readonly = QuestionViewFlags.isReadOnly questionViewFlags
            }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        , viewAnswerAdvice
            { mbAnswer = mbAnswer
            , mbReply = mbReply
            }
        , Html.viewIf (followUpsCount > 0) <|
            viewFollowUpsCollapse
                { answerPath = answerPath
                , followUpsCount = followUpsCount
                , isCollapsed = followUpsCollapsed
                , locale = locale
                }
        ]


type alias ViewAnswerProps =
    { locale : Gettext.Locale
    , mbSelectedAnswerUuid : Maybe String
    , metrics : List Metric
    , questionPath : String
    , questionViewFlags : Int
    }


viewAnswer : ViewAnswerProps -> Int -> Answer -> Html Msg
viewAnswer props order answer =
    let
        humanIdentifier =
            CharIdentifier.fromInt order ++ ". "

        followUpsIndicator =
            if List.isEmpty answer.followUpUuids then
                Html.nothing

            else
                span (class "ms-3 text-muted" :: tooltipRight (gettext "This option leads to some follow up questions." props.locale))
                    [ faQuestionnaireFollowUpsIndication
                    ]

        isSelected =
            props.mbSelectedAnswerUuid == Just answer.uuid

        isDisabled =
            QuestionViewFlags.isReadOnly props.questionViewFlags

        setReply =
            SetReply props.questionPath (ReplyValue.AnswerReply answer.uuid)
    in
    div
        [ class "questionnaireContent__option"
        , classList
            [ ( "questionnaireContent__option--selected", isSelected )
            , ( "questionnaireContent__option--disabled", isDisabled )
            ]
        ]
        [ label []
            [ input
                [ type_ "radio"
                , checked isSelected
                , disabled isDisabled
                , onClick setReply
                ]
                []
            , text humanIdentifier
            , text answer.label
            , followUpsIndicator
            , viewAnswerMetrics
                { answer = answer
                , metrics = props.metrics
                , viewValue = QuestionViewFlags.showMetricValues props.questionViewFlags
                }
            ]
        ]


type alias ViewAnswerMetricsProps =
    { answer : Answer
    , metrics : List Metric
    , viewValue : Bool
    }


viewAnswerMetrics : ViewAnswerMetricsProps -> Html msg
viewAnswerMetrics props =
    if List.isEmpty props.answer.metricMeasures then
        Html.nothing

    else
        let
            getMetricName uuid =
                List.find ((==) uuid << .uuid) props.metrics
                    |> Maybe.map .title
                    |> Maybe.withDefault "Unknown"

            getBadgeClass value =
                (++) "bg-value-" <| String.fromInt <| (*) 10 <| round <| value * 10

            metricExists measure =
                List.find ((==) measure.metricUuid << .uuid) props.metrics /= Nothing

            metricValue metricMeasure =
                if props.viewValue then
                    span [ class "ms-1" ] [ text (String.fromInt (round (100 * metricMeasure.measure)) ++ "%") ]

                else
                    Html.nothing

            createBadge metricMeasure =
                Badge.badge
                    [ class (getBadgeClass metricMeasure.measure) ]
                    [ text <| getMetricName metricMeasure.metricUuid
                    , metricValue metricMeasure
                    ]
        in
        div [ class "badges" ]
            (List.filter metricExists props.answer.metricMeasures
                |> List.map createBadge
            )


type alias ViewAnswerAdviceProps =
    { mbAnswer : Maybe Answer
    , mbReply : Maybe Reply
    }


viewAnswerAdvice : ViewAnswerAdviceProps -> Html msg
viewAnswerAdvice props =
    props.mbAnswer
        |> Maybe.andThen .advice
        |> Maybe.unwrap Html.nothing (questionnaireMarkdown [ class "questionnaireContent__advice" ])


type alias ViewFollowUpsCollapseProps =
    { answerPath : String
    , followUpsCount : Int
    , isCollapsed : Bool
    , locale : Gettext.Locale
    }


viewFollowUpsCollapse : ViewFollowUpsCollapseProps -> Html Msg
viewFollowUpsCollapse props =
    if props.isCollapsed then
        let
            expandButton =
                a
                    [ class "with-icon"
                    , onClick (ExpandPaths [ props.answerPath ])
                    , dataCy "questionnaire_followups_expand"
                    ]
                    [ faQuestionnaireItemExpand
                    , text (gettext "Expand" props.locale)
                    ]
        in
        div [ class "questionnaireContent__followUpsCollapse questionnaireContent__followUpsCollapse--collapsed" ] <|
            String.formatHtml
                (ngettext ( "%s %s follow up question", "%s %s follow up questions" ) props.followUpsCount props.locale)
                [ expandButton
                , strong [] [ text (String.fromInt props.followUpsCount) ]
                ]

    else
        div [ class "questionnaireContent__followUpsCollapse" ]
            [ a
                [ class "with-icon"
                , onClick (CollapsePaths [ props.answerPath ])
                , dataCy "questionnaire_followups_collapse"
                ]
                [ faQuestionnaireItemCollapse
                , text (gettext "Collapse" props.locale)
                ]
            ]



-- QUESTION LIST


viewQuestionList : AppState -> Model -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionList appState model questionViewFlags questionNodeData mbReply commentCount =
    Lazy.lazy6 viewQuestionListLazy
        appState.locale
        model.projectQuestionActionPlugins
        questionNodeData
        questionViewFlags
        (replyToString mbReply)
        commentCount


viewQuestionListLazy : Gettext.Locale -> List ( Plugin, ProjectQuestionActionConnector ) -> QuestionNodeData -> Int -> String -> Int -> Html Msg
viewQuestionListLazy locale pluginActions questionNodeData questionViewFlags replyString commentCount =
    let
        mbReply =
            replyFromString replyString

        isAnswered =
            Maybe.isJust mbReply
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ viewItemCollapse
            { locale = locale
            , mbReply = mbReply
            , questionPath = questionNodeData.questionPath
            }
        ]



-- QUESTION VALUE


viewQuestionValue : AppState -> Model -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionValue appState model questionViewFlags questionNodeData mbReply commentCount =
    Lazy.lazy7 viewQuestionValueLazy
        appState.locale
        model.projectQuestionActionPlugins
        questionNodeData
        questionViewFlags
        (getQuestionReplyTime appState mbReply)
        (replyToString mbReply)
        commentCount


viewQuestionValueLazy : Gettext.Locale -> List ( Plugin, ProjectQuestionActionConnector ) -> QuestionNodeData -> Int -> String -> String -> Int -> Html Msg
viewQuestionValueLazy locale pluginActions questionNodeData questionViewFlags replyTime replyString commentCount =
    let
        { question, questionPath } =
            questionNodeData

        mbReply =
            replyFromString replyString

        isReadOnly =
            QuestionViewFlags.isReadOnly questionViewFlags

        defaultValue =
            if Question.getValueType question == Just QuestionValueType.ColorQuestionValueType then
                "#000000"

            else
                ""

        replyValue =
            Maybe.unwrap defaultValue (ReplyValue.getStringReply << .value) mbReply

        defaultAttrs =
            [ class "form-control", value replyValue ]

        toMsg =
            SetReply questionPath << ReplyValue.StringReply

        extraAttrs =
            if isReadOnly then
                [ disabled True ]

            else
                [ onInput toMsg ]

        defaultInput =
            [ input (type_ "text" :: defaultAttrs ++ extraAttrs) [] ]

        readonlyOr otherInput =
            if isReadOnly then
                defaultInput

            else
                otherInput

        warningView regex warning =
            if not (String.isEmpty replyValue) && not (Regex.contains regex replyValue) then
                Flash.warning warning

            else
                Html.nothing

        validationWarning validation =
            case QuestionValidation.validate { locale = locale } validation replyValue of
                Ok _ ->
                    Html.nothing

                Err error ->
                    Flash.warning error

        validations =
            case questionNodeData.questionSpecificData of
                ValueQuestionSpecificNodeData data ->
                    data.validations

                _ ->
                    []

        validationWarnings =
            List.map validationWarning validations

        inputView =
            case Question.getValueType question of
                Just QuestionValueType.NumberQuestionValueType ->
                    [ input (type_ "number" :: defaultAttrs ++ extraAttrs) [] ]

                Just QuestionValueType.DateQuestionValueType ->
                    readonlyOr [ DatePicker.datePicker [ DatePicker.onChange toMsg, DatePicker.value replyValue ] ]

                Just QuestionValueType.DateTimeQuestionValueType ->
                    readonlyOr [ DatePicker.dateTimePicker [ DatePicker.onChange toMsg, DatePicker.value replyValue ] ]

                Just QuestionValueType.TimeQuestionValueType ->
                    readonlyOr [ DatePicker.timePicker [ DatePicker.onChange toMsg, DatePicker.value replyValue ] ]

                Just QuestionValueType.EmailQuestionValueType ->
                    [ input (type_ "email" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.email (gettext "This is not a valid email address." locale)
                    ]

                Just QuestionValueType.UrlQuestionValueType ->
                    [ input (type_ "text" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.url (gettext "This is not a valid URL." locale)
                    ]

                Just QuestionValueType.TextQuestionValueType ->
                    [ resizableTextarea 3 replyValue (defaultAttrs ++ extraAttrs) [] ]

                Just QuestionValueType.ColorQuestionValueType ->
                    [ input (type_ "color" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.color (gettext "This is not a valid color." locale)
                    ]

                _ ->
                    defaultInput

        isAnswered =
            Maybe.isJust mbReply
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__value" ]
            (inputView ++ validationWarnings)
        , viewQuestionClearReply
            { isAnswered = isAnswered
            , locale = locale
            , questionPath = questionPath
            , readonly = isReadOnly
            }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        ]



-- QUESTION MULTICHOICE


viewQuestionMultiChoice : AppState -> Model -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionMultiChoice appState model questionViewFlags questionNodeData mbReply commentCount =
    Lazy.lazy7 viewQuestionMultiChoiceLazy
        appState.locale
        model.projectQuestionActionPlugins
        questionNodeData
        questionViewFlags
        (getQuestionReplyTime appState mbReply)
        (replyToString mbReply)
        commentCount


viewQuestionMultiChoiceLazy :
    Gettext.Locale
    -> List ( Plugin, ProjectQuestionActionConnector )
    -> QuestionNodeData
    -> Int
    -> String
    -> String
    -> Int
    -> Html Msg
viewQuestionMultiChoiceLazy locale pluginActions questionNodeData questionViewFlags replyTime replyString commentCount =
    let
        { question, questionPath, questionSpecificData } =
            questionNodeData

        mbReply =
            replyFromString replyString

        choices =
            case questionSpecificData of
                MultiChoiceQuestionSpecificNodeData data ->
                    data.choices

                _ ->
                    []

        mbSelectedChoiceUuids =
            Maybe.map (ReplyValue.getChoiceUuids << .value) mbReply

        viewChoice_ =
            viewChoice
                { locale = locale
                , mbSelectedChoiceUuids = mbSelectedChoiceUuids
                , questionPath = questionPath
                , readonly = QuestionViewFlags.isReadOnly questionViewFlags
                }

        choicesView =
            List.indexedMap viewChoice_ choices

        isAnswered =
            Maybe.isJust mbReply
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__options" ] choicesView
        , viewQuestionClearReply
            { isAnswered = isAnswered
            , locale = locale
            , questionPath = questionPath
            , readonly = QuestionViewFlags.isReadOnly questionViewFlags
            }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        ]


type alias ViewChoiceProps =
    { locale : Gettext.Locale
    , mbSelectedChoiceUuids : Maybe (List String)
    , questionPath : String
    , readonly : Bool
    }


viewChoice : ViewChoiceProps -> Int -> Choice -> Html Msg
viewChoice props order choice =
    let
        humanIdentifier =
            CharIdentifier.fromInt order ++ ". "

        selectedChoiceUuids =
            Maybe.withDefault [] props.mbSelectedChoiceUuids

        isSelected =
            List.member choice.uuid selectedChoiceUuids

        isDisabled =
            props.readonly

        toggleChoiceMsg =
            let
                newSelectedChoices =
                    if List.member choice.uuid selectedChoiceUuids then
                        List.filter ((/=) choice.uuid) selectedChoiceUuids

                    else
                        choice.uuid :: selectedChoiceUuids
            in
            SetReply props.questionPath (ReplyValue.MultiChoiceReply newSelectedChoices)
    in
    div
        [ class "questionnaireContent__option"
        , classList
            [ ( "questionnaireContent__option--selected", isSelected )
            , ( "questionnaireContent__option--disabled", isDisabled )
            ]
        ]
        [ label []
            [ input
                [ type_ "checkbox"
                , checked isSelected
                , disabled isDisabled
                , onClick toggleChoiceMsg
                ]
                []
            , text humanIdentifier
            , text choice.label
            ]
        ]



-- QUESTION ITEM SELECT


viewQuestionItemSelect : AppState -> Model -> ViewConfig -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionItemSelect appState model cfg questionViewFlags questionNodeData mbReply commentCount =
    let
        itemOptions =
            case Question.getListQuestionUuid questionNodeData.question of
                Just listQuestionUuid ->
                    let
                        mbItemQuestionUuid =
                            KnowledgeModel.getQuestion listQuestionUuid cfg.questionnaire.knowledgeModel
                                |> Maybe.map Question.getUuid
                    in
                    case mbItemQuestionUuid of
                        Just itemQuestionUuid ->
                            let
                                itemTemplateQuestions =
                                    case questionNodeData.questionSpecificData of
                                        ItemSelectQuestionSpecificNodeData data ->
                                            data.itemTemplateQuestions

                                        _ ->
                                            []

                                itemsToOptions ( itemQuestionPath, reply ) =
                                    ReplyValue.getItemUuids reply.value
                                        |> List.indexedMap
                                            (\i itemUuid ->
                                                ItemOption itemUuid
                                                    (ProjectQuestionnaire.getItemTitle cfg.questionnaire (String.split "." itemQuestionPath ++ [ itemUuid ]) itemTemplateQuestions
                                                        |> Maybe.withDefault (String.format (gettext "Item %s" appState.locale) [ String.fromInt (i + 1) ])
                                                    )
                                            )
                            in
                            cfg.questionnaire.replies
                                |> Dict.filter (\key _ -> String.endsWith itemQuestionUuid key)
                                |> Dict.toList
                                |> List.concatMap itemsToOptions

                        Nothing ->
                            []

                Nothing ->
                    []

        itemMissing =
            ProjectQuestionnaire.itemSelectQuestionItemMissing
                cfg.questionnaire
                (Question.getListQuestionUuid questionNodeData.question)
                questionNodeData.questionPath

        itemPath =
            ProjectQuestionnaire.itemSelectQuestionItemPath cfg.questionnaire
                (Question.getListQuestionUuid questionNodeData.question)
                questionNodeData.questionPath

        itemSelectExtraProps =
            { itemOptions = itemOptions
            , itemMissing = itemMissing
            , mbItemPath = itemPath
            }
    in
    Lazy.lazy8 viewQuestionItemSelectLazy
        appState.locale
        model.projectQuestionActionPlugins
        questionNodeData
        questionViewFlags
        (itemSelectExtraPropsToString itemSelectExtraProps)
        (getQuestionReplyTime appState mbReply)
        (replyToString mbReply)
        commentCount


type alias ItemSelectExtraProps =
    { itemOptions : List ItemOption
    , itemMissing : Bool
    , mbItemPath : Maybe String
    }


type alias ItemOption =
    { itemUuid : String
    , itemTitle : String
    }


itemSelectExtraPropsToString : ItemSelectExtraProps -> String
itemSelectExtraPropsToString props =
    E.encode 0 (encodeItemSelectExtraProps props)


encodeItemSelectExtraProps : ItemSelectExtraProps -> E.Value
encodeItemSelectExtraProps props =
    E.object
        [ ( "itemOptions", E.list encodeItemOption props.itemOptions )
        , ( "itemMissing", E.bool props.itemMissing )
        , ( "itemPath", E.maybe E.string props.mbItemPath )
        ]


encodeItemOption : ItemOption -> E.Value
encodeItemOption itemOption =
    E.object
        [ ( "itemUuid", E.string itemOption.itemUuid )
        , ( "itemTitle", E.string itemOption.itemTitle )
        ]


itemSelectExtraPropsFromString : String -> ItemSelectExtraProps
itemSelectExtraPropsFromString str =
    D.decodeString itemSelectExtraPropsDecoder str
        |> Result.withDefault (ItemSelectExtraProps [] False Nothing)


itemSelectExtraPropsDecoder : Decoder ItemSelectExtraProps
itemSelectExtraPropsDecoder =
    D.succeed ItemSelectExtraProps
        |> D.required "itemOptions" (D.list itemOptionDecoder)
        |> D.required "itemMissing" D.bool
        |> D.required "itemPath" (D.maybe D.string)


itemOptionDecoder : Decoder ItemOption
itemOptionDecoder =
    D.succeed ItemOption
        |> D.required "itemUuid" D.string
        |> D.required "itemTitle" D.string


viewQuestionItemSelectLazy :
    Gettext.Locale
    -> List ( Plugin, ProjectQuestionActionConnector )
    -> QuestionNodeData
    -> Int
    -> String
    -> String
    -> String
    -> Int
    -> Html Msg
viewQuestionItemSelectLazy locale pluginActions questionNodeData questionViewFlags itemSelectExtraProps replyTime replyString commentCount =
    let
        { question, questionPath } =
            questionNodeData

        mbReply =
            replyFromString replyString

        { itemOptions, itemMissing, mbItemPath } =
            itemSelectExtraPropsFromString itemSelectExtraProps

        mbSelectedItem =
            Maybe.map (ReplyValue.getSelectedItemUuid << .value) mbReply

        extraAttrs =
            if QuestionViewFlags.isReadOnly questionViewFlags then
                [ disabled True ]

            else
                [ onChange (SetReply questionPath << ItemSelectReply) ]

        mbListQuestionUuid =
            Question.getListQuestionUuid question

        ( items, warning ) =
            case mbListQuestionUuid of
                Just itemQuestionUuid ->
                    let
                        noItemsWarning =
                            if List.isEmpty itemOptions && not itemMissing then
                                Flash.warningHtml
                                    (div [ class "ms-2" ]
                                        [ text (gettext "There are no items to select from yet." locale)
                                        , a
                                            [ onClick (ScrollToQuestion itemQuestionUuid)
                                            , class "ms-1"
                                            ]
                                            [ text (gettext "Create them now." locale)
                                            ]
                                        ]
                                    )

                            else
                                Html.nothing
                    in
                    ( itemOptions
                    , noItemsWarning
                    )

                Nothing ->
                    ( []
                    , Flash.warning (gettext "This question does not have any configured list options to select from." locale)
                    )

        itemToOption { itemUuid, itemTitle } =
            option [ value itemUuid, selected (Just itemUuid == mbSelectedItem) ]
                [ text itemTitle ]

        options =
            List.map itemToOption items

        optionsWithSelect =
            if Maybe.isJust mbSelectedItem && not itemMissing then
                options

            else
                itemToOption { itemUuid = "", itemTitle = gettext "- select -" locale } :: options

        itemLink =
            case mbItemPath of
                Just itemPath ->
                    div [ class "questionnaireContent__itemSelectLink" ]
                        [ a [ onClick (ScrollToPath itemPath) ]
                            [ text (gettext "Go to item" locale)
                            , fa "fas fa-arrow-right ms-1"
                            ]
                        ]

                Nothing ->
                    Html.nothing

        missingItemWarning =
            if itemMissing then
                Flash.warning (gettext "The selected item was deleted." locale)

            else
                Html.nothing

        isAnswered =
            Maybe.isJust mbSelectedItem
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__value questionnaireContent__itemSelect" ]
            [ select (class "form-control" :: extraAttrs) optionsWithSelect
            , itemLink
            , warning
            , missingItemWarning
            ]
        , viewQuestionClearReply
            { isAnswered = isAnswered
            , locale = locale
            , questionPath = questionPath
            , readonly = QuestionViewFlags.isReadOnly questionViewFlags
            }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        ]



-- QUESTION FILE


viewQuestionFile : AppState -> Model -> ViewConfig -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionFile appState model cfg questionViewFlags questionNodeData mbReply commentCount =
    let
        fileString =
            mbReply
                |> Maybe.andThen (ReplyValue.getFileUuid << .value)
                |> Maybe.andThen (ProjectQuestionnaire.getFile cfg.questionnaire)
                |> Maybe.unwrap "" (E.encode 0 << ProjectFileSimple.encode)
    in
    Lazy.lazy8 viewQuestionFileLazy
        appState.locale
        model.projectQuestionActionPlugins
        questionNodeData
        questionViewFlags
        (getQuestionReplyTime appState mbReply)
        (replyToString mbReply)
        fileString
        commentCount


viewQuestionFileLazy : Gettext.Locale -> List ( Plugin, ProjectQuestionActionConnector ) -> QuestionNodeData -> Int -> String -> String -> String -> Int -> Html Msg
viewQuestionFileLazy locale pluginActions questionNodeData questionViewFlags replyTime replyString fileString commentCount =
    let
        { question, questionPath } =
            questionNodeData

        mbReply =
            replyFromString replyString

        ( questionContent, clearReplyVisible ) =
            case Maybe.map (ReplyValue.getFileUuid << .value) mbReply of
                Just _ ->
                    let
                        mbFile =
                            fileString
                                |> String.toMaybe
                                |> Maybe.andThen (Result.toMaybe << D.decodeString ProjectFileSimple.decoder)
                    in
                    case mbFile of
                        Just file ->
                            ( div [ class "questionnaireContent__file" ]
                                [ fa ("me-2 " ++ FileIcon.getFileIcon file.fileName file.contentType)
                                , a
                                    [ onClick (DownloadFile file.uuid)
                                    , class "text-truncate"
                                    ]
                                    [ text file.fileName ]
                                , span [ class "text-muted ms-2 text-nowrap" ]
                                    [ text ("(" ++ (ByteUnits.toReadable file.fileSize ++ ")")) ]
                                , Html.viewIf (not (QuestionViewFlags.isReadOnly questionViewFlags)) <|
                                    div [ class "d-flex justify-content-end flex-grow-1" ]
                                        [ a
                                            (onClick (DeleteFile file.uuid file.fileName questionPath)
                                                :: dataCy "file-delete"
                                                :: class "btn-link text-danger ms-2 d-block"
                                                :: tooltip (gettext "Delete" locale)
                                            )
                                            [ faDelete ]
                                        ]
                                ]
                            , False
                            )

                        Nothing ->
                            ( Flash.warning (gettext "The file was deleted." locale)
                            , True
                            )

                Nothing ->
                    let
                        fileConfig =
                            { fileTypes = Question.getFileTypes question
                            , maxSize = Question.getMaxSize question
                            }
                    in
                    ( div []
                        [ button
                            [ class "btn btn-outline-primary"
                            , onClick (OpenFileUpload questionPath fileConfig)
                            , disabled (QuestionViewFlags.isReadOnly questionViewFlags)
                            , dataCy "file-upload"
                            ]
                            [ text (gettext "Upload file" locale) ]
                        ]
                    , False
                    )

        isAnswered =
            Maybe.isJust mbReply
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__value questionnaireContent__integrationQuestion" ] [ questionContent ]
        , Html.viewIf clearReplyVisible <|
            viewQuestionClearReply
                { isAnswered = Maybe.isJust mbReply
                , locale = locale
                , questionPath = questionPath
                , readonly = QuestionViewFlags.isReadOnly questionViewFlags
                }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        ]



-- QUESTION INTEGRATION


viewQuestionIntegration : AppState -> Model -> Int -> QuestionNodeData -> Maybe Reply -> Int -> Html Msg
viewQuestionIntegration appState model questionViewFlags questionNodeData mbReply commentCount =
    let
        mbIntegration =
            case questionNodeData.questionSpecificData of
                IntegrationQuestionSpecificNodeData data ->
                    data.integration

                _ ->
                    Nothing
    in
    case mbIntegration of
        Just (Integration.ApiIntegration _) ->
            let
                typeHints =
                    case model.typeHints of
                        Just { path } ->
                            if path == questionNodeData.questionPath then
                                model.typeHints

                            else
                                model.typeHintsNothing

                        Nothing ->
                            model.typeHintsNothing
            in
            Lazy.lazy8 viewQuestionIntegrationApiLazy
                appState.locale
                model.projectQuestionActionPlugins
                questionNodeData
                questionViewFlags
                typeHints
                (getQuestionReplyTime appState mbReply)
                (replyToString mbReply)
                commentCount

        Just (Integration.PluginIntegration pluginIntegrationData) ->
            let
                pluginAndConnector =
                    AppState.getPluginsByConnector appState .knowledgeModelIntegrations
                        |> List.find (\( p, c ) -> Uuid.toString p.uuid == pluginIntegrationData.pluginUuid && c.integrationId == pluginIntegrationData.pluginIntegrationId)
            in
            case pluginAndConnector of
                Just ( plugin, connector ) ->
                    let
                        pluginDataString =
                            { integrationSettings = pluginIntegrationData.pluginIntegrationSettings
                            , pluginElement = connector.questionnaireElement
                            , rendersReply = connector.rendersReply
                            , userSettings = AppState.getPluginUserSettings appState plugin.uuid
                            , settings = AppState.getPluginSettings appState plugin.uuid
                            }
                                |> encodeIntegrationPluginData
                                |> E.encode 0
                    in
                    Lazy.lazy8 viewQuestionIntegrationPluginLazy
                        appState.locale
                        model.projectQuestionActionPlugins
                        questionNodeData
                        questionViewFlags
                        (getQuestionReplyTime appState mbReply)
                        (replyToString mbReply)
                        pluginDataString
                        commentCount

                Nothing ->
                    viewQuestionWrapper
                        { commentCount = commentCount
                        , isAnswered = Maybe.isJust mbReply
                        , locale = appState.locale
                        , pluginActions = model.projectQuestionActionPlugins
                        , questionNodeData = questionNodeData
                        , questionViewFlags = questionViewFlags
                        }
                        [ Flash.error (gettext "Missing plugin." appState.locale) ]

        Nothing ->
            viewQuestionWrapper
                { commentCount = commentCount
                , isAnswered = Maybe.isJust mbReply
                , locale = appState.locale
                , pluginActions = model.projectQuestionActionPlugins
                , questionNodeData = questionNodeData
                , questionViewFlags = questionViewFlags
                }
                [ Flash.warning (gettext "The integration for this question is not configured properly." appState.locale) ]


viewQuestionIntegrationApiLazy :
    Gettext.Locale
    -> List ( Plugin, ProjectQuestionActionConnector )
    -> QuestionNodeData
    -> Int
    -> Maybe TypeHints
    -> String
    -> String
    -> Int
    -> Html Msg
viewQuestionIntegrationApiLazy locale pluginActions questionNodeData questionViewFlags mbTypeHints replyTime replyString commentCount =
    let
        { question, questionPath, questionSpecificData } =
            questionNodeData

        apiIntegrationData =
            case questionSpecificData of
                IntegrationQuestionSpecificNodeData data ->
                    case data.integration of
                        Just (Integration.ApiIntegration apiData) ->
                            Just apiData

                        _ ->
                            Nothing

                _ ->
                    Nothing

        allowCustomReply =
            Maybe.unwrap False .allowCustomReply apiIntegrationData

        mbReply =
            replyFromString replyString

        mbReplyValue =
            Maybe.map .value mbReply

        isReadOnly =
            QuestionViewFlags.isReadOnly questionViewFlags

        extraArgs =
            if isReadOnly then
                [ disabled True ]

            else
                let
                    questionUuid =
                        Question.getUuid question

                    questionValue =
                        Maybe.unwrap "" ReplyValue.getStringReply mbReplyValue

                    requestAllowEmptySearch =
                        Maybe.unwrap False .requestAllowEmptySearch apiIntegrationData
                in
                [ onInput (TypeHintInput questionPath allowCustomReply requestAllowEmptySearch questionUuid << ReplyValue.IntegrationReply << IntegrationReplyType.PlainType)
                , onBlur HideTypeHints
                , onFocus (ShowTypeHints questionPath requestAllowEmptySearch questionUuid questionValue)
                ]

        viewInput currentValue =
            if allowCustomReply then
                input ([ class "form-control", type_ "text", value currentValue ] ++ extraArgs) []

            else
                div [ class "input-group" ]
                    [ span [ class "input-group-text" ]
                        [ faSearch ]
                    , input ([ class "form-control", type_ "text" ] ++ extraArgs) []
                    ]

        questionInput =
            case mbReplyValue of
                Just (IntegrationReply integrationReply) ->
                    case integrationReply of
                        IntegrationReplyType.PlainType plainValue ->
                            viewInput plainValue

                        IntegrationReplyType.IntegrationType value _ ->
                            Markdown.toHtml [ class "form-control questionnaireContent__markdown" ] value

                _ ->
                    viewInput ""

        typeHintsVisible =
            Maybe.unwrap False (.path >> (==) questionPath) mbTypeHints

        viewTypeHints =
            if not isReadOnly && typeHintsVisible then
                viewQuestionIntegrationTypeHints
                    { locale = locale
                    , questionPath = questionPath
                    , typeHints = mbTypeHints
                    }

            else
                Html.nothing

        isAnswered =
            Maybe.isJust mbReply
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = isAnswered
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__value questionnaireContent__integrationQuestion" ]
            [ questionInput
            , viewTypeHints
            ]
        , viewQuestionClearReply
            { isAnswered = isAnswered
            , locale = locale
            , questionPath = questionPath
            , readonly = isReadOnly
            }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        ]


viewQuestionIntegrationPluginLazy :
    Gettext.Locale
    -> List ( Plugin, ProjectQuestionActionConnector )
    -> QuestionNodeData
    -> Int
    -> String
    -> String
    -> String
    -> Int
    -> Html Msg
viewQuestionIntegrationPluginLazy locale pluginActions questionNodeData questionViewFlags replyTime replyString pluginDataString commentCount =
    let
        mbReply =
            replyFromString replyString

        isAnswered =
            Maybe.isJust mbReply

        isReadOnly =
            QuestionViewFlags.isReadOnly questionViewFlags

        mbPluginData =
            D.decodeString decodeIntegrationPluginData pluginDataString
                |> Result.toMaybe

        content =
            case mbPluginData of
                Just pluginData ->
                    if isAnswered && not pluginData.rendersReply then
                        Markdown.toHtml [ class "form-control questionnaireContent__markdown" ]
                            (Maybe.unwrap "" (ReplyValue.getStringReply << .value) mbReply)

                    else
                        let
                            replyValue =
                                Maybe.map .value mbReply
                                    |> Maybe.map (E.encode 0 << ReplyValue.encode)
                                    |> Maybe.withDefault ""
                        in
                        PluginElement.element pluginData.pluginElement
                            [ PluginElement.settingValue pluginData.settings
                            , PluginElement.userSettingsValue pluginData.userSettings
                            , PluginElement.pluginIntegrationSettingsValue pluginData.integrationSettings
                            , PluginElement.integrationReplyValue replyValue
                            , PluginElement.onReplyValueChange (SetPluginReply questionNodeData.questionPath)
                            ]

                Nothing ->
                    Flash.error (gettext "Plugin error." locale)
    in
    viewQuestionWrapper
        { commentCount = commentCount
        , isAnswered = Maybe.isJust (replyFromString replyString)
        , locale = locale
        , pluginActions = pluginActions
        , questionNodeData = questionNodeData
        , questionViewFlags = questionViewFlags
        }
        [ div [ class "questionnaireContent__value" ] [ content ]
        , viewQuestionClearReply
            { isAnswered = isAnswered
            , locale = locale
            , questionPath = questionNodeData.questionPath
            , readonly = isReadOnly
            }
        , viewQuestionAnsweredBy
            { locale = locale
            , mbReply = mbReply
            , question = questionNodeData.question
            , replyTime = replyTime
            , questionViewFlags = questionViewFlags
            }
        ]


type alias IntegrationPluginData =
    { integrationSettings : String
    , pluginElement : PluginElement
    , rendersReply : Bool
    , settings : String
    , userSettings : String
    }


encodeIntegrationPluginData : IntegrationPluginData -> E.Value
encodeIntegrationPluginData data =
    E.object
        [ ( "integrationSettings", E.string data.integrationSettings )
        , ( "pluginElement", PluginElement.encode data.pluginElement )
        , ( "rendersReply", E.bool data.rendersReply )
        , ( "settings", E.string data.settings )
        , ( "userSettings", E.string data.userSettings )
        ]


decodeIntegrationPluginData : D.Decoder IntegrationPluginData
decodeIntegrationPluginData =
    D.succeed IntegrationPluginData
        |> D.required "integrationSettings" D.string
        |> D.required "pluginElement" PluginElement.decoder
        |> D.required "rendersReply" D.bool
        |> D.required "settings" D.string
        |> D.required "userSettings" D.string


type alias ViewQuestionIntegrationTypeHintsProps =
    { locale : Gettext.Locale
    , questionPath : String
    , typeHints : Maybe TypeHints
    }


viewQuestionIntegrationTypeHints : ViewQuestionIntegrationTypeHintsProps -> Html Msg
viewQuestionIntegrationTypeHints props =
    let
        content =
            case Maybe.unwrap ActionResult.Unset .hints props.typeHints of
                ActionResult.Success [] ->
                    div [ class "questionnaireContent__integrationTypeHintsContent questionnaireContent__integrationTypeHintsContent--info" ]
                        [ faInfo
                        , text (gettext "There are no results for your search." props.locale)
                        ]

                ActionResult.Success hints ->
                    let
                        viewQuestionIntegrationTypeHint_ =
                            viewQuestionIntegrationTypeHint
                                { locale = props.locale
                                , questionPath = props.questionPath
                                }
                    in
                    ul [ class "questionnaireContent__integrationTypeHintsContent questionnaireContent__integrationTypeHintsContent--list" ]
                        (List.map viewQuestionIntegrationTypeHint_ hints)

                ActionResult.Loading ->
                    div [ class "questionnaireContent__integrationTypeHintsContent questionnaireContent__integrationTypeHintsContent--loading" ]
                        [ faSpinner
                        , text (gettext "Loading..." props.locale)
                        ]

                ActionResult.Error err ->
                    div [ class "questionnaireContent__integrationTypeHintsContent questionnaireContent__integrationTypeHintsContent--error" ]
                        [ faError
                        , text err
                        ]

                ActionResult.Unset ->
                    Html.nothing
    in
    div [ class "questionnaireContent__integrationTypeHints" ] [ content ]


type alias ViewQuestionIntegrationTypeHintProps =
    { locale : Gettext.Locale
    , questionPath : String
    }


viewQuestionIntegrationTypeHint : ViewQuestionIntegrationTypeHintProps -> TypeHint -> Html Msg
viewQuestionIntegrationTypeHint props typeHint =
    li
        [ class "questionnaireContent__integrationTypeHintsContentItem"
        , onMouseDown (SetReply props.questionPath (IntegrationReply (IntegrationReplyType.IntegrationType typeHint.value typeHint.raw)))
        ]
        [ Markdown.toHtml [ class "questionnaireContent__markdown" ] (Maybe.withDefault typeHint.value typeHint.valueForSelection)
        ]



-- ITEM


viewItemHeaderNode : AppState -> ViewConfig -> Int -> ItemHeaderNodeData -> ( String, Html Msg )
viewItemHeaderNode appState cfg questionViewFlags itemHeaderNodeData =
    let
        questions =
            KnowledgeModel.getQuestionItemTemplateQuestions itemHeaderNodeData.parentQuestionUuid cfg.questionnaire.knowledgeModel

        itemTitle =
            Maybe.withDefault ""
                (ProjectQuestionnaire.getItemTitle cfg.questionnaire (String.split "." itemHeaderNodeData.itemPath) questions)

        mbReply =
            Dict.get itemHeaderNodeData.parentQuestionPath cfg.questionnaire.replies
    in
    ( itemHeaderNodeData.itemPath ++ "-header"
    , Lazy.lazy5 viewItemHeaderNodeLazy
        appState.locale
        itemHeaderNodeData
        questionViewFlags
        itemTitle
        (replyToString mbReply)
    )


viewItemHeaderNodeLazy : Gettext.Locale -> ItemHeaderNodeData -> Int -> String -> String -> Html Msg
viewItemHeaderNodeLazy locale { isCollapsed, itemIndex, itemPath, itemUuid, nestingType, parentQuestionPath } questionViewFlags mbItemTitle replyString =
    let
        ( collapseIcon, collapseEvent, collapseDataCy ) =
            if isCollapsed then
                ( faQuestionnaireItemExpand
                , ExpandPaths [ itemPath ]
                , "item-expand"
                )

            else
                ( faQuestionnaireItemCollapse
                , CollapsePaths [ itemPath ]
                , "item-collapse"
                )

        itemTitle =
            if isCollapsed then
                Maybe.unwrap
                    (i [ class "ms-2" ] [ text (String.format (gettext "Item %s" locale) [ String.fromInt (itemIndex + 1) ]) ])
                    (strong [ class "ms-2 flex-grow-1 text-truncate" ] << List.singleton << text)
                    (String.toMaybe mbItemTitle)

            else
                Html.nothing

        buttons =
            if QuestionViewFlags.isReadOnly questionViewFlags then
                Html.nothing

            else
                let
                    mbReply =
                        replyFromString replyString

                    itemEventData =
                        { path = parentQuestionPath
                        , itemUuids = Maybe.unwrap [] (ReplyValue.getItemUuids << .value) mbReply
                        , itemUuid = itemUuid
                        }

                    deleteButton =
                        a
                            (class "btn-link text-danger"
                                :: onClick (RemoveItemOpen itemEventData)
                                :: dataCy "item-delete"
                                :: tooltip (gettext "Delete" locale)
                            )
                            [ faDelete ]

                    moveUpButton =
                        if itemIndex == 0 then
                            Html.nothing

                        else
                            a
                                (class "btn-link me-2"
                                    :: onClick (MoveItemUp itemEventData)
                                    :: dataCy "item-move-up"
                                    :: tooltip (gettext "Move up" locale)
                                )
                                [ faQuestionnaireItemMoveUp ]

                    moveDownButton =
                        if itemIndex == List.length itemEventData.itemUuids - 1 then
                            Html.nothing

                        else
                            a
                                (class "btn-link me-2"
                                    :: onClick (MoveItemDown itemEventData)
                                    :: dataCy "item-move-down"
                                    :: tooltip (gettext "Move down" locale)
                                )
                                [ faQuestionnaireItemMoveDown ]
                in
                div [ class "d-flex" ]
                    [ moveUpButton
                    , moveDownButton
                    , deleteButton
                    ]
    in
    wrapNestingType nestingType
        [ div
            [ class "questionnaireContent__itemHeader"
            , classList [ ( "questionnaireContent__itemHeader--collapsed", isCollapsed ) ]
            , attribute "data-path" itemPath
            ]
            [ div
                [ class "flex-grow-1 d-flex me-3 cursor-pointer overflow-hidden"
                , onClick collapseEvent
                , dataCy collapseDataCy
                ]
                [ span [ class "text-primary" ] [ collapseIcon ]
                , itemTitle
                ]
            , buttons
            ]
        ]


viewItemFooterNode : AppState -> ItemFooterNodeData -> ( String, Html Msg )
viewItemFooterNode appState itemFooterNodeData =
    ( itemFooterNodeData.itemPath ++ "-footer"
    , Lazy.lazy2 viewItemFooterNodeLazy appState.locale itemFooterNodeData
    )


viewItemFooterNodeLazy : Gettext.Locale -> ItemFooterNodeData -> Html Msg
viewItemFooterNodeLazy locale { itemPath, nestingType } =
    wrapNestingType nestingType
        [ div [ class "questionnaireContent__itemFooter" ]
            [ div [ class "questionnaireContent__itemFooterInner" ]
                [ a [ onClick (CollapsePaths [ itemPath ]) ]
                    [ faQuestionnaireItemCollapse
                    , text (gettext "Collapse" locale)
                    ]
                ]
            ]
        ]


viewItemsEndNode : AppState -> ViewConfig -> Int -> ItemsEndNodeData -> ( String, Html Msg )
viewItemsEndNode appState cfg questionViewFlags itemAddNodeData =
    let
        mbReply =
            Dict.get itemAddNodeData.questionPath cfg.questionnaire.replies
    in
    ( itemAddNodeData.questionPath ++ "-items-end"
    , Lazy.lazy4 viewItemsEndNodeLazy
        appState.locale
        itemAddNodeData
        questionViewFlags
        (replyToString mbReply)
    )


viewItemsEndNodeLazy : Gettext.Locale -> ItemsEndNodeData -> Int -> String -> Html Msg
viewItemsEndNodeLazy locale { nestingType, questionPath } questionViewFlags replyString =
    let
        mbReply =
            replyFromString replyString

        originalItemUuids =
            Maybe.unwrap [] (ReplyValue.getItemUuids << .value) mbReply
    in
    wrapNestingType nestingType
        [ div [ class "questionnaireContent__itemsEnd" ]
            [ viewItemCollapse
                { locale = locale
                , mbReply = mbReply
                , questionPath = questionPath
                }
            , Html.viewIf (not (QuestionViewFlags.isReadOnly questionViewFlags)) <|
                button
                    [ class "btn btn-outline-secondary with-icon"
                    , onClick (AddItem questionPath originalItemUuids)
                    ]
                    [ faAdd
                    , text (gettext "Add" locale)
                    ]
            ]
        ]


type alias ViewItemCollapseProps =
    { locale : Gettext.Locale
    , mbReply : Maybe Reply
    , questionPath : String
    }


viewItemCollapse : ViewItemCollapseProps -> Html Msg
viewItemCollapse props =
    let
        itemUuids =
            Maybe.unwrap [] (ReplyValue.getItemUuids << .value) props.mbReply

        itemPaths =
            List.map (\itemUuid -> props.questionPath ++ "." ++ itemUuid) itemUuids
    in
    Html.viewIf (List.length itemUuids >= 3) <|
        div [ class "mb-3" ]
            [ a [ onClick (ExpandPaths itemPaths), class "with-icon" ]
                [ faQuestionnaireItemExpandAll
                , text (gettext "Expand all" props.locale)
                ]
            , a [ onClick (CollapsePaths itemPaths), class "with-icon ms-3" ]
                [ faQuestionnaireItemCollapseAll, text (gettext "Collapse all" props.locale) ]
            ]



-- UTILS


wrapNestingType : NestingType -> (List (Html msg) -> Html msg)
wrapNestingType nestingType children =
    case nestingType of
        ContentNesting ->
            div [ class "questionnaireContent__nest", dataCy "questionnaire_content" ] children

        FollowUpNesting innerNesting ->
            div [ class "questionnaireContent__nest questionnaireContent__nest--followup" ] [ wrapNestingType innerNesting children ]

        ItemNesting innerNesting ->
            div [ class "questionnaireContent__nest questionnaireContent__nest--item" ] [ wrapNestingType innerNesting children ]


questionnaireMarkdown : List (Html.Attribute msg) -> String -> Html msg
questionnaireMarkdown attrs markdownText =
    Markdown.toHtml (class "questionnaireContent__markdown" :: attrs) markdownText


getQuestionReplyTime : AppState -> Maybe Reply -> String
getQuestionReplyTime appState mbReply =
    case mbReply of
        Just reply ->
            let
                readableTime =
                    TimeUtils.toReadableDateTime appState.timeZone reply.createdAt

                timeDiff =
                    Time.inWordsWithConfig { withAffix = True } (TimeDistance.locale appState.locale) reply.createdAt appState.currentTime
            in
            readableTime ++ "#" ++ timeDiff

        Nothing ->
            ""


questionReplyTimeFromString : String -> Maybe ( String, String )
questionReplyTimeFromString timeString =
    if String.isEmpty timeString then
        Nothing

    else
        case String.split "#" timeString of
            [ readableTime, timeDiff ] ->
                Just ( readableTime, timeDiff )

            _ ->
                Nothing


replyToString : Maybe Reply -> String
replyToString mbReply =
    case mbReply of
        Nothing ->
            ""

        Just reply ->
            E.encode 0 (Reply.encode reply)


replyFromString : String -> Maybe Reply
replyFromString replyString =
    if String.isEmpty replyString then
        Nothing

    else
        D.decodeString Reply.decoder replyString
            |> Result.toMaybe



-- MODALS


viewRemoveItemModal : AppState -> ViewConfig -> Model -> Html Msg
viewRemoveItemModal appState cfg model =
    let
        viewLink ( path, label ) =
            li []
                [ a [ onClick (ScrollToPath path) ]
                    [ text label ]
                ]

        wrapItemLinks links =
            if List.isEmpty links then
                Html.nothing

            else
                p [ class "mt-3" ]
                    [ text (gettext "There are some item select questions using this item:" appState.locale)
                    , ul [] links
                    ]

        items =
            Maybe.map .itemUuid model.removeItem
                |> Maybe.unwrap [] (ProjectQuestionnaire.getItemUsageInItemSelectQuestions cfg.questionnaire)
                |> List.map viewLink
                |> wrapItemLinks

        modalContent =
            [ text (gettext "Are you sure you want to remove this item?" appState.locale)
            , items
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Remove item" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.removeItem)
                |> Modal.confirmConfigAction (gettext "Remove" appState.locale) RemoveItemConfirm
                |> Modal.confirmConfigCancelMsg RemoveItemCancel
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "remove-item"
    in
    Modal.confirm appState modalConfig
