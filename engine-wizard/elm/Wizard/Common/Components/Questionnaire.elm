module Wizard.Common.Components.Questionnaire exposing
    ( ActivePage(..)
    , Config
    , Context
    , FeaturesConfig
    , Model
    , Msg(..)
    , QuestionnaireRenderer
    , TypeHints
    , addComment
    , addEvent
    , addFile
    , assignCommentThread
    , clearReply
    , deleteComment
    , deleteCommentThread
    , editComment
    , init
    , initSimple
    , reopenCommentThread
    , resolveCommentThread
    , setActiveChapterUuid
    , setLabels
    , setPhaseUuid
    , setReply
    , subscriptions
    , update
    , updateWithQuestionnaireData
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Browser.Events
import CharIdentifier
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, a, button, div, h2, h5, i, img, input, label, li, option, p, select, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, href, id, name, placeholder, selected, src, target, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onFocus, onInput, onMouseDown, onMouseOut)
import Html.Events.Extra exposing (onChange)
import Html.Extra as Html
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Decode.Extra as D
import Json.Encode as E
import List.Extra as List
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Regex
import Roman
import Set exposing (Set)
import Shared.Api.QuestionnaireActions as QuestionnaireActionsApi
import Shared.Api.QuestionnaireFiles as QuestionnaireFilesApi
import Shared.Api.QuestionnaireImporters as QuestionnaireImportersApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.TypeHints as TypeHintsApi
import Shared.Auth.Session as Session
import Shared.Common.ByteUnits as ByteUnits
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Copy as Copy
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeel
import Shared.Data.Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Integration exposing (Integration(..))
import Shared.Data.KnowledgeModel.Integration.ApiIntegrationData exposing (ApiIntegrationData)
import Shared.Data.KnowledgeModel.Integration.CommonIntegrationData exposing (CommonIntegrationData)
import Shared.Data.KnowledgeModel.Integration.WidgetIntegrationData exposing (WidgetIntegrationData)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValidation as QuestionValidation
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Shared.Data.QuestionnaireAction exposing (QuestionnaireAction)
import Shared.Data.QuestionnaireDetail.Comment as Comment exposing (Comment)
import Shared.Data.QuestionnaireDetail.CommentThread as CommentThread exposing (CommentThread)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireDetail.Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType exposing (IntegrationReplyType(..))
import Shared.Data.QuestionnaireFileSimple exposing (QuestionnaireFileSimple)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Data.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Shared.Data.QuestionnaireVersion exposing (QuestionnaireVersion)
import Shared.Data.TypeHint exposing (TypeHint)
import Shared.Data.User as User
import Shared.Data.UserInfo as UserInfo
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Shared.Data.WebSockets.QuestionnaireAction.SetQuestionnaireData exposing (SetQuestionnaireData)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, fa, faKeyClass, faSet)
import Shared.Markdown as Markdown
import Shared.RegexPatterns as RegexPatterns
import Shared.Undraw as Undraw
import Shared.Utils exposing (dispatch, flip, getUuidString, listFilterJust, listInsertIf)
import Shared.Utils.ListUtils as ListUtils
import SplitPane
import String
import String.Extra as String
import String.Format as String
import Time
import Time.Distance as Time
import Uuid exposing (Uuid)
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.DatePicker as DatePicker
import Wizard.Common.Components.Questionnaire.DeleteVersionModal as DeleteVersionModal
import Wizard.Common.Components.Questionnaire.FeedbackModal as FeedbackModal
import Wizard.Common.Components.Questionnaire.FileUploadModal as FileUploadModal
import Wizard.Common.Components.Questionnaire.History as History
import Wizard.Common.Components.Questionnaire.NavigationTree as NavigationTree
import Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Common.Components.Questionnaire.RightPanel as RightPanel exposing (RightPanel)
import Wizard.Common.Components.Questionnaire.UserSuggestionDropdown as UserSuggestionDropdown
import Wizard.Common.Components.Questionnaire.VersionModal as VersionModal
import Wizard.Common.ElementScrollTop as ElementScrollTop
import Wizard.Common.Feature as Feature
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.Common.FileIcon as FileIcon
import Wizard.Common.Html exposing (illustratedMessage, resizableTextarea)
import Wizard.Common.Html.Attribute exposing (dataCy, grammarlyAttributes, linkToAttributes, tooltip, tooltipLeft, tooltipRight)
import Wizard.Common.IntegrationWidgetValue exposing (IntegrationWidgetValue)
import Wizard.Common.Integrations as Integrations
import Wizard.Common.LocalStorageData as LocalStorageData exposing (LocalStorageData)
import Wizard.Common.TimeDistance as TimeDistance
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Tag as Tag
import Wizard.Common.View.UserIcon as UserIcon
import Wizard.Ports as Ports
import Wizard.Projects.Common.QuestionnaireTodoGroup as QuestionnaireTodoGroup
import Wizard.Routes as Routes
import Wizard.Routing as Routing



-- MODEL


type alias Model =
    { uuid : Uuid
    , mbCommentThreadUuid : Maybe Uuid
    , activePage : ActivePage
    , rightPanel : RightPanel
    , questionnaire : QuestionnaireQuestionnaire
    , questionnaireEvents : ActionResult (List QuestionnaireEvent)
    , questionnaireVersions : ActionResult (List QuestionnaireVersion)
    , phaseModalOpen : Bool
    , removeItem : Maybe ( String, String )
    , deleteFile : Maybe ( Uuid, String )
    , deletingFile : ActionResult ()
    , typeHints : Maybe TypeHints
    , typeHintsDebounce : Debounce ( List String, String, String )
    , feedbackModalModel : FeedbackModal.Model
    , fileUploadModalModel : FileUploadModal.Model
    , viewSettings : QuestionnaireViewSettings
    , viewSettingsDropdown : Dropdown.State
    , historyModel : History.Model
    , versionModalModel : VersionModal.Model
    , deleteVersionModalModel : DeleteVersionModal.Model
    , commentInputs : Dict String String
    , commentEditInputs : Dict String String
    , commentDeleting : Maybe Uuid
    , commentDeletingListenClicks : Bool
    , commentsViewResolved : Bool
    , commentsViewPrivate : Bool
    , commentDropdownStates : Dict String Dropdown.State
    , splitPane : SplitPane.State
    , navigationTreeModel : NavigationTree.Model
    , questionnaireImportersDropdown : Dropdown.State
    , questionnaireImporters : ActionResult (List QuestionnaireImporter)
    , questionnaireActionsDropdown : Dropdown.State
    , questionnaireActions : ActionResult (List QuestionnaireAction)
    , questionnaireActionResult : Maybe Integrations.ActionResult
    , collapsedItems : Set String
    , recentlyCopied : Bool
    , contentScrollTop : Maybe Int
    , commentThreadsMap : Dict String (ActionResult (List CommentThread))
    , userSuggestionDropdownModels : Dict String UserSuggestionDropdown.Model
    }


type alias TypeHints =
    { path : List String
    , searchValue : String
    , hints : ActionResult (List TypeHint)
    }


type ActivePage
    = PageNone
    | PageChapter String


initSimple : AppState -> QuestionnaireQuestionnaire -> ( Model, Cmd Msg )
initSimple appState questionnaire =
    init appState questionnaire Nothing Nothing


init : AppState -> QuestionnaireQuestionnaire -> Maybe String -> Maybe Uuid -> ( Model, Cmd Msg )
init appState questionnaire mbPath mbCommentThreadUuid =
    let
        mbChapterUuid =
            List.head questionnaire.knowledgeModel.chapterUuids

        activePage =
            Maybe.unwrap PageNone PageChapter mbChapterUuid

        navigationTreeModel =
            Maybe.unwrap NavigationTree.initialModel
                (flip NavigationTree.openChapter NavigationTree.initialModel)
                mbChapterUuid

        defaultModel =
            { uuid = questionnaire.uuid
            , mbCommentThreadUuid = mbCommentThreadUuid
            , activePage = activePage
            , rightPanel = RightPanel.None
            , questionnaire = questionnaire
            , questionnaireEvents = ActionResult.Unset
            , questionnaireVersions = ActionResult.Unset
            , phaseModalOpen = False
            , removeItem = Nothing
            , deleteFile = Nothing
            , deletingFile = ActionResult.Unset
            , typeHints = Nothing
            , typeHintsDebounce = Debounce.init
            , feedbackModalModel = FeedbackModal.init
            , fileUploadModalModel = FileUploadModal.init questionnaire.uuid
            , viewSettings = QuestionnaireViewSettings.default
            , viewSettingsDropdown = Dropdown.initialState
            , historyModel = History.init appState
            , versionModalModel = VersionModal.init
            , deleteVersionModalModel = DeleteVersionModal.init
            , commentInputs = Dict.empty
            , commentEditInputs = Dict.empty
            , commentDeleting = Nothing
            , commentDeletingListenClicks = False
            , commentsViewResolved = False
            , commentsViewPrivate = False
            , commentDropdownStates = Dict.empty
            , splitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.2 (Just ( 0.1, 0.7 )))
            , navigationTreeModel = navigationTreeModel
            , questionnaireImportersDropdown = Dropdown.initialState
            , questionnaireImporters = ActionResult.Unset
            , questionnaireActionsDropdown = Dropdown.initialState
            , questionnaireActions = ActionResult.Unset
            , questionnaireActionResult = Nothing
            , collapsedItems = Set.empty
            , recentlyCopied = False
            , contentScrollTop = Nothing
            , commentThreadsMap = Dict.empty
            , userSuggestionDropdownModels = Dict.empty
            }

        ( model, scrollCmd ) =
            case ( mbPath, mbCommentThreadUuid ) of
                ( Just path, Just _ ) ->
                    ( defaultModel, dispatch (OpenComments True path) )

                ( Just path, Nothing ) ->
                    handleScrollToPath defaultModel False path

                _ ->
                    ( defaultModel, Cmd.none )

        rightPanelCmd =
            case mbCommentThreadUuid of
                Just _ ->
                    Cmd.none

                _ ->
                    Ports.localStorageGet (localStorageRightPanelKey questionnaire.uuid)
    in
    ( model
    , Cmd.batch
        [ scrollCmd
        , Ports.localStorageGet (localStorageCollapsedItemKey questionnaire.uuid)
        , Ports.localStorageGet (localStorageViewResolvedKey questionnaire.uuid)
        , Ports.localStorageGet (localStorageNamedOnlyKey questionnaire.uuid)
        , Ports.localStorageGet localStorageViewSettingsKey
        , rightPanelCmd
        ]
    )


addEvent : QuestionnaireEvent -> Model -> Model
addEvent event model =
    { model | questionnaireEvents = ActionResult.map (\events -> events ++ [ event ]) model.questionnaireEvents }


setActiveChapterUuid : String -> Model -> Model
setActiveChapterUuid uuid model =
    { model
        | activePage = PageChapter uuid
        , navigationTreeModel = NavigationTree.openChapter uuid model.navigationTreeModel
    }


updateWithQuestionnaireData : AppState -> SetQuestionnaireData -> Model -> Model
updateWithQuestionnaireData appState data model =
    let
        updatedQuestionnaire =
            QuestionnaireQuestionnaire.updateWithQuestionnaireData data model.questionnaire

        setNewPanel panel allowed =
            if allowed then
                panel

            else
                RightPanel.None

        rightPanel =
            case model.rightPanel of
                RightPanel.TODOs ->
                    setNewPanel RightPanel.TODOs <|
                        Feature.projectTodos appState updatedQuestionnaire

                RightPanel.VersionHistory ->
                    setNewPanel RightPanel.VersionHistory <|
                        Feature.projectVersionHistory appState updatedQuestionnaire

                RightPanel.CommentsOverview ->
                    setNewPanel RightPanel.CommentsOverview <|
                        Feature.projectCommentAdd appState updatedQuestionnaire

                RightPanel.Comments path ->
                    setNewPanel (RightPanel.Comments path) <|
                        Feature.projectCommentAdd appState updatedQuestionnaire

                _ ->
                    model.rightPanel
    in
    { model
        | questionnaire = updatedQuestionnaire
        , rightPanel = rightPanel
    }


setPhaseUuid : Maybe Uuid -> Model -> Model
setPhaseUuid phaseUuid =
    updateQuestionnaire <| QuestionnaireQuestionnaire.setPhaseUuid phaseUuid


setReply : String -> Reply -> Model -> Model
setReply path reply =
    updateQuestionnaire <| QuestionnaireQuestionnaire.setReply path reply


clearReply : String -> Model -> Model
clearReply path =
    updateQuestionnaire <| QuestionnaireQuestionnaire.clearReplyValue path


setLabels : String -> List String -> Model -> Model
setLabels path value =
    updateQuestionnaire <| QuestionnaireQuestionnaire.setLabels path value


resolveCommentThread : String -> Uuid -> Int -> Model -> Model
resolveCommentThread path threadUuid commentCount model =
    let
        mapCommentThread commentThread =
            { commentThread | resolved = True }
    in
    model
        |> mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))
        |> updateQuestionnaire (QuestionnaireQuestionnaire.addResolvedCommentThreadToCount path threadUuid commentCount)


reopenCommentThread : String -> Uuid -> Int -> Model -> Model
reopenCommentThread path threadUuid commentCount model =
    let
        mapCommentThread commentThread =
            { commentThread | resolved = False }
    in
    model
        |> mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))
        |> updateQuestionnaire (QuestionnaireQuestionnaire.addReopenedCommentThreadToCount path threadUuid commentCount)


deleteCommentThread : String -> Uuid -> Model -> Model
deleteCommentThread path threadUuid model =
    model
        |> mapCommentThreads path (List.filter (\t -> t.uuid /= threadUuid))
        |> updateQuestionnaire (QuestionnaireQuestionnaire.removeCommentThreadFromCount path threadUuid)


assignCommentThread : String -> Uuid -> Maybe UserSuggestion -> Model -> Model
assignCommentThread path threadUuid mbUserSuggestion model =
    let
        mapCommentThread commentThread =
            { commentThread | assignedTo = mbUserSuggestion }
    in
    model
        |> mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))


addComment : String -> Uuid -> Bool -> Comment -> Model -> Model
addComment path threadUuid private comment model =
    let
        threadExists =
            Dict.get path model.commentThreadsMap
                |> Maybe.andThen ActionResult.toMaybe
                |> Maybe.withDefault []
                |> List.any (.uuid >> (==) threadUuid)

        mapCommentThread commentThread =
            { commentThread | comments = commentThread.comments ++ [ comment ] }

        questionnaireWithThread =
            if threadExists then
                model

            else
                addCommentThread path threadUuid private comment model
    in
    questionnaireWithThread
        |> mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))
        |> updateQuestionnaire (QuestionnaireQuestionnaire.addCommentCount path threadUuid)


addCommentThread : String -> Uuid -> Bool -> Comment -> Model -> Model
addCommentThread path threadUuid private comment model =
    let
        commentThread =
            { uuid = threadUuid
            , resolved = False
            , comments = []
            , private = private
            , createdAt = comment.createdAt
            , createdBy = comment.createdBy
            , assignedTo = Nothing
            }

        commentThreads =
            Dict.get path model.commentThreadsMap
                |> Maybe.withDefault (Success [])

        mapAddCommentThread originalCommentThreads =
            originalCommentThreads ++ [ commentThread ]
    in
    { model | commentThreadsMap = Dict.insert path (ActionResult.map mapAddCommentThread commentThreads) model.commentThreadsMap }


editComment : String -> Uuid -> Uuid -> Time.Posix -> String -> Model -> Model
editComment path threadUuid commentUuid updatedAt newText =
    let
        mapComment comment =
            if comment.uuid == commentUuid then
                { comment | text = newText, updatedAt = updatedAt }

            else
                comment

        mapCommentThread commentThread =
            { commentThread | comments = List.map mapComment commentThread.comments }
    in
    mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))


deleteComment : String -> Uuid -> Uuid -> Model -> Model
deleteComment path threadUuid commentUuid model =
    let
        mapCommentThread commentThread =
            { commentThread | comments = List.filter (\c -> c.uuid /= commentUuid) commentThread.comments }
    in
    model
        |> mapCommentThreads path (List.map (wrapMapCommentThread threadUuid mapCommentThread))
        |> updateQuestionnaire (QuestionnaireQuestionnaire.subCommentCount path threadUuid)


mapCommentThreads : String -> (List CommentThread -> List CommentThread) -> Model -> Model
mapCommentThreads path map model =
    let
        mbCommentThreads =
            Dict.get path model.commentThreadsMap
                |> Maybe.map (ActionResult.map map)
    in
    case mbCommentThreads of
        Just commentThreads ->
            { model | commentThreadsMap = Dict.insert path commentThreads model.commentThreadsMap }

        Nothing ->
            model


wrapMapCommentThread : Uuid -> (CommentThread -> CommentThread) -> CommentThread -> CommentThread
wrapMapCommentThread threadUuid mapCommentThread commentThread =
    if commentThread.uuid == threadUuid then
        mapCommentThread commentThread

    else
        commentThread


updateQuestionnaire : (QuestionnaireQuestionnaire -> QuestionnaireQuestionnaire) -> Model -> Model
updateQuestionnaire fn model =
    { model | questionnaire = fn model.questionnaire }


isQuestionDesirable : Model -> Question -> Bool
isQuestionDesirable model =
    Question.isDesirable model.questionnaire.knowledgeModel.phaseUuids
        (Uuid.toString (Maybe.withDefault Uuid.nil model.questionnaire.phaseUuid))


addFile : QuestionnaireFileSimple -> Model -> Model
addFile file model =
    { model | questionnaire = QuestionnaireQuestionnaire.addFile file model.questionnaire }


type alias Config msg =
    { features : FeaturesConfig
    , renderer : QuestionnaireRenderer Msg
    , wrapMsg : Msg -> msg
    , previewQuestionnaireEventMsg : Maybe (Uuid -> msg)
    , revertQuestionnaireMsg : Maybe (QuestionnaireEvent -> msg)
    , isKmEditor : Bool
    }


type alias FeaturesConfig =
    { feedbackEnabled : Bool
    , todosEnabled : Bool
    , commentsEnabled : Bool
    , readonly : Bool
    , toolbarEnabled : Bool
    , questionLinksEnabled : Bool
    }


type alias QuestionnaireRenderer msg =
    { renderQuestionLabel : Question -> Html msg
    , renderQuestionDescription : QuestionnaireViewSettings -> Question -> Html msg
    , getQuestionExtraClass : Question -> Maybe String
    , renderAnswerLabel : Answer -> Html msg
    , renderAnswerBadges : Bool -> Answer -> Html msg
    , renderAnswerAdvice : Answer -> Html msg
    , renderChoiceLabel : Choice -> Html msg
    }


type alias Context =
    { events : List Event
    }


type QuestionViewState
    = Default
    | Answered
    | Desirable


localStorageViewSettingsKey : String
localStorageViewSettingsKey =
    "project-view-settings"


localStorageViewSettingsDecoder : Decoder (LocalStorageData QuestionnaireViewSettings)
localStorageViewSettingsDecoder =
    LocalStorageData.decoder QuestionnaireViewSettings.decoder


localStorageViewSettingsEncode : QuestionnaireViewSettings -> E.Value
localStorageViewSettingsEncode qvs =
    LocalStorageData.encode QuestionnaireViewSettings.encode
        { key = localStorageViewSettingsKey
        , value = qvs
        }


localStorageCollapsedItemKey : Uuid -> String
localStorageCollapsedItemKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-items"


localStorageCollapsedItemsDecoder : Decoder (LocalStorageData (Set String))
localStorageCollapsedItemsDecoder =
    LocalStorageData.decoder (D.set D.string)


localStorageCollapsedItemsEncode : LocalStorageData (Set String) -> E.Value
localStorageCollapsedItemsEncode =
    LocalStorageData.encode (E.list E.string << Set.toList)


localStorageRightPanelKey : Uuid -> String
localStorageRightPanelKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-right-panel"


localStorageRightPanelDecoder : Decoder (LocalStorageData RightPanel)
localStorageRightPanelDecoder =
    LocalStorageData.decoder RightPanel.decoder


localStorageViewResolvedKey : Uuid -> String
localStorageViewResolvedKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-view-resolved"


localStorageViewResolvedDecoder : Decoder (LocalStorageData Bool)
localStorageViewResolvedDecoder =
    LocalStorageData.decoder D.bool


localStorageNamedOnlyKey : Uuid -> String
localStorageNamedOnlyKey uuid =
    "project-" ++ Uuid.toString uuid ++ "-named-only"


localStorageNamedOnlyDecoder : Decoder (LocalStorageData Bool)
localStorageNamedOnlyDecoder =
    LocalStorageData.decoder D.bool


contentElementSelector : String
contentElementSelector =
    ".questionnaire__content"



-- UPDATE


type Msg
    = SetActivePage ActivePage
    | SetRightPanel RightPanel
    | SetFullscreen Bool
    | ScrollToPath String
    | UpdateContentScroll
    | GotContentScroll E.Value
    | ShowTypeHints (List String) Bool String String
    | HideTypeHints
    | TypeHintInput (List String) Bool Reply
    | TypeHintDebounceMsg Debounce.Msg
    | TypeHintsLoaded (List String) String (Result ApiError (List TypeHint))
    | FeedbackModalMsg FeedbackModal.Msg
    | FileUploadModalMsg FileUploadModal.Msg
    | PhaseModalUpdate Bool (Maybe Uuid)
    | SetReply String Reply
    | SetFile String QuestionnaireFileSimple
    | ClearReply String
    | AddItem String (List String)
    | RemoveItem String String
    | RemoveItemConfirm
    | RemoveItemCancel
    | DeleteFile Uuid String
    | DeleteFileConfirm
    | DeleteFileCompleted String (Result ApiError ())
    | DeleteFileCancel
    | DownloadFile Uuid
    | FileDownloaderMsg FileDownloader.Msg
    | MoveItemUp String String
    | MoveItemDown String String
    | OpenIntegrationWidget String String
    | GotIntegrationWidgetValue (Result D.Error IntegrationWidgetValue)
    | SetLabels String (List String)
    | ViewSettingsDropdownMsg Dropdown.State
    | SetViewSettings QuestionnaireViewSettings
    | GetQuestionnaireEventsCompleted (Result ApiError (List QuestionnaireEvent))
    | GetQuestionnaireVersionsCompleted (Result ApiError (List QuestionnaireVersion))
    | HistoryMsg History.Msg
    | VersionModalMsg VersionModal.Msg
    | DeleteVersionModalMsg DeleteVersionModal.Msg
    | CreateNamedVersion Uuid
    | RenameVersion QuestionnaireVersion
    | DeleteVersion QuestionnaireVersion
    | AddQuestionnaireVersion QuestionnaireVersion
    | UpdateQuestionnaireVersion QuestionnaireVersion
    | DeleteQuestionnaireVersion QuestionnaireVersion
    | OpenComments Bool String
    | CommentInput String (Maybe Uuid) String
    | CommentSubmit String (Maybe Uuid) String Bool
    | CommentDelete (Maybe Uuid)
    | CommentDeleteListenClicks
    | CommentDeleteSubmit String Uuid Uuid Bool
    | CommentEditInput Uuid String
    | CommentEditCancel Uuid
    | CommentEditSubmit String Uuid Uuid String Bool
    | CommentThreadDelete String CommentThread
    | CommentThreadResolve String CommentThread
    | CommentThreadReopen String CommentThread
    | CommentThreadAssign String CommentThread (Maybe UserSuggestion)
    | CommentsViewResolved Bool
    | CommentsViewPrivate Bool
    | CommentDropdownMsg String Dropdown.State
    | SplitPaneMsg SplitPane.Msg
    | NavigationTreeMsg NavigationTree.Msg
    | ImportersDropdownMsg Dropdown.State
    | ActionsDropdownMsg Dropdown.State
    | GotActionResult (Result D.Error Integrations.ActionResult)
    | CloseActionResult
    | OpenAction QuestionnaireAction
    | CollapseItem String
    | ExpandItem String
    | CollapseItems (List String)
    | ExpandItems (List String)
    | GotLocalStorageData E.Value
    | CopyLinkToQuestion (List String)
    | ClearRecentlyCopied
    | GetCommentThreadsCompleted String (Result ApiError (Dict String (List CommentThread)))
    | GetQuestionnaireImportersComplete (Result ApiError (List QuestionnaireImporter))
    | GetQuestionnaireActionsComplete (Result ApiError (List QuestionnaireAction))
    | UserSuggestionDropdownMsg String Uuid Bool UserSuggestionDropdown.Msg


update : Msg -> (Msg -> msg) -> Maybe (Bool -> msg) -> AppState -> Context -> Model -> ( Seed, Model, Cmd msg )
update msg wrapMsg mbSetFullscreenMsg appState ctx model =
    let
        withSeed ( newModel, cmd ) =
            ( appState.seed, newModel, Cmd.map wrapMsg cmd )

        wrap newModel =
            ( appState.seed, newModel, Cmd.none )

        updateCollapsedItems newCollapsedItems =
            withSeed
                ( { model | collapsedItems = newCollapsedItems }
                , localStorageCollapsedItemsCmd model.uuid newCollapsedItems
                )

        loadComments path =
            QuestionnairesApi.getQuestionnaireComments model.uuid path appState (GetCommentThreadsCompleted path)
    in
    case msg of
        SetActivePage activePage ->
            let
                newNavigationTreeModel =
                    case activePage of
                        PageChapter chapterUuid ->
                            NavigationTree.openChapter chapterUuid model.navigationTreeModel

                        _ ->
                            model.navigationTreeModel
            in
            withSeed <|
                ( { model
                    | activePage = activePage
                    , navigationTreeModel = newNavigationTreeModel
                    , contentScrollTop = Nothing
                  }
                , Ports.scrollToTop contentElementSelector
                )

        SetRightPanel rightPanel ->
            let
                panelCmd =
                    case rightPanel of
                        RightPanel.VersionHistory ->
                            Cmd.batch
                                [ QuestionnairesApi.getQuestionnaireEvents model.uuid appState GetQuestionnaireEventsCompleted
                                , QuestionnairesApi.getQuestionnaireVersions model.uuid appState GetQuestionnaireVersionsCompleted
                                ]

                        RightPanel.Comments path ->
                            loadComments path

                        _ ->
                            Cmd.none

                newModel =
                    { model | rightPanel = rightPanel, questionnaireEvents = ActionResult.Loading }
            in
            withSeed
                ( newModel
                , Cmd.batch
                    [ panelCmd
                    , localStorageRightPanelCmd newModel
                    ]
                )

        SetFullscreen fullscreen ->
            case mbSetFullscreenMsg of
                Just setFullscreenMsg ->
                    ( appState.seed, model, dispatch (setFullscreenMsg fullscreen) )

                Nothing ->
                    ( appState.seed, model, Cmd.none )

        ScrollToPath path ->
            withSeed <| handleScrollToPath model False path

        UpdateContentScroll ->
            let
                subscribeCmd =
                    Ports.subscribeScrollTop contentElementSelector
            in
            case model.contentScrollTop of
                Just value ->
                    withSeed
                        ( model
                        , Cmd.batch
                            [ subscribeCmd
                            , Ports.setScrollTop
                                { selector = contentElementSelector
                                , scrollTop = value
                                }
                            ]
                        )

                Nothing ->
                    withSeed ( model, subscribeCmd )

        GotContentScroll value ->
            case decodeValue ElementScrollTop.decoder value of
                Ok elementScrollTop ->
                    if elementScrollTop.selector == contentElementSelector then
                        wrap { model | contentScrollTop = Just elementScrollTop.scrollTop }

                    else
                        wrap model

                Err _ ->
                    wrap model

        ShowTypeHints path emptySearch questionUuid value ->
            withSeed <| handleShowTypeHints appState ctx model path emptySearch questionUuid value

        HideTypeHints ->
            wrap { model | typeHints = Nothing }

        TypeHintInput path emptySearch value ->
            withSeed <| handleTypeHintsInput model path emptySearch value

        TypeHintDebounceMsg debounceMsg ->
            withSeed <| handleTypeHintDebounceMsg appState ctx model debounceMsg

        TypeHintsLoaded path value result ->
            wrap <| handleTypeHintsLoaded appState model path value result

        FeedbackModalMsg feedbackModalMsg ->
            withSeed <| handleFeedbackModalMsg appState model feedbackModalMsg

        FileUploadModalMsg fileUploadModalMsg ->
            withSeed <| handleFileUploadModalMsg appState model fileUploadModalMsg

        PhaseModalUpdate open mbPhaseUuid ->
            let
                modelWithPhase =
                    if Maybe.isJust mbPhaseUuid then
                        setPhaseUuid mbPhaseUuid model

                    else
                        model
            in
            wrap { modelWithPhase | phaseModalOpen = open }

        SetReply path replyValue ->
            wrap <| setReply path replyValue model

        SetFile path file ->
            let
                modelWithFile =
                    updateQuestionnaire (QuestionnaireQuestionnaire.addFile file) model

                reply =
                    createReply appState (FileReply file.uuid)
            in
            withSeed <| ( modelWithFile, dispatch (SetReply path reply) )

        ClearReply path ->
            wrap <| clearReply path model

        AddItem path originalItems ->
            handleAddItem appState wrapMsg model path originalItems

        RemoveItem path itemUuid ->
            wrap <| { model | removeItem = Just ( path, itemUuid ) }

        RemoveItemConfirm ->
            case model.removeItem of
                Just ( path, itemUuid ) ->
                    let
                        itemUuids =
                            Dict.get path model.questionnaire.replies
                                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)

                        newItemUuids =
                            List.filter ((/=) itemUuid) itemUuids

                        replyValue =
                            createReply appState (ItemListReply newItemUuids)

                        setReplyMsg =
                            SetReply path replyValue
                    in
                    withSeed ( { model | removeItem = Nothing }, dispatch setReplyMsg )

                Nothing ->
                    wrap model

        RemoveItemCancel ->
            wrap <| { model | removeItem = Nothing }

        DeleteFile fileUuid path ->
            wrap <|
                { model
                    | deleteFile = Just ( fileUuid, path )
                    , deletingFile = ActionResult.Unset
                }

        DeleteFileConfirm ->
            case model.deleteFile of
                Just ( fileUuid, path ) ->
                    let
                        deleteFileCmd =
                            QuestionnaireFilesApi.deleteFile model.uuid fileUuid appState (DeleteFileCompleted path)

                        modelWithDeletingFile =
                            { model | deletingFile = ActionResult.Loading }
                    in
                    withSeed ( modelWithDeletingFile, deleteFileCmd )

                Nothing ->
                    wrap model

        DeleteFileCompleted path result ->
            case result of
                Ok _ ->
                    withSeed
                        ( { model | deleteFile = Nothing }
                        , dispatch (ClearReply path)
                        )

                Err err ->
                    wrap { model | deletingFile = ApiError.toActionResult appState (gettext "Unable to delete file." appState.locale) err }

        DeleteFileCancel ->
            wrap <| { model | deleteFile = Nothing }

        DownloadFile uuid ->
            withSeed ( model, Cmd.map FileDownloaderMsg (FileDownloader.fetchFile appState (QuestionnaireFilesApi.fileUrl model.uuid uuid appState)) )

        FileDownloaderMsg fileDownloaderMsg ->
            withSeed ( model, Cmd.map FileDownloaderMsg (FileDownloader.update fileDownloaderMsg) )

        MoveItemUp path itemUuid ->
            let
                itemUuids =
                    Dict.get path model.questionnaire.replies
                        |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)

                index =
                    Maybe.withDefault -1 (List.elemIndex itemUuid itemUuids)

                newItemUuids =
                    if index > 0 then
                        List.swapAt index (index - 1) itemUuids

                    else
                        itemUuids

                replyValue =
                    createReply appState (ItemListReply newItemUuids)

                setReplyMsg =
                    SetReply path replyValue
            in
            withSeed ( { model | removeItem = Nothing }, dispatch setReplyMsg )

        MoveItemDown path itemUuid ->
            let
                itemUuids =
                    Dict.get path model.questionnaire.replies
                        |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)

                itemCount =
                    List.length itemUuids

                index =
                    Maybe.withDefault (List.length itemUuids) (List.elemIndex itemUuid itemUuids)

                newItemUuids =
                    if index < itemCount - 1 then
                        List.swapAt index (index + 1) itemUuids

                    else
                        itemUuids

                replyValue =
                    createReply appState (ItemListReply newItemUuids)

                setReplyMsg =
                    SetReply path replyValue
            in
            withSeed ( { model | removeItem = Nothing }, dispatch setReplyMsg )

        OpenIntegrationWidget path requestUrl ->
            withSeed
                ( model
                , Integrations.openIntegrationWidget
                    { url = requestUrl
                    , theme = Maybe.withDefault (LookAndFeel.getTheme appState.config.lookAndFeel) appState.theme
                    , data = { path = path }
                    }
                )

        GotIntegrationWidgetValue result ->
            case result of
                Ok value ->
                    let
                        setReplyMsg =
                            SetReply value.path <|
                                createReply appState <|
                                    IntegrationReply <|
                                        IntegrationType (Just value.id) value.value
                    in
                    withSeed ( model, dispatch setReplyMsg )

                Err _ ->
                    wrap model

        SetLabels path value ->
            wrap <| setLabels path value model

        ViewSettingsDropdownMsg state ->
            wrap { model | viewSettingsDropdown = state }

        SetViewSettings viewSettings ->
            withSeed
                ( { model | viewSettings = viewSettings }
                , Ports.localStorageSet (localStorageViewSettingsEncode viewSettings)
                )

        GetQuestionnaireEventsCompleted result ->
            wrap <|
                case result of
                    Ok questionnaireEvents ->
                        { model | questionnaireEvents = Success questionnaireEvents }

                    Err _ ->
                        { model | questionnaireEvents = Error (gettext "Unable to get version history." appState.locale) }

        GetQuestionnaireVersionsCompleted result ->
            wrap <|
                case result of
                    Ok questionnaireVersions ->
                        { model | questionnaireVersions = Success questionnaireVersions }

                    Err _ ->
                        { model | questionnaireVersions = Error (gettext "Unable to get version history." appState.locale) }

        HistoryMsg historyMsg ->
            let
                newModel =
                    { model | historyModel = History.update historyMsg model.historyModel }

                cmd =
                    case historyMsg of
                        History.SetNamedOnly _ ->
                            localStorageNamedOnlyCmd newModel

                        _ ->
                            Cmd.none
            in
            withSeed ( newModel, cmd )

        VersionModalMsg versionModalMsg ->
            let
                cfg =
                    { wrapMsg = VersionModalMsg
                    , questionnaireUuid = model.questionnaire.uuid
                    , addVersionCmd = dispatch << AddQuestionnaireVersion
                    , renameVersionCmd = dispatch << UpdateQuestionnaireVersion
                    }

                ( versionModalModel, cmd ) =
                    VersionModal.update cfg appState versionModalMsg model.versionModalModel

                newModel =
                    { model | versionModalModel = versionModalModel }
            in
            withSeed ( newModel, cmd )

        DeleteVersionModalMsg modalMsg ->
            let
                cfg =
                    { wrapMsg = DeleteVersionModalMsg
                    , questionnaireUuid = model.questionnaire.uuid
                    , deleteVersionCmd = dispatch << DeleteQuestionnaireVersion
                    }

                ( deleteVersionModalModel, cmd ) =
                    DeleteVersionModal.update cfg appState modalMsg model.deleteVersionModalModel

                newModel =
                    { model | deleteVersionModalModel = deleteVersionModalModel }
            in
            withSeed ( newModel, cmd )

        CreateNamedVersion eventUuid ->
            wrap { model | versionModalModel = VersionModal.setEventUuid eventUuid model.versionModalModel }

        RenameVersion questionnaireVersion ->
            wrap { model | versionModalModel = VersionModal.setVersion questionnaireVersion model.versionModalModel }

        DeleteVersion questionnaireVersion ->
            wrap { model | deleteVersionModalModel = DeleteVersionModal.setVersion questionnaireVersion model.deleteVersionModalModel }

        AddQuestionnaireVersion questionnaireVersion ->
            let
                questionnaireVersions =
                    ActionResult.map ((::) questionnaireVersion) model.questionnaireVersions
            in
            wrap { model | questionnaireVersions = questionnaireVersions }

        UpdateQuestionnaireVersion questionnaireVersion ->
            let
                updateVersion version =
                    if version.uuid == questionnaireVersion.uuid then
                        { version | name = questionnaireVersion.name, description = questionnaireVersion.description }

                    else
                        version

                questionnaireVersions =
                    ActionResult.map (List.map updateVersion) model.questionnaireVersions
            in
            wrap { model | questionnaireVersions = questionnaireVersions }

        DeleteQuestionnaireVersion questionnaireVersion ->
            let
                questionnaireVersions =
                    ActionResult.map (List.filter (not << (==) questionnaireVersion.uuid << .uuid)) model.questionnaireVersions
            in
            wrap
                { model | questionnaireVersions = questionnaireVersions }

        OpenComments immediate path ->
            let
                ( newModel, cmd ) =
                    handleScrollToPath { model | rightPanel = RightPanel.Comments path } immediate path
            in
            withSeed
                ( newModel
                , Cmd.batch
                    [ cmd
                    , loadComments path
                    , localStorageRightPanelCmd newModel
                    ]
                )

        CommentInput path mbThreadUuid value ->
            let
                key =
                    path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid

                commentInputs =
                    Dict.insert key value model.commentInputs
            in
            wrap { model | commentInputs = commentInputs }

        CommentSubmit path mbThreadUuid _ _ ->
            let
                key =
                    path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid

                commentInputs =
                    Dict.remove key model.commentInputs
            in
            wrap { model | commentInputs = commentInputs }

        CommentEditInput commentUuid value ->
            let
                commentEditInputs =
                    Dict.insert (Uuid.toString commentUuid) value model.commentEditInputs
            in
            wrap { model | commentEditInputs = commentEditInputs }

        CommentEditCancel commentUuid ->
            let
                commentEditInputs =
                    Dict.remove (Uuid.toString commentUuid) model.commentEditInputs
            in
            wrap { model | commentEditInputs = commentEditInputs }

        CommentEditSubmit _ _ commentUuid _ _ ->
            let
                commentEditInputs =
                    Dict.remove (Uuid.toString commentUuid) model.commentEditInputs
            in
            wrap { model | commentEditInputs = commentEditInputs }

        CommentDelete mbCommentUuid ->
            wrap { model | commentDeleting = mbCommentUuid, commentDeletingListenClicks = False }

        CommentDeleteListenClicks ->
            wrap { model | commentDeletingListenClicks = True }

        CommentsViewResolved value ->
            let
                newModel =
                    { model | commentsViewResolved = value }
            in
            withSeed ( newModel, localStorageViewResolvedCmd newModel )

        CommentsViewPrivate value ->
            wrap { model | commentsViewPrivate = value }

        CommentDropdownMsg commentUuid state ->
            wrap { model | commentDropdownStates = Dict.insert commentUuid state model.commentDropdownStates }

        SplitPaneMsg splitPaneMsg ->
            wrap { model | splitPane = SplitPane.update splitPaneMsg model.splitPane }

        NavigationTreeMsg navigationTreeMsg ->
            wrap { model | navigationTreeModel = NavigationTree.update navigationTreeMsg model.navigationTreeModel }

        ImportersDropdownMsg state ->
            let
                ( questionnaireImporters, cmd ) =
                    if ActionResult.isUnset model.questionnaireImporters then
                        ( Loading
                        , QuestionnaireImportersApi.getQuestionnaireImportersFor model.uuid appState GetQuestionnaireImportersComplete
                        )

                    else
                        ( model.questionnaireImporters, Cmd.none )
            in
            withSeed
                ( { model
                    | questionnaireImportersDropdown = state
                    , questionnaireImporters = questionnaireImporters
                  }
                , cmd
                )

        ActionsDropdownMsg state ->
            let
                ( questionnaireActions, cmd ) =
                    if ActionResult.isUnset model.questionnaireActions then
                        ( Loading
                        , QuestionnaireActionsApi.getQuestionnaireActionsFor model.uuid appState GetQuestionnaireActionsComplete
                        )

                    else
                        ( model.questionnaireActions, Cmd.none )
            in
            withSeed
                ( { model
                    | questionnaireActionsDropdown = state
                    , questionnaireActions = questionnaireActions
                  }
                , cmd
                )

        GotActionResult result ->
            case result of
                Ok actionResult ->
                    wrap { model | questionnaireActionResult = Just actionResult }

                Err err ->
                    wrap
                        { model
                            | questionnaireActionResult =
                                Just
                                    { success = False
                                    , message = "```\n" ++ D.errorToString err ++ "\n```"
                                    }
                        }

        CloseActionResult ->
            wrap { model | questionnaireActionResult = Nothing }

        OpenAction questionnaireAction ->
            withSeed
                ( model
                , Integrations.openAction
                    { url = questionnaireAction.url
                    , theme = Maybe.withDefault (LookAndFeel.getTheme appState.config.lookAndFeel) appState.theme
                    , data =
                        { projectUuid = model.uuid
                        , userToken = String.toMaybe appState.session.token.token
                        }
                    }
                )

        CollapseItem path ->
            updateCollapsedItems <|
                Set.insert path model.collapsedItems

        ExpandItem path ->
            updateCollapsedItems <|
                Set.remove path model.collapsedItems

        CollapseItems paths ->
            updateCollapsedItems <|
                List.foldl Set.insert model.collapsedItems paths

        ExpandItems paths ->
            updateCollapsedItems <|
                List.foldl Set.remove model.collapsedItems paths

        GotLocalStorageData value ->
            case decodeValue (D.field "key" D.string) value of
                Ok key ->
                    if key == localStorageViewSettingsKey then
                        case decodeValue localStorageViewSettingsDecoder value of
                            Ok data ->
                                wrap { model | viewSettings = data.value }

                            Err _ ->
                                wrap model

                    else if key == localStorageCollapsedItemKey model.uuid then
                        case decodeValue localStorageCollapsedItemsDecoder value of
                            Ok data ->
                                wrap { model | collapsedItems = data.value }

                            Err _ ->
                                wrap model

                    else if key == localStorageRightPanelKey model.uuid then
                        case decodeValue localStorageRightPanelDecoder value of
                            Ok data ->
                                withSeed ( model, dispatch (SetRightPanel data.value) )

                            Err _ ->
                                wrap model

                    else if key == localStorageViewResolvedKey model.uuid then
                        case decodeValue localStorageViewResolvedDecoder value of
                            Ok data ->
                                wrap { model | commentsViewResolved = data.value }

                            Err _ ->
                                wrap model

                    else if key == localStorageNamedOnlyKey model.uuid then
                        case decodeValue localStorageNamedOnlyDecoder value of
                            Ok data ->
                                wrap { model | historyModel = History.setNamedOnly data.value model.historyModel }

                            Err _ ->
                                wrap model

                    else
                        wrap model

                Err _ ->
                    wrap model

        CopyLinkToQuestion path ->
            let
                route =
                    Routing.toUrl appState <|
                        Routes.projectsDetailQuestionnaire model.uuid (Just (String.join "." path)) Nothing
            in
            ( appState.seed, { model | recentlyCopied = True }, Copy.copyToClipboard (AppState.getClientUrlRoot appState ++ route) )

        ClearRecentlyCopied ->
            wrap { model | recentlyCopied = False }

        GetCommentThreadsCompleted path result ->
            case result of
                Ok threads ->
                    let
                        newModel =
                            case Dict.get path threads of
                                Just commentThreads ->
                                    { model | commentThreadsMap = Dict.insert path (Success commentThreads) model.commentThreadsMap }

                                Nothing ->
                                    { model | commentThreadsMap = Dict.insert path (Success []) model.commentThreadsMap }
                    in
                    case model.mbCommentThreadUuid of
                        Just threadUuid ->
                            let
                                selector =
                                    "[data-comment-thread-uuid=\"" ++ Uuid.toString threadUuid ++ "\"]"

                                ( isPrivate, isResolved ) =
                                    Dict.get path newModel.commentThreadsMap
                                        |> Maybe.andThen ActionResult.toMaybe
                                        |> Maybe.andThen (List.find (\t -> t.uuid == threadUuid))
                                        |> Maybe.unwrap ( False, False ) (\t -> ( t.private, t.resolved ))
                            in
                            withSeed
                                ( { newModel
                                    | mbCommentThreadUuid = Nothing
                                    , commentsViewPrivate = isPrivate
                                    , commentsViewResolved = isResolved
                                  }
                                , Ports.scrollIntoView selector
                                )

                        Nothing ->
                            wrap newModel

                Err _ ->
                    wrap { model | commentThreadsMap = Dict.insert path (Error (gettext "Unable to get comments." appState.locale)) model.commentThreadsMap }

        GetQuestionnaireActionsComplete result ->
            wrap
                { model
                    | questionnaireActions =
                        case result of
                            Ok actions ->
                                Success actions

                            Err error ->
                                ApiError.toActionResult appState (gettext "Unable to get project actions." appState.locale) error
                }

        GetQuestionnaireImportersComplete result ->
            wrap
                { model
                    | questionnaireImporters =
                        case result of
                            Ok importers ->
                                Success importers

                            Err error ->
                                ApiError.toActionResult appState (gettext "Unable to get project importers." appState.locale) error
                }

        UserSuggestionDropdownMsg uuid threadUuid editorNote userSuggestionDropdownMsg ->
            let
                ( userSuggestionModalModel, userSuggestionCmd ) =
                    Dict.get uuid model.userSuggestionDropdownModels
                        |> Maybe.withDefault (UserSuggestionDropdown.init model.uuid threadUuid editorNote)
                        |> UserSuggestionDropdown.update appState userSuggestionDropdownMsg
            in
            withSeed
                ( { model | userSuggestionDropdownModels = Dict.insert uuid userSuggestionModalModel model.userSuggestionDropdownModels }
                , Cmd.map (UserSuggestionDropdownMsg uuid threadUuid editorNote) userSuggestionCmd
                )

        _ ->
            wrap model


localStorageCollapsedItemsCmd : Uuid -> Set String -> Cmd msg
localStorageCollapsedItemsCmd uuid items =
    let
        data =
            { key = localStorageCollapsedItemKey uuid
            , value = items
            }
    in
    data
        |> localStorageCollapsedItemsEncode
        |> Ports.localStorageSet


localStorageRightPanelCmd : Model -> Cmd msg
localStorageRightPanelCmd model =
    if model.rightPanel == RightPanel.None then
        Ports.localStorageRemove (localStorageRightPanelKey model.uuid)

    else
        let
            data =
                { key = localStorageRightPanelKey model.uuid
                , value = model.rightPanel
                }
        in
        data
            |> LocalStorageData.encode RightPanel.encode
            |> Ports.localStorageSet


localStorageViewResolvedCmd : Model -> Cmd msg
localStorageViewResolvedCmd model =
    let
        data =
            { key = localStorageViewResolvedKey model.uuid
            , value = model.commentsViewResolved
            }
    in
    data
        |> LocalStorageData.encode E.bool
        |> Ports.localStorageSet


localStorageNamedOnlyCmd : Model -> Cmd msg
localStorageNamedOnlyCmd model =
    let
        data =
            { key = localStorageNamedOnlyKey model.uuid
            , value = model.historyModel.namedOnly
            }
    in
    data
        |> LocalStorageData.encode E.bool
        |> Ports.localStorageSet


handleScrollToPath : Model -> Bool -> String -> ( Model, Cmd Msg )
handleScrollToPath model immediate path =
    let
        pathParts =
            String.split "." path

        chapterUuid =
            List.head pathParts
                |> Maybe.withDefault ""

        createSubpaths parts =
            case parts of
                [] ->
                    []

                _ ->
                    let
                        rest =
                            List.unconsLast parts
                                |> Maybe.unwrap [] Tuple.second
                    in
                    String.join "." parts :: createSubpaths rest

        newCollapsedItems =
            createSubpaths pathParts
                |> List.foldl (\currentPath collapsedItems -> Set.remove currentPath collapsedItems) model.collapsedItems

        selector =
            "[data-path=\"" ++ path ++ "\"]"

        scrollIntoViewCmd =
            if immediate then
                Ports.scrollIntoViewInstant selector

            else
                Ports.scrollIntoView selector
    in
    ( { model
        | activePage = PageChapter chapterUuid
        , removeItem = Nothing
        , collapsedItems = newCollapsedItems
      }
    , Cmd.batch
        [ scrollIntoViewCmd
        , localStorageCollapsedItemsCmd model.uuid newCollapsedItems
        ]
    )


handleShowTypeHints : AppState -> Context -> Model -> List String -> Bool -> String -> String -> ( Model, Cmd Msg )
handleShowTypeHints appState ctx model path emptySearch questionUuid value =
    if not emptySearch && String.isEmpty value then
        ( model, Cmd.none )

    else
        let
            typeHints =
                Just
                    { path = path
                    , searchValue = value
                    , hints = Loading
                    }

            cmd =
                loadTypeHints appState ctx model path questionUuid value
        in
        ( { model | typeHints = typeHints }, cmd )


handleTypeHintsInput : Model -> List String -> Bool -> Reply -> ( Model, Cmd Msg )
handleTypeHintsInput model path emptySearch reply =
    let
        ( ( debounce, debounceCmd ), newTypeHints ) =
            case ( emptySearch, reply.value ) of
                ( False, IntegrationReply (PlainType "") ) ->
                    ( ( model.typeHintsDebounce, Cmd.none ), Nothing )

                _ ->
                    let
                        questionUuid =
                            Maybe.withDefault "" (List.last path)

                        updatedTypeHints =
                            Just
                                { path = path
                                , searchValue = ReplyValue.getStringReply reply.value
                                , hints = Loading
                                }
                    in
                    ( Debounce.push
                        debounceConfig
                        ( path, questionUuid, ReplyValue.getStringReply reply.value )
                        model.typeHintsDebounce
                    , updatedTypeHints
                    )

        dispatchCmd =
            dispatch <|
                SetReply (pathToString path) reply
    in
    ( { model | typeHintsDebounce = debounce, typeHints = newTypeHints }
    , Cmd.batch [ debounceCmd, dispatchCmd ]
    )


handleTypeHintDebounceMsg : AppState -> Context -> Model -> Debounce.Msg -> ( Model, Cmd Msg )
handleTypeHintDebounceMsg appState ctx model debounceMsg =
    let
        load ( path, questionUuid, value ) =
            loadTypeHints appState ctx model path questionUuid value

        ( typeHintsDebounce, cmd ) =
            Debounce.update
                debounceConfig
                (Debounce.takeLast load)
                debounceMsg
                model.typeHintsDebounce
    in
    ( { model | typeHintsDebounce = typeHintsDebounce }, cmd )


handleTypeHintsLoaded : AppState -> Model -> List String -> String -> Result ApiError (List TypeHint) -> Model
handleTypeHintsLoaded appState model path value result =
    case model.typeHints of
        Just typeHints ->
            if typeHints.path == path && typeHints.searchValue == value then
                case result of
                    Ok hints ->
                        { model | typeHints = Just { typeHints | hints = Success hints } }

                    Err _ ->
                        { model | typeHints = Just { typeHints | hints = Error <| gettext "Unable to get type hints." appState.locale } }

            else
                model

        _ ->
            model


handleFeedbackModalMsg : AppState -> Model -> FeedbackModal.Msg -> ( Model, Cmd Msg )
handleFeedbackModalMsg appState model feedbackModalMsg =
    let
        ( feedbackModalModel, cmd ) =
            FeedbackModal.update feedbackModalMsg appState model.feedbackModalModel
    in
    ( { model | feedbackModalModel = feedbackModalModel }
    , Cmd.map FeedbackModalMsg cmd
    )


handleFileUploadModalMsg : AppState -> Model -> FileUploadModal.Msg -> ( Model, Cmd Msg )
handleFileUploadModalMsg appState model fileUploadModalMsg =
    let
        updateConfig =
            { wrapMsg = FileUploadModalMsg
            , setFileMsg = SetFile
            }

        ( fileUploadModalModel, fileUploadModalCmd ) =
            FileUploadModal.update appState updateConfig fileUploadModalMsg model.fileUploadModalModel
    in
    ( { model | fileUploadModalModel = fileUploadModalModel }
    , fileUploadModalCmd
    )


handleAddItem : AppState -> (Msg -> msg) -> Model -> String -> List String -> ( Seed, Model, Cmd msg )
handleAddItem appState wrapMsg model path originalItems =
    let
        ( uuid, newSeed ) =
            getUuidString appState.seed

        itemPath =
            path ++ "." ++ uuid

        scrollCmd =
            Ports.scrollIntoView ("[data-path=\"" ++ itemPath ++ "\"]")

        dispatchCmd =
            ItemListReply (originalItems ++ [ uuid ])
                |> createReply appState
                |> SetReply path
                |> wrapMsg
                |> dispatch
    in
    ( newSeed, model, Cmd.batch [ dispatchCmd, scrollCmd ] )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = TypeHintDebounceMsg
    }


loadTypeHints : AppState -> Context -> Model -> List String -> String -> String -> Cmd Msg
loadTypeHints appState ctx model path questionUuid value =
    TypeHintsApi.fetchTypeHints
        (Just model.questionnaire.packageId)
        ctx.events
        questionUuid
        value
        appState
        (TypeHintsLoaded path value)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        commentDeleteSub =
            case ( model.commentDeleting, model.commentDeletingListenClicks ) of
                ( Just _, False ) ->
                    Browser.Events.onAnimationFrame (\_ -> CommentDeleteListenClicks)

                ( Just _, True ) ->
                    Browser.Events.onClick (D.succeed (CommentDelete Nothing))

                _ ->
                    Sub.none

        commentDropdownSubs =
            Dict.toList model.commentDropdownStates
                |> List.map (\( uuid, state ) -> Dropdown.subscriptions state (CommentDropdownMsg uuid))

        splitPaneSubscriptions =
            Sub.map SplitPaneMsg <|
                SplitPane.subscriptions model.splitPane

        collapsedItemsSub =
            Ports.localStorageData GotLocalStorageData

        contentScrollSub =
            Ports.gotScrollTop GotContentScroll

        userSuggestionDropdownSubs =
            Dict.toList model.userSuggestionDropdownModels
                |> List.map (\( uuid, userSuggestionModalModel ) -> Sub.map (UserSuggestionDropdownMsg uuid userSuggestionModalModel.threadUuid userSuggestionModalModel.editorNote) (UserSuggestionDropdown.subscriptions userSuggestionModalModel))
    in
    Sub.batch
        ([ Dropdown.subscriptions model.viewSettingsDropdown ViewSettingsDropdownMsg
         , Dropdown.subscriptions model.questionnaireImportersDropdown ImportersDropdownMsg
         , Dropdown.subscriptions model.questionnaireActionsDropdown ActionsDropdownMsg
         , Integrations.actionSub GotActionResult
         , Integrations.integrationWidgetSub GotIntegrationWidgetValue
         , Sub.map HistoryMsg <| History.subscriptions model.historyModel
         , commentDeleteSub
         , splitPaneSubscriptions
         , collapsedItemsSub
         , contentScrollSub
         ]
            ++ commentDropdownSubs
            ++ userSuggestionDropdownSubs
        )



-- VIEW


view : AppState -> Config msg -> Context -> Model -> Html msg
view appState cfg ctx model =
    let
        ( toolbar, toolbarEnabled ) =
            if cfg.features.toolbarEnabled then
                ( Html.map cfg.wrapMsg <| viewQuestionnaireToolbar appState cfg model, True )

            else
                ( emptyNode, False )

        splitPaneConfig =
            SplitPane.createViewConfig
                { toMsg = cfg.wrapMsg << SplitPaneMsg
                , customSplitter = Nothing
                }
    in
    div
        [ class "questionnaire"
        , classList [ ( "toolbar-enabled", toolbarEnabled ) ]
        ]
        [ toolbar
        , div [ class "questionnaire__body" ]
            [ SplitPane.view splitPaneConfig
                (Html.map cfg.wrapMsg <| viewQuestionnaireLeftPanel appState cfg model)
                (Html.map cfg.wrapMsg <| viewQuestionnaireContent appState cfg ctx model)
                model.splitPane
            , viewQuestionnaireRightPanel appState cfg model
            ]
        , Html.map cfg.wrapMsg <| viewActionResultModal appState model
        , Html.map cfg.wrapMsg <| viewPhaseModal appState model
        , Html.map (cfg.wrapMsg << FeedbackModalMsg) <| FeedbackModal.view appState model.feedbackModalModel
        , Html.map (cfg.wrapMsg << FileUploadModalMsg) <| FileUploadModal.view appState cfg.isKmEditor model.fileUploadModalModel
        , Html.map cfg.wrapMsg <| viewRemoveItemModal appState model
        , Html.map cfg.wrapMsg <| viewFileDeleteModal appState model
        ]



-- QUESTIONNAIRE - TOOLBAR


viewQuestionnaireToolbar : AppState -> Config msg -> Model -> Html Msg
viewQuestionnaireToolbar appState cfg model =
    let
        viewDropdown =
            let
                viewSettings =
                    model.viewSettings

                settingsIcon enabled =
                    if enabled then
                        faSet "_global.success" appState

                    else
                        emptyNode
            in
            div [ class "item-group" ]
                [ Dropdown.dropdown model.viewSettingsDropdown
                    { options = []
                    , toggleMsg = ViewSettingsDropdownMsg
                    , toggleButton =
                        Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item" ] ]
                            [ text (gettext "View" appState.locale) ]
                    , items =
                        [ Dropdown.anchorItem
                            [ onClick (SetViewSettings QuestionnaireViewSettings.all) ]
                            [ text (gettext "Show all" appState.locale) ]
                        , Dropdown.anchorItem
                            [ onClick (SetViewSettings QuestionnaireViewSettings.none) ]
                            [ text (gettext "Hide all" appState.locale) ]
                        , Dropdown.divider
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon", onClick (SetViewSettings (QuestionnaireViewSettings.toggleAnsweredBy viewSettings)) ]
                            [ settingsIcon viewSettings.answeredBy, text (gettext "Answered by" appState.locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.togglePhases viewSettings))
                            ]
                            [ settingsIcon viewSettings.phases, text (gettext "Phases" appState.locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleTags viewSettings))
                            ]
                            [ settingsIcon viewSettings.tags, text (gettext "Question tags" appState.locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleNonDesirableQuestions viewSettings))
                            ]
                            [ settingsIcon viewSettings.nonDesirableQuestions, text (gettext "Non-desirable questions" appState.locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleMetricValues viewSettings))
                            ]
                            [ settingsIcon viewSettings.metricValues, text (gettext "Metric values" appState.locale) ]
                        ]
                    }
                ]

        importersDropdown =
            if importersAvailable appState cfg model then
                div [ class "item-group" ]
                    [ Dropdown.dropdown model.questionnaireImportersDropdown
                        { options = []
                        , toggleMsg = ImportersDropdownMsg
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item" ] ]
                                [ text (gettext "Import replies" appState.locale) ]
                        , items =
                            ActionResultBlock.dropdownView appState importerDropdownItem model.questionnaireImporters
                        }
                    ]

            else
                emptyNode

        importerDropdownItem importer =
            Dropdown.anchorItem
                (class "dropdown-item" :: linkToAttributes appState (Routes.projectsImport model.uuid importer.id))
                [ text importer.name ]

        actionsDropdown =
            if actionsAvailable appState cfg model then
                div [ class "item-group" ]
                    [ Dropdown.dropdown model.questionnaireActionsDropdown
                        { options = []
                        , toggleMsg = ActionsDropdownMsg
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item item-actions" ] ]
                                [ span [ class "icon" ] []
                                , text (gettext "Actions" appState.locale)
                                ]
                        , items =
                            ActionResultBlock.dropdownView appState actionDropdownItem model.questionnaireActions
                        }
                    ]

            else
                emptyNode

        actionDropdownItem action =
            Dropdown.anchorItem
                [ class "dropdown-item", onClick (OpenAction action) ]
                [ text action.name ]

        navButton buttonElement visibleCondition =
            if visibleCondition then
                buttonElement

            else
                emptyNode

        ( todosPanel, todosOpen ) =
            case model.rightPanel of
                RightPanel.TODOs ->
                    ( RightPanel.None, True )

                _ ->
                    ( RightPanel.TODOs, False )

        ( commentsOverviewPanel, commentsOverviewOpen ) =
            case model.rightPanel of
                RightPanel.CommentsOverview ->
                    ( RightPanel.None, True )

                RightPanel.Comments _ ->
                    ( RightPanel.CommentsOverview, True )

                _ ->
                    ( RightPanel.CommentsOverview, False )

        ( versionsPanel, versionsOpen ) =
            case model.rightPanel of
                RightPanel.VersionHistory ->
                    ( RightPanel.None, True )

                _ ->
                    ( RightPanel.VersionHistory, False )

        ( warningsPanel, warningsOpen ) =
            case model.rightPanel of
                RightPanel.Warnings ->
                    ( RightPanel.None, True )

                _ ->
                    ( RightPanel.Warnings, False )

        todosLength =
            QuestionnaireQuestionnaire.todosLength model.questionnaire

        todosBadge =
            if todosLength > 0 then
                Badge.danger [ class "rounded-pill" ] [ text (String.fromInt todosLength) ]

            else
                emptyNode

        todosButtonVisible =
            Feature.projectTodos appState model.questionnaire

        todosButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", todosOpen ) ], onClick (SetRightPanel todosPanel) ]
                    [ text (gettext "TODOs" appState.locale)
                    , todosBadge
                    ]
                ]

        commentsLength =
            QuestionnaireQuestionnaire.commentsLength model.questionnaire

        commentsBadge =
            if commentsLength > 0 then
                Badge.secondary [ class "rounded-pill", dataCy "questionnaire_toolbar_comments_count" ] [ text (String.fromInt commentsLength) ]

            else
                emptyNode

        commentsOverviewButtonVisible =
            Feature.projectCommentAdd appState model.questionnaire

        commentsOverviewButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", commentsOverviewOpen ) ], onClick (SetRightPanel commentsOverviewPanel) ]
                    [ text (gettext "Comments" appState.locale)
                    , commentsBadge
                    ]
                ]

        warningsLength =
            QuestionnaireQuestionnaire.warningsLength model.questionnaire

        warningsButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", warningsOpen ) ], onClick (SetRightPanel warningsPanel) ]
                    [ text (gettext "Warnings" appState.locale)
                    , Badge.danger [ class "rounded-pill" ] [ text (String.fromInt warningsLength) ]
                    ]
                ]

        warningsButtonVisible =
            warningsLength > 0

        versionHistoryButtonVisible =
            Feature.projectVersionHistory appState model.questionnaire

        versionHistoryButton =
            div [ class "item-group" ]
                [ a [ class "item", classList [ ( "selected", versionsOpen ) ], onClick (SetRightPanel versionsPanel) ]
                    [ text (gettext "Version history" appState.locale) ]
                ]

        ( expandIcon, expandMsg ) =
            if AppState.isFullscreen appState then
                ( faSet "questionnaire.shrink" appState, SetFullscreen False )

            else
                ( faSet "questionnaire.expand" appState, SetFullscreen True )
    in
    div [ class "questionnaire__toolbar" ]
        [ div [ class "questionnaire__toolbar__left" ]
            [ viewDropdown
            , importersDropdown
            , actionsDropdown
            ]
        , div [ class "questionnaire__toolbar__right" ]
            [ navButton warningsButton warningsButtonVisible
            , navButton todosButton todosButtonVisible
            , navButton commentsOverviewButton commentsOverviewButtonVisible
            , navButton versionHistoryButton versionHistoryButtonVisible
            , div [ class "item-group" ]
                [ a [ class "item", onClick expandMsg ] [ expandIcon ]
                ]
            ]
        ]


viewActionResultModal : AppState -> Model -> Html Msg
viewActionResultModal appState model =
    let
        modalTitle actionResult =
            if actionResult.success then
                [ span [ class "text-success me-2" ] [ faSet "_global.success" appState ]
                , text (gettext "Action succeeded!" appState.locale)
                ]

            else
                [ span [ class "text-danger me-2" ] [ faSet "_global.error" appState ]
                , text (gettext "Action failed!" appState.locale)
                ]

        modalBody =
            Maybe.unwrap [] (List.singleton << Markdown.toHtml [] << .message) model.questionnaireActionResult

        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] (Maybe.unwrap [] modalTitle model.questionnaireActionResult)
                ]
            , div [ class "modal-body" ] modalBody
            , div [ class "modal-footer" ]
                [ button [ class "btn btn-primary", onClick CloseActionResult ]
                    [ text (gettext "OK" appState.locale) ]
                ]
            ]
    in
    Modal.simple
        { modalContent = modalContent
        , visible = Maybe.isJust model.questionnaireActionResult
        , dataCy = "questionnaire-action-result"
        }



-- QUESTIONNAIRE - LEFT PANEL


viewQuestionnaireLeftPanel : AppState -> Config msg -> Model -> Html Msg
viewQuestionnaireLeftPanel appState cfg model =
    div [ class "questionnaire__left-panel" ]
        [ viewQuestionnaireLeftPanelPhaseSelection appState cfg model
        , viewQuestionnaireLeftPanelChapters appState model
        ]



-- QUESTIONNAIRE - LEFT PANEL - PHASE SELECTION


viewQuestionnaireLeftPanelPhaseSelection : AppState -> Config msg -> Model -> Html Msg
viewQuestionnaireLeftPanelPhaseSelection appState cfg model =
    let
        phases =
            KnowledgeModel.getPhases model.questionnaire.knowledgeModel
    in
    if List.length phases > 0 then
        let
            selectedPhaseTitle =
                List.find ((==) (Maybe.map Uuid.toString model.questionnaire.phaseUuid) << Just << .uuid) phases
                    |> Maybe.orElse (List.head phases)
                    |> Maybe.unwrap "" .title

            phaseButtonOnClick =
                if cfg.features.readonly then
                    []

                else
                    [ onClick (PhaseModalUpdate True Nothing) ]

            phaseButton =
                button
                    ([ class "btn btn-input w-100"
                     , onClick (PhaseModalUpdate True Nothing)
                     , dataCy "phase-selection"
                     , disabled cfg.features.readonly
                     ]
                        ++ phaseButtonOnClick
                    )
                    [ text selectedPhaseTitle ]
        in
        div [ class "questionnaire__left-panel__phase" ]
            [ label [] [ text (gettext "Current Phase" appState.locale) ]
            , phaseButton
            ]

    else
        emptyNode


viewPhaseModal : AppState -> Model -> Html Msg
viewPhaseModal appState model =
    let
        phases =
            KnowledgeModel.getPhases model.questionnaire.knowledgeModel

        currentPhaseIndex =
            case model.questionnaire.phaseUuid of
                Just phaseUuid ->
                    List.map .uuid phases
                        |> List.elemIndex (Uuid.toString phaseUuid)
                        |> Maybe.withDefault 0

                Nothing ->
                    0

        viewPhase : Int -> Phase -> Html Msg
        viewPhase index phase =
            let
                descriptionElement =
                    case phase.description of
                        Just description ->
                            div [ class "phase-description" ] [ text description ]

                        Nothing ->
                            emptyNode

                clickAttribute =
                    if index == currentPhaseIndex then
                        []

                    else
                        [ onClick (PhaseModalUpdate False (Uuid.fromString phase.uuid)) ]
            in
            div
                ([ class "phase"
                 , classList
                    [ ( "phase-done", index < currentPhaseIndex )
                    , ( "phase-active", index == currentPhaseIndex )
                    ]
                 , dataCy "phase-option"
                 ]
                    ++ clickAttribute
                )
                [ div [ class "phase-title" ] [ text phase.title ]
                , descriptionElement
                ]
    in
    Modal.simpleWithAttrs [ class "PhaseSelectionModal modal-wide" ]
        { modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Select phase" appState.locale) ]
                , button
                    [ class "close"
                    , onClick (PhaseModalUpdate False Nothing)
                    ]
                    [ faSet "_global.close" appState ]
                ]
            , div [ class "modal-body" ]
                [ div [] (List.indexedMap viewPhase phases)
                ]
            ]
        , visible = model.phaseModalOpen
        , dataCy = "phase-selection"
        }



-- QUESTIONNAIRE - LEFT PANEL - CHAPTERS


viewQuestionnaireLeftPanelChapters : AppState -> Model -> Html Msg
viewQuestionnaireLeftPanelChapters appState model =
    let
        mbActiveChapterUuid =
            case model.activePage of
                PageChapter chapterUuid ->
                    Just chapterUuid

                _ ->
                    Nothing

        navigationTreeConfig =
            { activeChapterUuid = mbActiveChapterUuid
            , nonDesirableQuestions = model.viewSettings.nonDesirableQuestions
            , questionnaire = model.questionnaire
            , openChapter = SetActivePage << PageChapter
            , scrollToPath = ScrollToPath
            , wrapMsg = NavigationTreeMsg
            }
    in
    NavigationTree.view appState navigationTreeConfig model.navigationTreeModel



-- QUESTIONNAIRE - RIGHT PANEL


viewQuestionnaireRightPanel : AppState -> Config msg -> Model -> Html msg
viewQuestionnaireRightPanel appState cfg model =
    let
        wrapPanel content =
            div [ class "questionnaire__right-panel" ]
                content
    in
    case model.rightPanel of
        RightPanel.None ->
            emptyNode

        RightPanel.TODOs ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelTodos appState model ]

        RightPanel.CommentsOverview ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelCommentsOverview appState model ]

        RightPanel.Comments path ->
            wrapPanel <|
                [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelComments appState model path ]

        RightPanel.VersionHistory ->
            let
                historyCfg =
                    { questionnaire = model.questionnaire
                    , wrapMsg = cfg.wrapMsg << HistoryMsg
                    , scrollMsg = cfg.wrapMsg << ScrollToPath
                    , createVersionMsg = cfg.wrapMsg << CreateNamedVersion
                    , renameVersionMsg = cfg.wrapMsg << RenameVersion
                    , deleteVersionMsg = cfg.wrapMsg << DeleteVersion
                    , previewQuestionnaireEventMsg = cfg.previewQuestionnaireEventMsg
                    , revertQuestionnaireMsg = cfg.revertQuestionnaireMsg
                    }

                versionsAndEvents =
                    ActionResult.combine model.questionnaireVersions model.questionnaireEvents
            in
            wrapPanel <|
                [ History.view appState historyCfg model.historyModel versionsAndEvents
                , Html.map (cfg.wrapMsg << VersionModalMsg) <| VersionModal.view appState model.versionModalModel
                , Html.map (cfg.wrapMsg << DeleteVersionModalMsg) <| DeleteVersionModal.view appState model.deleteVersionModalModel
                ]

        RightPanel.Warnings ->
            if QuestionnaireQuestionnaire.warningsLength model.questionnaire > 0 then
                wrapPanel <|
                    [ Html.map cfg.wrapMsg <| viewQuestionnaireRightPanelWarnings model ]

            else
                emptyNode



-- QUESTIONNAIRE - RIGHT PANEL - TODOS


viewQuestionnaireRightPanelTodos : AppState -> Model -> Html Msg
viewQuestionnaireRightPanelTodos appState model =
    let
        todos =
            QuestionnaireQuestionnaire.getTodos model.questionnaire

        viewTodoGroup group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [ class "fa-ul" ] (List.map viewTodo group.todos)
                ]

        viewTodo todo =
            li []
                [ span [ class "fa-li" ] [ fa "fas fa-edit" ]
                , a [ onClick (ScrollToPath todo.path) ] [ text <| Question.getTitle todo.question ]
                ]
    in
    if List.isEmpty todos then
        div [ class "todos todos-empty" ] <|
            [ illustratedMessage Undraw.feelingHappy (gettext "All TODOs have been completed." appState.locale) ]

    else
        div [ class "todos" ] <|
            List.map viewTodoGroup (QuestionnaireTodoGroup.groupTodos todos)



-- QUESTIONNAIRE - RIGHT PANEL - WARNINGS


viewQuestionnaireRightPanelWarnings : Model -> Html Msg
viewQuestionnaireRightPanelWarnings model =
    let
        warnings =
            QuestionnaireQuestionnaire.getWarnings model.questionnaire

        viewWarningGroup group =
            div []
                [ strong [] [ text group.chapter.title ]
                , ul [ class "fa-ul" ] (List.map viewTodo group.todos)
                ]

        viewTodo todo =
            li []
                [ span [ class "fa-li" ] [ fa "fas fa-exclamation-triangle" ]
                , a [ onClick (ScrollToPath todo.path) ] [ text <| Question.getTitle todo.question ]
                ]
    in
    div [ class "todos" ] <|
        List.map viewWarningGroup (QuestionnaireTodoGroup.groupTodos warnings)



-- QUESTIONNAIRE - RIGHT PANEL - COMMENTS OVERVIEW


viewQuestionnaireRightPanelCommentsOverview : AppState -> Model -> Html Msg
viewQuestionnaireRightPanelCommentsOverview appState model =
    let
        viewChapterComments group =
            let
                anyUnresolvedComments =
                    List.any (\c -> c.unresolvedComments > 0) group.comments
            in
            if not model.commentsViewResolved && not anyUnresolvedComments then
                emptyNode

            else
                div []
                    [ strong [] [ text group.chapter.title ]
                    , ul [ class "fa-ul" ] (List.map viewQuestionComments group.comments)
                    ]

        viewQuestionComments comment =
            if not model.commentsViewResolved && comment.unresolvedComments == 0 then
                emptyNode

            else
                let
                    resolvedCommentCount =
                        if model.commentsViewResolved && comment.resolvedComments > 0 then
                            Badge.success [ class "rounded-pill ms-1" ]
                                [ fa "fas fa-check"
                                , text (String.fromInt comment.resolvedComments)
                                ]

                        else
                            emptyNode
                in
                li []
                    [ span [ class "fa-li" ] [ fa "far fa-comment" ]
                    , a [ onClick (OpenComments False comment.path) ]
                        [ span [ class "question" ] [ text <| Question.getTitle comment.question ]
                        , span [ class "text-nowrap" ]
                            [ Badge.light [ class "rounded-pill" ] [ text (String.fromInt comment.unresolvedComments) ]
                            , resolvedCommentCount
                            ]
                        ]
                    ]

        groupComments comments =
            let
                fold comment acc =
                    if List.any (\group -> group.chapter.uuid == comment.chapter.uuid) acc then
                        List.map
                            (\group ->
                                if group.chapter.uuid == comment.chapter.uuid then
                                    { group | comments = group.comments ++ [ comment ] }

                                else
                                    group
                            )
                            acc

                    else
                        acc ++ [ { chapter = comment.chapter, comments = [ comment ] } ]
            in
            List.foldl fold [] comments

        questionnaireComments =
            QuestionnaireQuestionnaire.getComments model.questionnaire

        commentsEmpty =
            if model.commentsViewResolved then
                List.isEmpty questionnaireComments

            else
                questionnaireComments
                    |> List.map (\group -> group.unresolvedComments)
                    |> List.sum
                    |> (==) 0

        content =
            if commentsEmpty then
                [ div [ class "alert alert-info" ]
                    [ p
                        []
                        (String.formatHtml (gettext "Click the %s icon to add new comments to a question." appState.locale) [ faSet "questionnaire.comments" appState ])
                    ]
                ]

            else
                List.map viewChapterComments (groupComments questionnaireComments)
    in
    div [ class "comments-overview Comments" ]
        (viewCommentsResolvedSelect appState model :: content)


viewCommentsResolvedSelect : AppState -> Model -> Html Msg
viewCommentsResolvedSelect appState model =
    div [ class "form-check" ]
        [ label [ class "form-check-label form-check-toggle" ]
            [ input [ type_ "checkbox", class "form-check-input", onCheck CommentsViewResolved, checked model.commentsViewResolved ] []
            , span [] [ text (gettext "View resolved comments" appState.locale) ]
            ]
        ]



-- QUESTIONNAIRE - RIGHT PANEL - COMMENTS


viewQuestionnaireRightPanelComments : AppState -> Model -> String -> Html Msg
viewQuestionnaireRightPanelComments appState model path =
    Dict.get path model.commentThreadsMap
        |> Maybe.withDefault ActionResult.Loading
        |> ActionResultBlock.view appState (viewQuestionnaireRightPanelCommentsLoaded appState model path)


viewQuestionnaireRightPanelCommentsLoaded : AppState -> Model -> String -> List CommentThread -> Html Msg
viewQuestionnaireRightPanelCommentsLoaded appState model path commentThreads =
    let
        filter =
            if model.commentsViewResolved then
                always True

            else
                \group -> group.unresolvedComments > 0

        questionnaireComments =
            QuestionnaireQuestionnaire.getComments model.questionnaire

        comments =
            questionnaireComments
                |> List.filter filter
                |> List.map .path

        nextPrevNavigation =
            if List.length comments > 1 then
                case List.elemIndex path comments of
                    Just index ->
                        let
                            previousCommentsPath =
                                Maybe.withDefault "" <|
                                    ListUtils.findPreviousInfinite path comments

                            nextCommentsPath =
                                Maybe.withDefault "" <|
                                    ListUtils.findNextInfinite path comments

                            commentCountTooltip =
                                if model.commentsViewResolved then
                                    gettext "Resolved and unresolved comments" appState.locale

                                else
                                    gettext "Unresolved comments" appState.locale

                            numberText =
                                span
                                    (class "text-muted"
                                        :: dataCy "comments_nav_count"
                                        :: tooltip commentCountTooltip
                                    )
                                    [ text
                                        (String.format "%s/%s"
                                            [ String.fromInt (index + 1)
                                            , String.fromInt (List.length comments)
                                            ]
                                        )
                                    ]
                        in
                        div
                            [ class "comments-navigation"
                            ]
                            [ a
                                [ onClick (OpenComments False previousCommentsPath)
                                , dataCy "comments_nav_prev"
                                ]
                                [ fa "fas fa-arrow-left me-2"
                                , text (gettext "Previous" appState.locale)
                                ]
                            , numberText
                            , a
                                [ onClick (OpenComments False nextCommentsPath)
                                , dataCy "comments_nav_next"
                                ]
                                [ text (gettext "Next" appState.locale)
                                , fa "fas fa-arrow-right ms-2"
                                ]
                            ]

                    Nothing ->
                        emptyNode

            else
                emptyNode

        navigationView =
            if Feature.projectCommentPrivate appState model.questionnaire then
                viewCommentsNavigation appState model commentThreads

            else
                emptyNode

        editorNoteExplanation =
            if model.commentsViewPrivate then
                div [ class "alert alert-editor-notes" ]
                    [ i [ class "fa fas fa-lock" ] []
                    , span [] [ text (gettext "Editor notes are only visible to project Editors and Owners." appState.locale) ]
                    ]

            else
                emptyNode

        viewFilteredCommentThreads condition =
            commentThreads
                |> List.filter (\thread -> thread.private == model.commentsViewPrivate)
                |> List.filter condition
                |> List.sortWith CommentThread.compare
                |> List.map (viewCommentThread appState model path)

        resolvedThreadsView =
            if model.commentsViewResolved then
                div [] (viewFilteredCommentThreads (\thread -> thread.resolved))

            else
                emptyNode

        commentThreadsView =
            div [] (viewFilteredCommentThreads (\thread -> not thread.resolved))

        newThreadForm =
            viewCommentReplyForm appState
                { submitText = gettext "Comment" appState.locale
                , placeholderText = gettext "Create a new comment..." appState.locale
                , model = model
                , path = path
                , mbThreadUuid = Nothing
                , private = model.commentsViewPrivate
                }
    in
    div [ class "Comments" ]
        [ nextPrevNavigation
        , viewCommentsResolvedSelect appState model
        , navigationView
        , resolvedThreadsView
        , commentThreadsView
        , editorNoteExplanation
        , newThreadForm
        ]


viewCommentsNavigation : AppState -> Model -> List CommentThread -> Html Msg
viewCommentsNavigation appState model commentThreads =
    let
        threadCount privatePredicate resolvedPredicate =
            List.filter (\c -> privatePredicate c && resolvedPredicate c) commentThreads
                |> List.map (List.length << .comments)
                |> List.sum

        publicThreadsCount =
            threadCount (not << .private) (not << .resolved)

        privateThreadsCount =
            threadCount .private (not << .resolved)

        resolvedPublicThreadsCount =
            threadCount (not << .private) .resolved

        resolvedPrivateThreadsCount =
            threadCount .private .resolved

        toBadge count =
            if count == 0 then
                emptyNode

            else
                Badge.light [ class "rounded-pill" ] [ text (String.fromInt count) ]

        toResolvedBadge count =
            if model.commentsViewResolved && count > 0 then
                Badge.success [ class "rounded-pill" ]
                    [ fa "fas fa-check"
                    , text (String.fromInt count)
                    ]

            else
                emptyNode
    in
    ul [ class "nav nav-underline-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", not model.commentsViewPrivate ) ]
                , onClick (CommentsViewPrivate False)
                , dataCy "comments_nav_comments"
                ]
                [ span [ attribute "data-content" (gettext "Comments" appState.locale) ]
                    [ text (gettext "Comments" appState.locale) ]
                , toBadge publicThreadsCount
                , toResolvedBadge resolvedPublicThreadsCount
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link nav-link-editor-notes"
                , classList [ ( "active", model.commentsViewPrivate ) ]
                , onClick (CommentsViewPrivate True)
                , dataCy "comments_nav_private-notes"
                ]
                [ span [ attribute "data-content" (gettext "Editor notes" appState.locale) ]
                    [ text (gettext "Editor notes" appState.locale) ]
                , toBadge privateThreadsCount
                , toResolvedBadge resolvedPrivateThreadsCount
                ]
            ]
        ]


viewCommentThread : AppState -> Model -> String -> CommentThread -> Html Msg
viewCommentThread appState model path commentThread =
    let
        comments =
            List.sortWith Comment.compare commentThread.comments

        deleteOverlay =
            if model.commentDeleting == Maybe.map .uuid (List.head comments) then
                viewCommentDeleteOverlay appState
                    { deleteMsg = CommentThreadDelete path commentThread
                    , deleteText = gettext "Delete this comment thread?" appState.locale
                    , extraClass = "CommentDeleteOverlay--Thread"
                    }

            else
                emptyNode

        replyForm =
            if commentThread.resolved then
                emptyNode

            else
                viewCommentReplyForm appState
                    { submitText = gettext "Reply" appState.locale
                    , placeholderText = gettext "Reply..." appState.locale
                    , model = model
                    , path = path
                    , mbThreadUuid = Just commentThread.uuid
                    , private = commentThread.private
                    }

        assignedHeader =
            case commentThread.assignedTo of
                Just assignedTo ->
                    let
                        assignedToYou =
                            Just assignedTo.uuid == Maybe.map .uuid appState.config.user

                        assignedContent =
                            if assignedToYou then
                                [ fa "fas fa-user-pen fa-fw me-1"
                                , text (gettext "Assigned to you" appState.locale)
                                ]

                            else
                                [ fa "fas fa-user-check fa-fw me-1"
                                , text (String.format (gettext "Assigned to %s" appState.locale) [ User.fullName assignedTo ])
                                ]
                    in
                    div
                        [ class "CommentThread__AssignedHeader"
                        , classList [ ( "CommentThread__AssignedHeader--You", assignedToYou ) ]
                        ]
                        assignedContent

                Nothing ->
                    emptyNode

        commentViews =
            List.indexedMap (viewComment appState model path commentThread) comments
    in
    div
        [ class "CommentThread"
        , classList
            [ ( "CommentThread--Resolved", commentThread.resolved )
            , ( "CommentThread--Private", commentThread.private )
            ]
        , attribute "data-comment-thread-uuid" (Uuid.toString commentThread.uuid)
        ]
        (assignedHeader
            :: commentViews
            ++ [ replyForm, deleteOverlay ]
        )


viewComment : AppState -> Model -> String -> CommentThread -> Int -> Comment -> Html Msg
viewComment appState model path commentThread index comment =
    let
        commentHeader =
            viewCommentHeader appState model path commentThread index comment

        mbEditValue =
            Dict.get (Uuid.toString comment.uuid) model.commentEditInputs

        content =
            case mbEditValue of
                Just editValue ->
                    div []
                        [ resizableTextarea 2
                            editValue
                            [ class "form-control mb-1", onInput (CommentEditInput comment.uuid) ]
                            []
                        , div []
                            [ button
                                [ class "btn btn-primary btn-sm me-1"
                                , disabled (String.isEmpty editValue)
                                , onClick (CommentEditSubmit path commentThread.uuid comment.uuid editValue commentThread.private)
                                ]
                                [ text (gettext "Edit" appState.locale) ]
                            , button
                                [ class "btn btn-outline-secondary btn-sm"
                                , onClick (CommentEditCancel comment.uuid)
                                ]
                                [ text (gettext "Cancel" appState.locale) ]
                            ]
                        ]

                Nothing ->
                    div [] [ Markdown.toHtml [ class "Comment_MD" ] comment.text ]

        deleteOverlay =
            if index /= 0 && model.commentDeleting == Just comment.uuid then
                viewCommentDeleteOverlay appState
                    { deleteMsg = CommentDeleteSubmit path commentThread.uuid comment.uuid commentThread.private
                    , deleteText = gettext "Delete this comment?" appState.locale
                    , extraClass = "CommentDeleteOverlay--Comment"
                    }

            else
                emptyNode
    in
    div [ class "Comment" ]
        [ commentHeader
        , content
        , deleteOverlay
        ]


viewCommentHeader : AppState -> Model -> String -> CommentThread -> Int -> Comment -> Html Msg
viewCommentHeader appState model path commentThread index comment =
    let
        resolveAction =
            if index == 0 && Feature.projectCommentThreadResolve appState model.questionnaire commentThread then
                a
                    ([ class "ms-1"
                     , onClick (CommentThreadResolve path commentThread)
                     , dataCy "comments_comment_resolve"
                     ]
                        ++ tooltipLeft (gettext "Resolve comment thread" appState.locale)
                    )
                    [ faSet "questionnaire.commentsResolve" appState ]

            else
                emptyNode

        assignAction =
            if index == 0 && Feature.projectCommentThreadAssign appState model.questionnaire commentThread then
                let
                    viewConfig =
                        { wrapMsg = UserSuggestionDropdownMsg (Uuid.toString comment.uuid) commentThread.uuid commentThread.private
                        , selectMsg = CommentThreadAssign path commentThread << Just
                        }

                    userSuggestionDropdownModel =
                        Dict.get (Uuid.toString comment.uuid) model.userSuggestionDropdownModels
                            |> Maybe.withDefault (UserSuggestionDropdown.init model.uuid commentThread.uuid commentThread.private)
                in
                UserSuggestionDropdown.view viewConfig appState userSuggestionDropdownModel

            else
                emptyNode

        removeAssignedAction =
            Dropdown.anchorItem
                [ onClick (CommentThreadAssign path commentThread Nothing) ]
                [ text (gettext "Remove assignment" appState.locale) ]

        removeAssignedActionVisible =
            index == 0 && Feature.projectCommentThreadRemoveAssign appState model.questionnaire commentThread

        reopenAction =
            Dropdown.anchorItem
                [ onClick (CommentThreadReopen path commentThread) ]
                [ text (gettext "Reopen" appState.locale) ]

        reopenActionVisible =
            index == 0 && Feature.projectCommentThreadReopen appState model.questionnaire commentThread

        editAction =
            Dropdown.anchorItem
                [ onClick (CommentEditInput comment.uuid comment.text) ]
                [ text (gettext "Edit" appState.locale) ]

        editActionVisible =
            Feature.projectCommentEdit appState model.questionnaire commentThread comment

        deleteAction =
            Dropdown.anchorItem
                [ onClick (CommentDelete (Just comment.uuid))
                , dataCy "comments_comment_menu_delete"
                ]
                [ text (gettext "Delete" appState.locale) ]

        deleteActionVisible =
            (index == 0 && Feature.projectCommentThreadDelete appState model.questionnaire commentThread)
                || (index /= 0 && Feature.projectCommentDelete appState model.questionnaire commentThread comment)

        actions =
            []
                |> listInsertIf removeAssignedAction removeAssignedActionVisible
                |> listInsertIf reopenAction reopenActionVisible
                |> listInsertIf editAction editActionVisible
                |> listInsertIf deleteAction deleteActionVisible

        dropdown =
            if List.isEmpty actions then
                emptyNode

            else
                let
                    dropdownState =
                        Dict.get (Uuid.toString comment.uuid) model.commentDropdownStates
                            |> Maybe.withDefault Dropdown.initialState
                in
                Dropdown.dropdown dropdownState
                    { options = [ Dropdown.attrs [ class "ListingDropdown", dataCy "comments_comment_menu" ], Dropdown.alignMenuRight ]
                    , toggleMsg = CommentDropdownMsg (Uuid.toString comment.uuid)
                    , toggleButton =
                        Dropdown.toggle [ Button.roleLink ]
                            [ faSet "listing.actions" appState ]
                    , items = actions
                    }

        createdLabel =
            TimeUtils.toReadableDateTime appState.timeZone comment.createdAt

        editedLabel =
            if comment.createdAt /= comment.updatedAt then
                span (tooltip (TimeUtils.toReadableDateTime appState.timeZone comment.updatedAt))
                    [ text <| " (" ++ gettext "edited" appState.locale ++ ")" ]

            else
                emptyNode

        userForIcon =
            case comment.createdBy of
                Just createdBy ->
                    { gravatarHash = createdBy.gravatarHash
                    , imageUrl = createdBy.imageUrl
                    }

                Nothing ->
                    { gravatarHash = ""
                    , imageUrl = Nothing
                    }
    in
    div [ class "Comment__Header" ]
        [ UserIcon.view userForIcon
        , div [ class "Comment__Header__User" ]
            [ strong [ class "Comment__Header__User__Name" ]
                [ text (Maybe.unwrap (gettext "Anonymous user" appState.locale) User.fullName comment.createdBy)
                ]
            , span [ class "Comment__Header__User__Time" ] [ text createdLabel, editedLabel ]
            ]
        , resolveAction
        , assignAction
        , dropdown
        ]


viewCommentReplyForm :
    AppState
    ->
        { submitText : String
        , placeholderText : String
        , model : Model
        , path : String
        , mbThreadUuid : Maybe Uuid
        , private : Bool
        }
    -> Html Msg
viewCommentReplyForm appState { submitText, placeholderText, model, path, mbThreadUuid, private } =
    let
        commentValue =
            model.commentInputs
                |> Dict.get (path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid)
                |> Maybe.withDefault ""

        cyFormType base =
            let
                privateType =
                    if private then
                        "private"

                    else
                        "public"
            in
            case mbThreadUuid of
                Just _ ->
                    base ++ "_reply_" ++ privateType

                Nothing ->
                    base ++ "_new_" ++ privateType

        newThreadFormSubmit =
            if String.isEmpty commentValue then
                emptyNode

            else
                div []
                    [ button
                        [ class "btn btn-primary btn-sm me-1"
                        , onClick (CommentSubmit path mbThreadUuid commentValue private)
                        , dataCy (cyFormType "comments_reply-form_submit")
                        ]
                        [ text submitText ]
                    , button
                        [ class "btn btn-outline-secondary btn-sm"
                        , onClick (CommentInput path mbThreadUuid "")
                        , dataCy (cyFormType "comments_reply-form_cancel")
                        ]
                        [ text (gettext "Cancel" appState.locale) ]
                    ]
    in
    div [ class "CommentReplyForm", classList [ ( "CommentReplyForm--Private", private ) ] ]
        [ resizableTextarea 2
            commentValue
            [ class "form-control"
            , placeholder placeholderText
            , onInput (CommentInput path mbThreadUuid)
            , dataCy (cyFormType "comments_reply-form_input")
            ]
            []
        , newThreadFormSubmit
        ]


viewCommentDeleteOverlay : AppState -> { deleteMsg : Msg, deleteText : String, extraClass : String } -> Html Msg
viewCommentDeleteOverlay appState { deleteMsg, deleteText, extraClass } =
    div [ class "CommentDeleteOverlay", class extraClass ]
        [ div [ class "text-center" ]
            [ div [ class "mb-2" ] [ text deleteText ]
            , button
                [ class "btn btn-danger btn-sm me-2"
                , onClick deleteMsg
                , dataCy "comments_delete-modal_delete"
                ]
                [ text (gettext "Delete" appState.locale) ]
            , button [ class "btn btn-secondary btn-sm", onClick (CommentDelete Nothing) ] [ text (gettext "Cancel" appState.locale) ]
            ]
        ]



-- QUESTIONNAIRE -- CONTENT


viewQuestionnaireContent : AppState -> Config msg -> Context -> Model -> Html Msg
viewQuestionnaireContent appState cfg ctx model =
    let
        content =
            case model.activePage of
                PageChapter chapterUuid ->
                    case KnowledgeModel.getChapter chapterUuid model.questionnaire.knowledgeModel of
                        Just chapter ->
                            viewQuestionnaireContentChapter appState cfg ctx model chapter

                        Nothing ->
                            emptyNode

                _ ->
                    emptyNode
    in
    div [ class "questionnaire__content" ] [ content ]



-- QUESTIONNAIRE -- CONTENT -- CHAPTER


viewQuestionnaireContentChapter : AppState -> Config msg -> Context -> Model -> Chapter -> Html Msg
viewQuestionnaireContentChapter appState cfg ctx model chapter =
    let
        chapters =
            KnowledgeModel.getChapters model.questionnaire.knowledgeModel

        chapterNumber =
            chapters
                |> List.findIndex (.uuid >> (==) chapter.uuid)
                |> Maybe.unwrap "I" ((+) 1 >> Roman.toRomanNumber)

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid model.questionnaire.knowledgeModel

        questionViews =
            if List.isEmpty questions then
                let
                    emptyMessage =
                        if List.isEmpty model.questionnaire.selectedQuestionTagUuids then
                            gettext "This chapter contains no questions." appState.locale

                        else
                            gettext "There are no questions matching the selected question tags." appState.locale
                in
                div [ class "flex-grow-1" ]
                    [ Flash.info appState emptyMessage
                    ]

            else
                let
                    desirableQuestions =
                        List.filter (isQuestionDesirable model) questions
                in
                if not model.viewSettings.nonDesirableQuestions && List.isEmpty desirableQuestions then
                    div [ class "flex-grow-1" ]
                        [ Flash.info appState (gettext "There are no questions in this phase." appState.locale)
                        ]

                else
                    div [ class "flex-grow-1" ] <|
                        List.indexedMap (viewQuestion appState cfg ctx model [ chapter.uuid ] [ chapterNumber ]) questions
    in
    div [ class "questionnaire__form container" ]
        [ h2 [] [ text (chapterNumber ++ ". " ++ chapter.title) ]
        , Markdown.toHtml [ class "chapter-description" ] (Maybe.withDefault "" chapter.text)
        , questionViews
        , viewPrevAndNextChapterLinks appState chapters chapter
        ]


viewPrevAndNextChapterLinks : AppState -> List Chapter -> Chapter -> Html Msg
viewPrevAndNextChapterLinks appState chapters currentChapter =
    let
        findPrevChapter cs =
            case cs of
                prev :: current :: rest ->
                    if current.uuid == currentChapter.uuid then
                        Just prev

                    else
                        findPrevChapter (current :: rest)

                _ ->
                    Nothing

        findNextChapter cs =
            case cs of
                current :: next :: rest ->
                    if current.uuid == currentChapter.uuid then
                        Just next

                    else
                        findNextChapter (next :: rest)

                _ ->
                    Nothing

        viewChapterLink cls label icon c =
            div
                [ class ("rounded-3 py-3 chapter-link " ++ cls)
                , onClick (SetActivePage (PageChapter c.uuid))
                ]
                [ div [ class "text-lighter" ] [ text label ]
                , text c.title
                , icon
                ]

        viewPrevChapterLink =
            viewChapterLink "chapter-link-prev"
                (gettext "Previous Chapter" appState.locale)
                (faSet "_global.chevronLeft" appState)

        viewNextChapterLink =
            viewChapterLink "chapter-link-next"
                (gettext "Next Chapter" appState.locale)
                (faSet "_global.chevronRight" appState)

        prevChapterLink =
            findPrevChapter chapters
                |> Maybe.unwrap emptyNode viewPrevChapterLink

        nextChapterLink =
            findNextChapter chapters
                |> Maybe.unwrap emptyNode viewNextChapterLink
    in
    div [ class "mt-5 pt-3 pb-3 d-flex flex-gap-2" ]
        [ prevChapterLink, nextChapterLink ]


viewQuestion : AppState -> Config msg -> Context -> Model -> List String -> List String -> Int -> Question -> Html Msg
viewQuestion appState cfg ctx model path humanIdentifiers order question =
    let
        newHumanIdentifiers =
            humanIdentifiers ++ [ String.fromInt (order + 1) ]

        newPath =
            path ++ [ Question.getUuid question ]

        ( viewInput, viewExtensions ) =
            case question of
                OptionsQuestion _ _ ->
                    viewQuestionOptions appState cfg ctx model newPath newHumanIdentifiers question

                ListQuestion _ _ ->
                    ( viewQuestionList appState cfg ctx model newPath newHumanIdentifiers question, [] )

                ValueQuestion _ _ ->
                    ( viewQuestionValue appState cfg model newPath question, [] )

                IntegrationQuestion _ data ->
                    let
                        mbIntegration =
                            KnowledgeModel.getIntegration data.integrationUuid model.questionnaire.knowledgeModel
                    in
                    case mbIntegration of
                        Just (ApiIntegration commonIntegrationData apiIntegrationData) ->
                            ( viewQuestionIntegrationApi appState cfg model newPath commonIntegrationData apiIntegrationData question, [] )

                        Just (WidgetIntegration commonIntegrationData widgetIntegrationData) ->
                            ( viewQuestionIntegrationWidget appState cfg model newPath commonIntegrationData widgetIntegrationData, [] )

                        _ ->
                            ( emptyNode, [] )

                MultiChoiceQuestion _ _ ->
                    ( viewQuestionMultiChoice appState cfg model newPath question, [] )

                ItemSelectQuestion _ _ ->
                    ( viewQuestionItemSelect appState cfg model newPath question, [] )

                FileQuestion _ _ ->
                    ( viewQuestionFile appState cfg model newPath question, [] )

        ( questionClass, questionState ) =
            case
                ( QuestionnaireQuestionnaire.hasReply (pathToString newPath) model.questionnaire
                , isQuestionDesirable model question
                )
            of
                ( True, _ ) ->
                    ( "question-answered", Answered )

                ( _, True ) ->
                    ( "question-desirable", Desirable )

                _ ->
                    if model.viewSettings.nonDesirableQuestions then
                        ( "question-default", Default )

                    else
                        ( "question-hidden", Default )

        viewLabel =
            viewQuestionLabel appState cfg ctx model newPath newHumanIdentifiers question questionState

        viewTags =
            if model.viewSettings.tags then
                let
                    tags =
                        Question.getTagUuids question
                            |> List.map (flip KnowledgeModel.getTag model.questionnaire.knowledgeModel)
                            |> listFilterJust
                            |> List.sortBy .name
                in
                Tag.viewList { showDescription = False } tags

            else
                emptyNode

        viewDescription =
            cfg.renderer.renderQuestionDescription model.viewSettings question

        mbReply =
            Dict.get (pathToString newPath) model.questionnaire.replies

        viewAnsweredBy =
            case ( mbReply, model.viewSettings.answeredBy && not (Question.isList question) ) of
                ( Just reply, True ) ->
                    let
                        userName =
                            case reply.createdBy of
                                Just userInfo ->
                                    User.fullName userInfo

                                Nothing ->
                                    gettext "anonymous user" appState.locale

                        readableTime =
                            TimeUtils.toReadableDateTime appState.timeZone reply.createdAt

                        timeDiff =
                            Time.inWordsWithConfig { withAffix = True } (TimeDistance.locale appState) reply.createdAt appState.currentTime

                        time =
                            span (tooltip readableTime) [ text timeDiff ]
                    in
                    div [ class "mt-2", dataCy "questionnaire_answered-by" ]
                        (String.formatHtml (gettext "Answered %s by %s." appState.locale) [ time, text userName ])

                _ ->
                    emptyNode

        content =
            viewLabel :: viewTags :: viewDescription :: viewInput :: viewAnsweredBy :: viewExtensions

        questionExtraClass =
            Maybe.withDefault "" (cfg.renderer.getQuestionExtraClass question)
    in
    div
        [ class ("form-group " ++ questionClass ++ " " ++ questionExtraClass)
        , id ("question-" ++ Question.getUuid question)
        , attribute "data-path" (pathToString newPath)
        ]
        content


viewQuestionLabel : AppState -> Config msg -> Context -> Model -> List String -> List String -> Question -> QuestionViewState -> Html Msg
viewQuestionLabel appState cfg _ model path humanIdentifiers question questionState =
    let
        ( icon, tooltipText ) =
            case questionState of
                Answered ->
                    ( "fas fa-check", gettext "This question has been answered" appState.locale )

                Desirable ->
                    ( "fas fa-pen", gettext "This question should be answered now" appState.locale )

                Default ->
                    ( "far fa-hourglass", gettext "This question can be answered later" appState.locale )
    in
    label []
        [ span []
            [ Badge.secondary
                [ class "mb-1 me-2 py-1 px-2 fs-6"
                , classList
                    [ ( "bg-success", questionState == Answered )
                    , ( "bg-danger", questionState == Desirable )
                    ]
                ]
                [ span (tooltipRight tooltipText)
                    [ fa (icon ++ " fa-fw") ]
                , text (String.join "." humanIdentifiers)
                ]
            , span
                [ classList
                    [ ( "text-success", questionState == Answered )
                    , ( "text-danger", questionState == Desirable )
                    ]
                ]
                [ cfg.renderer.renderQuestionLabel question ]
            ]
        , span [ class "custom-actions" ]
            [ viewTodoAction appState cfg model path
            , viewCommentAction appState cfg model path
            , viewFeedbackAction appState cfg model question
            , viewCopyLinkAction appState cfg model path
            ]
        ]


viewQuestionOptions : AppState -> Config msg -> Context -> Model -> List String -> List String -> Question -> ( Html Msg, List (Html Msg) )
viewQuestionOptions appState cfg ctx model path humanIdentifiers question =
    let
        answers =
            KnowledgeModel.getQuestionAnswers (Question.getUuid question) model.questionnaire.knowledgeModel

        selectedAnswerUuid =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.map (.value >> ReplyValue.getAnswerUuid)

        mbSelectedAnswer =
            List.find (.uuid >> Just >> (==) selectedAnswerUuid) answers

        clearReplyButton =
            viewQuestionClearButton appState cfg path (Maybe.isJust mbSelectedAnswer)

        advice =
            Maybe.unwrap emptyNode cfg.renderer.renderAnswerAdvice mbSelectedAnswer

        followUps =
            Maybe.unwrap emptyNode
                (viewQuestionOptionsFollowUps appState cfg ctx model answers path humanIdentifiers)
                mbSelectedAnswer
    in
    ( div []
        (List.indexedMap (viewAnswer appState cfg model model.questionnaire.knowledgeModel path selectedAnswerUuid) answers
            ++ [ clearReplyButton ]
        )
    , [ advice, followUps ]
    )


viewQuestionClearButton : AppState -> Config msg -> List String -> Bool -> Html Msg
viewQuestionClearButton appState cfg path hasAnswer =
    if cfg.features.readonly || not hasAnswer then
        emptyNode

    else
        a [ class "clear-answer", onClick (ClearReply (pathToString path)) ]
            [ faSet "questionnaire.clearAnswer" appState
            , text (gettext "Clear answer" appState.locale)
            ]


viewQuestionOptionsFollowUps : AppState -> Config msg -> Context -> Model -> List Answer -> List String -> List String -> Answer -> Html Msg
viewQuestionOptionsFollowUps appState cfg ctx model answers path humanIdentifiers answer =
    let
        index =
            Maybe.unwrap "a" CharIdentifier.fromInt <|
                List.findIndex (.uuid >> (==) answer.uuid) answers

        newPath =
            path ++ [ answer.uuid ]

        newHumanIdentifier =
            humanIdentifiers ++ [ index ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid model.questionnaire.knowledgeModel

        followUpQuestions =
            List.indexedMap (viewQuestion appState cfg ctx model newPath newHumanIdentifier) questions
    in
    if List.isEmpty followUpQuestions then
        emptyNode

    else
        let
            desirableQuestions =
                List.filter (isQuestionDesirable model) questions
        in
        if not model.viewSettings.nonDesirableQuestions && List.isEmpty desirableQuestions then
            div [ class "followups-group" ]
                [ Flash.info appState (gettext "There are no follow up questions in this phase." appState.locale)
                ]

        else
            let
                pathString =
                    pathToString newPath
            in
            if Set.member pathString model.collapsedItems then
                let
                    followUpCount =
                        List.length followUpQuestions

                    expandButton =
                        a [ onClick (ExpandItem pathString) ]
                            [ faSet "questionnaire.item.expand" appState
                            , span [ class "ms-1" ] [ text (gettext "Expand" appState.locale) ]
                            ]
                in
                div [ class "followups-group followups-group-collapsed" ] <|
                    String.formatHtml
                        (ngettext ( "%s %s follow up question", "%s %s follow up questions" ) followUpCount appState.locale)
                        [ expandButton
                        , strong [] [ text (String.fromInt followUpCount) ]
                        ]

            else
                let
                    collapseButton =
                        a [ onClick (CollapseItem pathString) ]
                            [ faSet "questionnaire.item.collapse" appState
                            , span [ class "ms-1" ] [ text (gettext "Collapse" appState.locale) ]
                            ]
                in
                div [ class "followups-group" ]
                    (div [ class "mb-4" ] [ collapseButton ] :: followUpQuestions)


viewQuestionMultiChoice : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionMultiChoice appState cfg model path question =
    let
        choices =
            KnowledgeModel.getQuestionChoices (Question.getUuid question) model.questionnaire.knowledgeModel

        selectedChoicesUuids =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getChoiceUuid)

        clearReplyButton =
            viewQuestionClearButton appState cfg path (not (List.isEmpty selectedChoicesUuids))
    in
    div [] (List.indexedMap (viewChoice appState cfg path selectedChoicesUuids) choices ++ [ clearReplyButton ])


viewQuestionList : AppState -> Config msg -> Context -> Model -> List String -> List String -> Question -> Html Msg
viewQuestionList appState cfg ctx model path humanIdentifiers question =
    let
        expandAndCollapseButtons =
            let
                allItemsPaths =
                    List.map (\uuid -> pathToString (path ++ [ uuid ])) itemUuids
            in
            div [ class "mb-3" ]
                [ a [ onClick (ExpandItems allItemsPaths) ]
                    [ faSet "questionnaire.item.expandAll" appState
                    , span [ class "ms-1" ] [ text (gettext "Expand all" appState.locale) ]
                    ]
                , a
                    [ onClick (CollapseItems allItemsPaths)
                    , class "ms-3"
                    ]
                    [ faSet "questionnaire.item.collapseAll" appState
                    , span [ class "ms-1" ] [ text (gettext "Collapse all" appState.locale) ]
                    ]
                ]

        viewItem =
            viewQuestionListItem appState cfg ctx model question path humanIdentifiers (List.length itemUuids)

        itemUuids =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)

        noAnswersInfo =
            if cfg.features.readonly && List.isEmpty itemUuids then
                i [] [ text (gettext "There are no answers yet." appState.locale) ]

            else
                emptyNode
    in
    div []
        [ Html.viewIf (List.length itemUuids > 1) <| expandAndCollapseButtons
        , div [] (List.indexedMap viewItem itemUuids)
        , Html.viewIf (List.length itemUuids > 2) <| expandAndCollapseButtons
        , viewQuestionListAdd appState cfg itemUuids path
        , noAnswersInfo
        ]


viewQuestionListAdd : AppState -> Config msg -> List String -> List String -> Html Msg
viewQuestionListAdd appState cfg itemUuids path =
    if cfg.features.readonly then
        emptyNode

    else
        button
            [ class "btn btn-outline-secondary with-icon"
            , onClick (AddItem (pathToString path) itemUuids)
            ]
            [ faSet "_global.add" appState
            , text (gettext "Add" appState.locale)
            ]


viewQuestionListItem : AppState -> Config msg -> Context -> Model -> Question -> List String -> List String -> Int -> Int -> String -> Html Msg
viewQuestionListItem appState cfg ctx model question path humanIdentifiers itemCount index uuid =
    let
        itemPath =
            path ++ [ uuid ]

        itemPathString =
            pathToString itemPath

        isCollapsed =
            Set.member itemPathString model.collapsedItems

        questions =
            KnowledgeModel.getQuestionItemTemplateQuestions (Question.getUuid question) model.questionnaire.knowledgeModel

        itemQuestions =
            if isCollapsed then
                []

            else if List.isEmpty questions then
                [ Flash.info appState (gettext "This item contains no questions." appState.locale) ]

            else
                let
                    desirableQuestions =
                        List.filter (isQuestionDesirable model) questions
                in
                if not model.viewSettings.nonDesirableQuestions && List.isEmpty desirableQuestions then
                    [ Flash.info appState (gettext "There are no questions in this phase." appState.locale) ]

                else
                    let
                        newHumanIdentifiers =
                            humanIdentifiers ++ [ CharIdentifier.fromInt index ]
                    in
                    List.indexedMap (viewQuestion appState cfg ctx model itemPath newHumanIdentifiers) questions

        buttons =
            if cfg.features.readonly then
                []

            else
                let
                    deleteButton =
                        a
                            (class "btn-link text-danger"
                                :: onClick (RemoveItem (pathToString path) uuid)
                                :: dataCy "item-delete"
                                :: tooltip (gettext "Delete" appState.locale)
                            )
                            [ faSet "_global.delete" appState ]

                    moveUpButton =
                        if index == 0 then
                            emptyNode

                        else
                            a
                                (class "btn-link me-2"
                                    :: onClick (MoveItemUp (pathToString path) uuid)
                                    :: dataCy "item-move-up"
                                    :: tooltip (gettext "Move Up" appState.locale)
                                )
                                [ faSet "questionnaire.item.moveUp" appState ]

                    moveDownButton =
                        if index == itemCount - 1 then
                            emptyNode

                        else
                            a
                                (class "btn-link me-2"
                                    :: onClick (MoveItemDown (pathToString path) uuid)
                                    :: dataCy "item-move-down"
                                    :: tooltip (gettext "Move Down" appState.locale)
                                )
                                [ faSet "questionnaire.item.moveDown" appState ]
                in
                [ moveUpButton, moveDownButton, deleteButton ]

        itemTitle =
            if isCollapsed then
                Maybe.unwrap
                    (i [ class "ms-2" ] [ text (String.format (gettext "Item %s" appState.locale) [ String.fromInt (index + 1) ]) ])
                    (strong [ class "ms-2" ] << List.singleton << text)
                    (QuestionnaireQuestionnaire.getItemTitle model.questionnaire itemPath questions)

            else
                emptyNode

        collapseButton =
            if isCollapsed then
                a [ onClick (ExpandItem itemPathString), dataCy "item-expand" ] [ faSet "questionnaire.item.expand" appState ]

            else
                a [ onClick (CollapseItem itemPathString), dataCy "item-collapse" ] [ faSet "questionnaire.item.collapse" appState ]

        itemHeader =
            div [ class "item-header d-flex justify-content-between" ]
                [ div [] [ collapseButton, itemTitle ]
                , div [] buttons
                ]
    in
    div
        [ class "item mb-3"
        , classList [ ( "item-collapsed", isCollapsed ) ]
        , attribute "data-path" (pathToString itemPath)
        ]
        [ div [ class "card bg-light" ]
            [ div [ class "card-body" ]
                (itemHeader :: itemQuestions)
            ]
        ]


viewQuestionValue : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionValue appState cfg model path question =
    let
        defaultValue =
            if Question.getValueType question == Just ColorQuestionValueType then
                "#000000"

            else
                ""

        mbAnswer =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.map (.value >> ReplyValue.getStringReply)

        answer =
            Maybe.withDefault defaultValue mbAnswer

        defaultAttrs =
            [ class "form-control", value answer ]

        toMsg =
            SetReply (pathToString path) << createReply appState << StringReply

        extraAttrs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onInput toMsg ]

        warningView regex warning =
            if not (String.isEmpty answer) && not (Regex.contains regex answer) then
                Flash.warning appState warning

            else
                emptyNode

        defaultInput =
            [ input (type_ "text" :: defaultAttrs ++ extraAttrs) [] ]

        readonlyOr otherInput =
            if cfg.features.readonly then
                defaultInput

            else
                otherInput

        inputView =
            case Question.getValueType question of
                Just NumberQuestionValueType ->
                    [ input (type_ "number" :: defaultAttrs ++ extraAttrs) [] ]

                Just DateQuestionValueType ->
                    readonlyOr [ DatePicker.datePicker [ DatePicker.onChange toMsg, DatePicker.value answer ] ]

                Just DateTimeQuestionValueType ->
                    readonlyOr [ DatePicker.dateTimePicker [ DatePicker.onChange toMsg, DatePicker.value answer ] ]

                Just TimeQuestionValueType ->
                    readonlyOr [ DatePicker.timePicker [ DatePicker.onChange toMsg, DatePicker.value answer ] ]

                Just EmailQuestionValueType ->
                    [ input (type_ "email" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.email (gettext "This is not a valid email address." appState.locale)
                    ]

                Just UrlQuestionValueType ->
                    [ input (type_ "text" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.url (gettext "This is not a valid URL." appState.locale)
                    ]

                Just TextQuestionValueType ->
                    [ resizableTextarea 3 answer (defaultAttrs ++ extraAttrs ++ grammarlyAttributes) [] ]

                Just ColorQuestionValueType ->
                    [ input (type_ "color" :: defaultAttrs ++ extraAttrs) []
                    , warningView RegexPatterns.color (gettext "This is not a valid color." appState.locale)
                    ]

                _ ->
                    defaultInput

        validationWarning validation =
            case QuestionValidation.validate appState validation answer of
                Ok _ ->
                    emptyNode

                Err error ->
                    Flash.warning appState error

        validationWarnings =
            case ( Question.getValidations question, mbAnswer ) of
                ( Just validations, Just _ ) ->
                    List.map validationWarning validations

                _ ->
                    []

        clearReplyButton =
            viewQuestionClearButton appState cfg path (Maybe.isJust mbAnswer)
    in
    div [] (inputView ++ validationWarnings ++ [ clearReplyButton ])


viewQuestionIntegrationWidget : AppState -> Config msg -> Model -> List String -> CommonIntegrationData -> WidgetIntegrationData -> Html Msg
viewQuestionIntegrationWidget appState cfg model path commonIntegrationData widgetIntegrationData =
    let
        mbReplyValue =
            Maybe.map .value <|
                Dict.get (pathToString path) model.questionnaire.replies

        questionInput =
            case mbReplyValue of
                Just (IntegrationReply (IntegrationType id integrationValue)) ->
                    viewQuestionIntegrationIntegrationReply commonIntegrationData id integrationValue

                _ ->
                    viewQuestionIntegrationWidgetSelectButton appState cfg path widgetIntegrationData mbReplyValue
    in
    div [ class "question-integration-answer" ]
        [ questionInput
        , viewQuestionClearButton appState cfg path (Maybe.isJust mbReplyValue)
        ]


viewQuestionIntegrationWidgetSelectButton : AppState -> Config msg -> List String -> WidgetIntegrationData -> Maybe ReplyValue -> Html Msg
viewQuestionIntegrationWidgetSelectButton appState cfg path widgetIntegrationData mbReplyValue =
    case ( cfg.features.readonly, Maybe.isJust mbReplyValue ) of
        ( False, False ) ->
            button
                [ onClick (OpenIntegrationWidget (pathToString path) widgetIntegrationData.widgetUrl)
                , class "btn btn-secondary"
                ]
                [ text (gettext "Select" appState.locale) ]

        _ ->
            emptyNode


viewQuestionIntegrationApi : AppState -> Config msg -> Model -> List String -> CommonIntegrationData -> ApiIntegrationData -> Question -> Html Msg
viewQuestionIntegrationApi appState cfg model path commonIntegrationData apiIntegrationData question =
    let
        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                let
                    questionValue =
                        Maybe.unwrap "" ReplyValue.getStringReply mbReplyValue

                    onFocusHandler =
                        [ onFocus (ShowTypeHints path apiIntegrationData.requestEmptySearch (Question.getUuid question) questionValue) ]
                in
                [ onInput (TypeHintInput path apiIntegrationData.requestEmptySearch << createReply appState << IntegrationReply << PlainType)
                , onBlur HideTypeHints
                ]
                    ++ onFocusHandler

        mbReplyValue =
            Maybe.map .value <|
                Dict.get (pathToString path) model.questionnaire.replies

        viewInput currentValue =
            input ([ class "form-control", type_ "text", value currentValue ] ++ extraArgs) []

        questionInput =
            case mbReplyValue of
                Just (IntegrationReply integrationReply) ->
                    case integrationReply of
                        PlainType plainValue ->
                            viewInput plainValue

                        IntegrationType id integrationValue ->
                            viewQuestionIntegrationIntegrationReply commonIntegrationData id integrationValue

                _ ->
                    viewInput ""

        typeHintsVisible =
            Maybe.unwrap False (.path >> (==) path) model.typeHints

        viewTypeHints =
            if typeHintsVisible then
                viewQuestionIntegrationTypeHints appState cfg model path

            else
                emptyNode
    in
    div [ class "question-integration-answer" ]
        [ questionInput
        , viewTypeHints
        , viewQuestionClearButton appState cfg path (Maybe.isJust mbReplyValue)
        ]


viewQuestionIntegrationTypeHints : AppState -> Config msg -> Model -> List String -> Html Msg
viewQuestionIntegrationTypeHints appState cfg model path =
    let
        content =
            case Maybe.unwrap Unset .hints model.typeHints of
                Success [] ->
                    div [ class "info" ]
                        [ faSet "_global.info" appState
                        , text (gettext "There are no results for your search." appState.locale)
                        ]

                Success hints ->
                    ul [ class "integration-typehints-list" ] (List.map (viewQuestionIntegrationTypeHint appState cfg path) hints)

                Loading ->
                    div [ class "loading" ]
                        [ faSet "_global.spinner" appState
                        , text (gettext "Loading..." appState.locale)
                        ]

                Error err ->
                    div [ class "error" ]
                        [ faSet "_global.error" appState
                        , text err
                        ]

                Unset ->
                    emptyNode
    in
    div [ class "integration-typehints" ] [ content ]


viewQuestionIntegrationTypeHint : AppState -> Config msg -> List String -> TypeHint -> Html Msg
viewQuestionIntegrationTypeHint appState cfg path typeHint =
    if cfg.features.readonly then
        emptyNode

    else
        li
            [ class "integration-typehints-list-item"
            , onMouseDown <| SetReply (pathToString path) <| createReply appState <| IntegrationReply <| IntegrationType typeHint.id typeHint.name
            ]
            [ Markdown.toHtml [ class "item-md" ] typeHint.name
            ]


viewQuestionIntegrationIntegrationReply : CommonIntegrationData -> Maybe String -> String -> Html Msg
viewQuestionIntegrationIntegrationReply integration id value =
    div [ class "card" ]
        [ Markdown.toHtml [ class "card-body item-md" ] value
        , viewQuestionIntegrationLink integration id
        ]


viewQuestionIntegrationLink : CommonIntegrationData -> Maybe String -> Html Msg
viewQuestionIntegrationLink integration mbId =
    case ( integration.itemUrl, mbId ) of
        ( Just itemUrl, Just id ) ->
            let
                url =
                    String.replace "${id}" id itemUrl

                logo =
                    case Maybe.andThen String.toMaybe integration.logo of
                        Just logoUrl ->
                            img [ src logoUrl ] []

                        Nothing ->
                            emptyNode
            in
            div [ class "card-footer" ]
                [ logo
                , a [ href url, target "_blank" ] [ text url ]
                ]

        _ ->
            emptyNode


viewQuestionItemSelect : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionItemSelect appState cfg model path question =
    let
        mbSelectedItem =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.map (.value >> ReplyValue.getSelectedItemUuid)

        extraAttrs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onChange (SetReply (pathToString path) << createReply appState << ItemSelectReply) ]

        mbListQuestionUuid =
            Question.getListQuestionUuid question

        mbItemQuestionUuid =
            mbListQuestionUuid
                |> Maybe.andThen (\uuid -> KnowledgeModel.getQuestion uuid model.questionnaire.knowledgeModel)
                |> Maybe.map Question.getUuid

        items =
            case mbItemQuestionUuid of
                Just itemQuestionUuid ->
                    let
                        itemTemplateQuestions =
                            KnowledgeModel.getQuestionItemTemplateQuestions itemQuestionUuid model.questionnaire.knowledgeModel

                        itemsToOptions ( itemQuestionPath, reply ) =
                            ReplyValue.getItemUuids reply.value
                                |> List.indexedMap
                                    (\i itemUuid ->
                                        ( itemUuid
                                        , QuestionnaireQuestionnaire.getItemTitle model.questionnaire (String.split "." itemQuestionPath ++ [ itemUuid ]) itemTemplateQuestions
                                            |> Maybe.withDefault (String.format (gettext "Item %s" appState.locale) [ String.fromInt (i + 1) ])
                                        )
                                    )
                    in
                    model.questionnaire.replies
                        |> Dict.filter (\key _ -> String.endsWith itemQuestionUuid key)
                        |> Dict.toList
                        |> List.concatMap itemsToOptions

                Nothing ->
                    []

        itemToOption ( optionValue, optionLabel ) =
            option [ value optionValue, selected (Just optionValue == mbSelectedItem) ]
                [ text optionLabel ]

        itemMissing =
            QuestionnaireQuestionnaire.itemSelectQuestionItemMissing model.questionnaire mbListQuestionUuid (pathToString path)

        options =
            List.map itemToOption items

        optionsWithSelect =
            if Maybe.isJust mbSelectedItem && not itemMissing then
                options

            else
                itemToOption ( "", gettext "- select -" appState.locale ) :: options

        itemLink =
            case QuestionnaireQuestionnaire.itemSelectQuestionItemPath model.questionnaire mbListQuestionUuid (pathToString path) of
                Just itemPath ->
                    div [ class "question-item-select-link" ]
                        [ a [ onClick (ScrollToPath itemPath) ]
                            [ text (gettext "Go to item" appState.locale)
                            , fa "fas fa-arrow-right ms-1"
                            ]
                        ]

                Nothing ->
                    emptyNode

        clearReplyButton =
            viewQuestionClearButton appState cfg path (Maybe.isJust mbSelectedItem)

        missingItemWarning =
            if itemMissing then
                Flash.warning appState (gettext "The selected item was deleted." appState.locale)

            else
                emptyNode
    in
    div [ class "question-item-select" ]
        [ select (class "form-control" :: extraAttrs) optionsWithSelect
        , itemLink
        , clearReplyButton
        , missingItemWarning
        ]


viewQuestionFile : AppState -> Config msg -> Model -> List String -> Question -> Html Msg
viewQuestionFile appState cfg model path question =
    let
        mbAnswer =
            Dict.get (pathToString path) model.questionnaire.replies
                |> Maybe.map .value
                |> Maybe.andThen ReplyValue.getFileUuid

        fileView fileUuid =
            case QuestionnaireQuestionnaire.getFile model.questionnaire fileUuid of
                Just file ->
                    div [ class "questionnaire-file" ]
                        [ fa ("me-2 " ++ FileIcon.getFileIcon file.fileName file.contentType)
                        , a [ onClick (DownloadFile file.uuid), class "text-truncate" ] [ text file.fileName ]
                        , span [ class "text-muted ms-2 text-nowrap" ]
                            [ text ("(" ++ (ByteUnits.toReadable file.fileSize ++ ")")) ]
                        , Html.viewIf (not cfg.features.readonly) <|
                            div [ class "flex-grow-1 text-end" ]
                                [ a
                                    (onClick (DeleteFile fileUuid (pathToString path))
                                        :: dataCy "file-delete"
                                        :: class "btn-link text-danger ms-2 d-block"
                                        :: tooltip (gettext "Delete" appState.locale)
                                    )
                                    [ faSet "_global.delete" appState ]
                                ]
                        ]

                Nothing ->
                    div []
                        [ Flash.warning appState (gettext "The file was deleted." appState.locale)
                        , viewQuestionClearButton appState cfg path True
                        ]

        questionContent =
            case mbAnswer of
                Just fileUuid ->
                    fileView fileUuid

                Nothing ->
                    let
                        fileConfig =
                            { fileTypes = Question.getFileTypes question
                            , maxSize = Question.getMaxSize question
                            }
                    in
                    div []
                        [ button
                            [ class "btn btn-outline-primary"
                            , onClick (FileUploadModalMsg (FileUploadModal.open (pathToString path) fileConfig))
                            , disabled cfg.features.readonly
                            , dataCy "file-upload"
                            ]
                            [ text (gettext "Upload File" appState.locale) ]
                        ]
    in
    div [] [ questionContent ]


viewChoice : AppState -> Config msg -> List String -> List String -> Int -> Choice -> Html Msg
viewChoice appState cfg path selectedChoicesUuids order choice =
    let
        checkboxName =
            pathToString (path ++ [ choice.uuid ])

        humanIdentifier =
            CharIdentifier.fromInt order ++ ". "

        isSelected =
            List.member choice.uuid selectedChoicesUuids

        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                let
                    newSelectedUuids =
                        if isSelected then
                            List.filter ((/=) choice.uuid) selectedChoicesUuids

                        else
                            choice.uuid :: selectedChoicesUuids
                in
                [ onClick (SetReply (pathToString path) (createReply appState (MultiChoiceReply newSelectedUuids))) ]
    in
    div
        [ class "radio"
        , classList [ ( "radio-selected", isSelected ), ( "radio-disabled", cfg.features.readonly ) ]
        ]
        [ label []
            [ input ([ type_ "checkbox", name checkboxName, checked isSelected ] ++ extraArgs) []
            , text humanIdentifier
            , cfg.renderer.renderChoiceLabel choice
            ]
        ]


viewAnswer : AppState -> Config msg -> Model -> KnowledgeModel -> List String -> Maybe String -> Int -> Answer -> Html Msg
viewAnswer appState cfg model km path selectedAnswerUuid order answer =
    let
        radioName =
            pathToString (path ++ [ answer.uuid ])

        humanIdentifier =
            CharIdentifier.fromInt order ++ ". "

        extraArgs =
            if cfg.features.readonly then
                [ disabled True ]

            else
                [ onClick (SetReply (pathToString path) (createReply appState (AnswerReply answer.uuid))) ]

        followUpsIndicator =
            if List.isEmpty (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km) then
                emptyNode

            else
                span (class "ms-3 text-muted" :: tooltipRight (gettext "This option leads to some follow up questions." appState.locale))
                    [ i
                        [ class (faKeyClass "questionnaire.followUpsIndication" appState)
                        ]
                        []
                    ]

        isSelected =
            selectedAnswerUuid == Just answer.uuid
    in
    div
        [ class "radio"
        , classList [ ( "radio-selected", isSelected ), ( "radio-disabled", cfg.features.readonly ) ]
        ]
        [ label []
            [ input ([ type_ "radio", name radioName, checked isSelected ] ++ extraArgs) []
            , text humanIdentifier
            , cfg.renderer.renderAnswerLabel answer
            , followUpsIndicator
            , cfg.renderer.renderAnswerBadges model.viewSettings.metricValues answer
            ]
        ]


viewCommentAction : AppState -> Config msg -> Model -> List String -> Html Msg
viewCommentAction appState cfg model path =
    if cfg.features.commentsEnabled && Feature.projectCommentAdd appState model.questionnaire then
        let
            pathString =
                pathToString path

            commentCount =
                QuestionnaireQuestionnaire.getUnresolvedCommentCount pathString model.questionnaire

            isOpen =
                case model.rightPanel of
                    RightPanel.Comments rightPanelPath ->
                        rightPanelPath == pathString

                    _ ->
                        False

            msg =
                if isOpen then
                    SetRightPanel RightPanel.None

                else
                    SetRightPanel (RightPanel.Comments pathString)
        in
        if commentCount > 0 then
            a
                [ class "action action-comments"
                , classList [ ( "action-comments-open", isOpen ) ]
                , onClick msg
                , dataCy "questionnaire_question-action_comment"
                ]
                [ faSet "questionnaire.comments" appState
                , text <| String.format (ngettext ( "1 comment", "%s comments" ) commentCount appState.locale) [ String.fromInt commentCount ]
                ]

        else
            a
                (class "action"
                    :: classList [ ( "action-comments-open", isOpen ) ]
                    :: onClick msg
                    :: dataCy "questionnaire_question-action_comment"
                    :: tooltip (gettext "Comments" appState.locale)
                )
                [ faSet "questionnaire.comments" appState ]

    else
        emptyNode


viewTodoAction : AppState -> Config msg -> Model -> List String -> Html Msg
viewTodoAction appState cfg model path =
    if cfg.features.todosEnabled then
        let
            currentPath =
                pathToString path

            hasTodo =
                model.questionnaire.labels
                    |> Dict.get currentPath
                    |> Maybe.unwrap False (List.member QuestionnaireQuestionnaire.todoUuid)
        in
        if hasTodo then
            a
                [ class "action action-todo"
                , onClick (SetLabels currentPath [])
                ]
                [ span [] [ text (gettext "TODO" appState.locale) ]
                , a (class "text-danger" :: tooltip (gettext "Remove TODO" appState.locale))
                    [ faSet "_global.remove" appState ]
                ]

        else
            a
                [ class "action action-add-todo"
                , onClick <| SetLabels currentPath [ QuestionnaireQuestionnaire.todoUuid ]
                ]
                [ faSet "_global.add" appState
                , span [] [ span [] [ text (gettext "Add TODO" appState.locale) ] ]
                ]

    else
        emptyNode


viewFeedbackAction : AppState -> Config msg -> Model -> Question -> Html Msg
viewFeedbackAction appState cfg model question =
    let
        feedbackEnabled =
            appState.config.questionnaire.feedback.enabled && cfg.features.feedbackEnabled
    in
    if feedbackEnabled then
        let
            openFeedbackModal =
                FeedbackModalMsg (FeedbackModal.OpenFeedback model.questionnaire.packageId (Question.getUuid question))
        in
        a
            (class "action"
                :: attribute "data-cy" "feedback"
                :: onClick openFeedbackModal
                :: tooltip (gettext "Feedback" appState.locale)
            )
            [ faSet "questionnaire.feedback" appState ]

    else
        emptyNode


viewCopyLinkAction : AppState -> Config msg -> Model -> List String -> Html Msg
viewCopyLinkAction appState cfg model path =
    if cfg.features.questionLinksEnabled then
        let
            copyText =
                if model.recentlyCopied then
                    gettext "Copied!" appState.locale

                else
                    gettext "Copy link" appState.locale
        in
        a (class "action" :: onClick (CopyLinkToQuestion path) :: onMouseOut ClearRecentlyCopied :: tooltipLeft copyText)
            [ faSet "questionnaire.copyLink" appState ]

    else
        emptyNode


viewRemoveItemModal : AppState -> Model -> Html Msg
viewRemoveItemModal appState model =
    let
        removeItemUuid =
            Maybe.map Tuple.second model.removeItem

        mapItem ( path, reply ) =
            case String.toMaybe (ReplyValue.getSelectedItemUuid reply.value) of
                Just selectedItemUuid ->
                    if Just selectedItemUuid == removeItemUuid then
                        String.split "." path
                            |> List.last
                            |> Maybe.andThen (flip KnowledgeModel.getQuestion model.questionnaire.knowledgeModel)
                            |> Maybe.map (\q -> ( path, Question.getTitle q ))

                    else
                        Nothing

                Nothing ->
                    Nothing

        viewLink ( path, label ) =
            li []
                [ a [ onClick (ScrollToPath path) ]
                    [ text label ]
                ]

        wrapItemLinks links =
            if List.isEmpty links then
                emptyNode

            else
                p [ class "mt-3" ]
                    [ text (gettext "There are some item select questions using this item:" appState.locale)
                    , ul [] links
                    ]

        items =
            Dict.toList model.questionnaire.replies
                |> List.filterMap mapItem
                |> List.map viewLink
                |> wrapItemLinks

        cfg =
            { modalTitle = gettext "Remove Item" appState.locale
            , modalContent =
                [ text (gettext "Are you sure you want to remove this item?" appState.locale)
                , items
                ]
            , visible = Maybe.isJust model.removeItem
            , actionResult = Unset
            , actionName = gettext "Remove" appState.locale
            , actionMsg = RemoveItemConfirm
            , cancelMsg = Just RemoveItemCancel
            , dangerous = True
            , dataCy = "remove-item"
            }
    in
    Modal.confirm appState cfg


viewFileDeleteModal : AppState -> Model -> Html Msg
viewFileDeleteModal appState model =
    let
        fileName =
            case model.deleteFile of
                Just ( fileUuid, _ ) ->
                    case QuestionnaireQuestionnaire.getFile model.questionnaire fileUuid of
                        Just file ->
                            file.fileName

                        Nothing ->
                            ""

                Nothing ->
                    ""

        cfg =
            { modalTitle = gettext "Delete File" appState.locale
            , modalContent =
                String.formatHtml (gettext "Are you sure you want to delete %s?" appState.locale)
                    [ strong [ class "text-break" ] [ text fileName ] ]
            , visible = Maybe.isJust model.deleteFile
            , actionResult = ActionResult.map (always "") model.deletingFile
            , actionName = gettext "Delete" appState.locale
            , actionMsg = DeleteFileConfirm
            , cancelMsg = Just DeleteFileCancel
            , dangerous = True
            , dataCy = "delete-file"
            }
    in
    Modal.confirm appState cfg



-- UTILS


pathToString : List String -> String
pathToString =
    String.join "."


createReply : AppState -> ReplyValue -> Reply
createReply appState value =
    { value = value
    , createdAt = appState.currentTime
    , createdBy = Maybe.map UserInfo.toUserSuggestion appState.config.user
    }


actionsAvailable : AppState -> Config a -> Model -> Bool
actionsAvailable appState cfg model =
    Session.exists appState.session
        && not cfg.features.readonly
        && model.questionnaire.questionnaireActionsAvailable
        > 0


importersAvailable : AppState -> Config a -> Model -> Bool
importersAvailable appState cfg model =
    Session.exists appState.session
        && not cfg.features.readonly
        && model.questionnaire.questionnaireImportersAvailable
        > 0
