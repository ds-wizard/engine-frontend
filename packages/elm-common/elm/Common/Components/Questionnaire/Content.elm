module Common.Components.Questionnaire.Content exposing
    ( Model
    , Structure
    , UpdateConfig
    , ViewState
    , chapterId
    , init
    , structure
    , subscriptions
    , update
    , view
    )

{-| The questionnaire's content: the chapter as a flat, keyed list of lazy nodes,
and what those nodes send back.

**The hot path.** `view` is called on every render the questionnaire's content is
part of, and it is small on purpose: for each node it reads the node's reply and
counts out of the questionnaire, turns them into the primitives the node's lazy
function compares, and hands over. Nothing else — every lookup that does not
change between renders was done by `Nodes.build`. See `Question` for what the
lazy functions may and may not be given.

Three things are cheaper than in the wizard:

  - A copied link marks its own node (`Flags.withCopiedLink`), not every node.
  - An open plugin panel changes the argument of its own node only
    (`ContentMsg.QuestionActions`).
  - The options of an item select are worked out once per render for each list
    question that is selected from, rather than by scanning every reply once per
    item select question.

**The list.** The chapter's heading and text come before the list and its links
after it, so the list holds only its items — which is what `role="list"` promises
a screen reader. The list is named by the chapter's heading.

-}

import ActionResult
import Browser.Dom as BrowserDom
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Integration as Integration
import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Common.Api.Models.ProjectFileSimple as ProjectFileSimple
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Models.TypeHintRequest as TypeHintRequest
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Api.ProjectFiles as ProjectFilesApi
import Common.Api.TypeHints as TypeHintsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Markdown as Markdown
import Common.Components.Questionnaire.ContentMsg exposing (FileConfig, ItemEventData, Msg(..), PluginQuestionActionData, QuestionActions, TypeHints)
import Common.Components.Questionnaire.Flags as Flags exposing (Flags)
import Common.Components.Questionnaire.Item as Item
import Common.Components.Questionnaire.Nest as Nest
import Common.Components.Questionnaire.Nodes exposing (ChapterLinksNodeData, ChapterNodeData, ContentNode(..), IntegrationAnswering(..), IntegrationPlugin, QuestionNodeData, QuestionSpecificNodeData(..))
import Common.Components.Questionnaire.Question as Question
import Common.Components.Questionnaire.ReplyString as ReplyString
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.BuiltInIntegration as BuiltInIntegration exposing (BuiltInIntegration)
import Common.Plugins.Plugin exposing (Plugin)
import Common.Plugins.PluginElement as PluginElement exposing (PluginElement)
import Common.Ports.Copy as Copy
import Common.Ports.Dom as Dom
import Common.Ports.File as File
import Common.Routes as Routes
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, div, h2, li, span, text, ul)
import Html.Attributes exposing (attribute, class, id, type_)
import Html.Events
import Html.Keyed
import Html.Lazy as Lazy
import Json.Decode as D
import Json.Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Process
import Random exposing (Seed)
import Set exposing (Set)
import String.Format as String
import Task
import Task.Extra as Task
import Time
import Ui.Alert as Alert
import Ui.Icon as Icon
import Ui.Menu as Menu
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid



-- MODEL


type alias Model =
    { removeItem : Maybe ItemEventData

    -- One item's menu of the item select questions using it, by the item's uuid.
    , itemUsagesMenu : Maybe ( String, Menu.Model )
    , typeHints : Maybe TypeHints
    , typeHintsDebounce : Debounce ( String, String, String )
    , typeHintsNothing : Maybe TypeHints
    , copiedLinkPath : Maybe String
    , questionActions : QuestionActions
    , integrationPlugins : List IntegrationPlugin

    -- The integrations the client has built in, answered from hints. Both
    -- lists are read once from the app state, for `Nodes.build`.
    , builtIns : List BuiltInIntegration
    }


init : AppState -> List ( Plugin, Common.Plugins.Plugin.ProjectQuestionActionConnector ) -> Model
init appState questionActionPlugins =
    { removeItem = Nothing
    , itemUsagesMenu = Nothing
    , typeHints = Nothing
    , typeHintsDebounce = Debounce.init
    , typeHintsNothing = Nothing
    , copiedLinkPath = Nothing
    , questionActions = { plugins = questionActionPlugins, open = Nothing }
    , integrationPlugins =
        AppState.getPluginsByConnector appState .knowledgeModelIntegrations
            |> List.map
                (\( plugin, connector ) ->
                    { plugin = plugin
                    , connector = connector
                    , settings = AppState.getPluginSettings appState plugin.uuid
                    , userSettings = AppState.getPluginUserSettings appState plugin.uuid
                    , user = AppState.getPluginUser appState
                    }
                )
    , builtIns = appState.extensions.integrations
    }


{-| A chapter's nodes, split once when they are built: the heading before the
list, whether the chapter shows nothing, the list's nodes, and the links after it.
-}
type alias Structure =
    { chapter : Maybe ChapterNodeData
    , chapterEmpty : Bool
    , nodes : List ContentNode
    , links : Maybe ChapterLinksNodeData
    }


structure : List ContentNode -> Structure
structure contentNodes =
    List.foldr
        (\node acc ->
            case node of
                ChapterNode data ->
                    { acc | chapter = Just data }

                ChapterEmptyNode ->
                    { acc | chapterEmpty = True }

                ChapterLinksNode data ->
                    { acc | links = Just data }

                _ ->
                    { acc | nodes = node :: acc.nodes }
        )
        { chapter = Nothing, chapterEmpty = False, nodes = [], links = Nothing }
        contentNodes



-- UPDATE


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , questionnaire : ProjectQuestionnaire
    , knowledgeModelParentMap : KnowledgeModel.ParentMap
    , collapsedPaths : Set String
    , mbKmEditorUuid : Maybe Uuid
    , openChapterCmd : String -> Cmd msg
    , scrollToPathCmd : Bool -> String -> Cmd msg
    , updateCollapsedPathsCmd : Set String -> Cmd msg
    , addTodoCmd : String -> Cmd msg
    , removeTodoCmd : String -> Cmd msg
    , openCommentsCmd : String -> Cmd msg
    , closeRightPanelCmd : Cmd msg
    , openFileUploadCmd : String -> FileConfig -> Cmd msg
    , deleteFileCmd : Uuid -> String -> String -> Cmd msg
    , openPluginQuestionActionModalCmd : Uuid -> PluginElement -> Question.Question -> String -> Cmd msg
    , openPluginQuestionActionRightPanelCmd : PluginQuestionActionData -> Cmd msg
    }


{-| What the content returns: its model, the seed it used, a command, and an event
when the user changed the questionnaire — which the caller applies and, on the
project page, sends.
-}
update : AppState -> UpdateConfig msg -> Msg -> Model -> { seed : Seed, model : Model, cmd : Cmd msg, event : Maybe ProjectEvent }
update appState cfg msg model =
    let
        plain newModel cmd =
            { seed = appState.seed, model = newModel, cmd = cmd, event = Nothing }

        withEvent newModel cmd toEvent =
            let
                ( uuid, seed ) =
                    Uuid.step appState.seed
            in
            { seed = seed, model = newModel, cmd = cmd, event = Just (toEvent uuid) }

        setReply path value uuid =
            ProjectEvent.SetReply { uuid = uuid, path = path, value = value, createdAt = appState.currentTime, createdBy = createdBy appState }
    in
    case msg of
        OpenChapter chapterUuid ->
            plain model (cfg.openChapterCmd chapterUuid)

        CollapsePaths paths ->
            plain model (cfg.updateCollapsedPathsCmd (List.foldl Set.insert cfg.collapsedPaths paths))

        ExpandPaths paths ->
            plain model (cfg.updateCollapsedPathsCmd (List.foldl Set.remove cfg.collapsedPaths paths))

        ScrollToPath path ->
            plain { model | itemUsagesMenu = Nothing } (cfg.scrollToPathCmd False path)

        ScrollToQuestion questionUuid ->
            case ProjectQuestionnaire.getClosestQuestionParentPath cfg.questionnaire cfg.knowledgeModelParentMap questionUuid of
                Just path ->
                    plain model (cfg.scrollToPathCmd True path)

                Nothing ->
                    plain model Cmd.none

        SetReply path value ->
            withEvent model Cmd.none (setReply path value)

        ClearReply path ->
            withEvent model Cmd.none (\uuid -> ProjectEvent.ClearReply { uuid = uuid, path = path, createdAt = appState.currentTime, createdBy = createdBy appState })

        AddItem path itemUuids ->
            let
                ( uuid, seed ) =
                    Uuid.step appState.seed

                itemPath =
                    path ++ "." ++ Uuid.toString uuid
            in
            -- The new item's header is its own path, with no `-header`
            -- suffix, which no element carries. Focus goes to its header's
            -- toggle, so the keyboard is where the new item is.
            { seed = seed
            , model = model
            , cmd = Dom.scrollToAndFocus { selector = "#" ++ Nest.titleId itemPath, focus = "#" ++ Item.toggleId itemPath, smooth = True }
            , event = Just (setReply path (ItemListReply (itemUuids ++ [ Uuid.toString uuid ])) uuid)
            }

        RemoveItemOpen item ->
            plain { model | removeItem = Just item } Cmd.none

        RemoveItemConfirm ->
            case model.removeItem of
                Just item ->
                    withEvent { model | removeItem = Nothing } Cmd.none (setReply item.path (ItemListReply (List.filter ((/=) item.itemUuid) item.itemUuids)))

                Nothing ->
                    plain model Cmd.none

        RemoveItemCancel ->
            plain { model | removeItem = Nothing } Cmd.none

        MoveItemUp item ->
            withEvent model Cmd.none (setReply item.path (ItemListReply (moveItem -1 item)))

        MoveItemDown item ->
            withEvent model Cmd.none (setReply item.path (ItemListReply (moveItem 1 item)))

        ItemUsagesMenuMsg itemUuid menuMsg ->
            let
                ( menu, menuCmd ) =
                    Menu.update (Item.usagesMenuId itemUuid)
                        (List.length (ProjectQuestionnaire.getItemUsageInItemSelectQuestions cfg.questionnaire itemUuid))
                        menuMsg
                        (itemUsagesMenu itemUuid model)
            in
            plain { model | itemUsagesMenu = Just ( itemUuid, menu ) } (Cmd.map (cfg.wrapMsg << ItemUsagesMenuMsg itemUuid) menuCmd)

        OpenFileUpload path fileConfig ->
            plain model (cfg.openFileUploadCmd path fileConfig)

        DeleteFile fileUuid fileName path ->
            plain model (cfg.deleteFileCmd fileUuid path fileName)

        DownloadFile fileUuid ->
            -- The server gives the file's URL and the browser is handed it, so the
            -- page stays where it was.
            plain model (ProjectFilesApi.getFileUrl appState cfg.questionnaire.uuid fileUuid (cfg.wrapMsg << GotFileUrl))

        GotFileUrl result ->
            case result of
                Ok urlResponse ->
                    plain model (File.downloadFile urlResponse.url)

                Err _ ->
                    plain model Cmd.none

        ShowTypeHints path emptySearch questionUuid value ->
            if not emptySearch && String.isEmpty value then
                plain model Cmd.none

            else
                plain { model | typeHints = Just { path = path, searchValue = value, hints = ActionResult.Loading, highlighted = Nothing } }
                    (Cmd.map cfg.wrapMsg (loadTypeHints appState cfg path questionUuid value))

        HideTypeHints ->
            plain { model | typeHints = Nothing } Cmd.none

        TypeHintInput path allowCustomReply emptySearch questionUuid value ->
            let
                ( ( debounce, debounceCmd ), typeHints ) =
                    case ( emptySearch, value ) of
                        ( False, IntegrationReply (IntegrationReplyType.PlainType "") ) ->
                            ( ( model.typeHintsDebounce, Cmd.none ), Nothing )

                        _ ->
                            let
                                search =
                                    ReplyValue.getStringReply value
                            in
                            ( Debounce.push debounceConfig ( path, questionUuid, search ) model.typeHintsDebounce
                            , Just { path = path, searchValue = search, hints = ActionResult.Loading, highlighted = Nothing }
                            )

                newModel =
                    { model | typeHints = typeHints, typeHintsDebounce = debounce }
            in
            if allowCustomReply then
                -- A custom reply is a reply as it is typed, as well as a search.
                withEvent newModel (Cmd.map cfg.wrapMsg debounceCmd) (setReply path value)

            else
                plain newModel (Cmd.map cfg.wrapMsg debounceCmd)

        TypeHintDebounceMsg debounceMsg ->
            let
                load ( path, questionUuid, value ) =
                    loadTypeHints appState cfg path questionUuid value

                ( debounce, debounceCmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast load) debounceMsg model.typeHintsDebounce
            in
            plain { model | typeHintsDebounce = debounce } (Cmd.map cfg.wrapMsg debounceCmd)

        TypeHintsLoaded path value result ->
            case model.typeHints of
                Just typeHints ->
                    -- An answer for anything but the latest search is dropped.
                    if typeHints.path == path && typeHints.searchValue == value then
                        case result of
                            Ok hints ->
                                plain { model | typeHints = Just { typeHints | hints = ActionResult.Success hints, highlighted = Nothing } } Cmd.none

                            Err _ ->
                                plain { model | typeHints = Just { typeHints | hints = ActionResult.Error (gettext "Unable to get type hints." appState.locale) } } Cmd.none

                    else
                        plain model Cmd.none

                Nothing ->
                    plain model Cmd.none

        HighlightTypeHint path mbIndex ->
            case model.typeHints of
                Just typeHints ->
                    if typeHints.path == path then
                        plain { model | typeHints = Just { typeHints | highlighted = mbIndex } }
                            (Maybe.unwrap Cmd.none (Cmd.map cfg.wrapMsg << scrollTypeHintIntoView path) mbIndex)

                    else
                        plain model Cmd.none

                Nothing ->
                    plain model Cmd.none

        ChooseTypeHint path value ->
            withEvent { model | typeHints = Nothing } Cmd.none (setReply path value)

        SetPluginReply path replyString ->
            case D.decodeString ReplyValue.decoder replyString of
                Ok value ->
                    withEvent model Cmd.none (setReply path value)

                Err _ ->
                    plain model Cmd.none

        CopyLinkToQuestion path ->
            let
                link =
                    AppState.getClientUrlRoot appState ++ Routes.toUrl appState (Routes.projectsDetailQuestionnaire cfg.questionnaire.uuid (Just path) Nothing)
            in
            -- "Copied!" stays a moment and goes by itself, as `Ui.Copy`'s does.
            plain { model | copiedLinkPath = Just path }
                (Cmd.batch
                    [ Copy.copyToClipboard link
                    , Task.perform (always (cfg.wrapMsg (ClearCopiedLink path))) (Process.sleep 2000)
                    ]
                )

        ClearCopiedLink path ->
            if model.copiedLinkPath == Just path then
                plain { model | copiedLinkPath = Nothing } Cmd.none

            else
                plain model Cmd.none

        AddTodo path ->
            plain model (cfg.addTodoCmd path)

        RemoveTodo path ->
            plain model (cfg.removeTodoCmd path)

        OpenComments path ->
            plain model (cfg.openCommentsCmd path)

        CloseRightPanel ->
            plain model cfg.closeRightPanelCmd

        OpenPluginQuestionActionModal pluginUuid element question path ->
            plain model (cfg.openPluginQuestionActionModalCmd pluginUuid element question path)

        OpenPluginQuestionActionRightPanel data ->
            plain model (cfg.openPluginQuestionActionRightPanelCmd data)

        Noop ->
            plain model Cmd.none


moveItem : Int -> ItemEventData -> List String
moveItem delta item =
    case List.elemIndex item.itemUuid item.itemUuids of
        Just index ->
            if index + delta >= 0 && index + delta < List.length item.itemUuids then
                List.swapAt index (index + delta) item.itemUuids

            else
                item.itemUuids

        Nothing ->
            item.itemUuids


{-| The item's menu of the item select questions using it; closed for every item
but the one last opened.
-}
itemUsagesMenu : String -> Model -> Menu.Model
itemUsagesMenu itemUuid model =
    case model.itemUsagesMenu of
        Just ( menuItemUuid, menu ) ->
            if menuItemUuid == itemUuid then
                menu

            else
                Menu.init

        Nothing ->
            Menu.init


createdBy : AppState -> Maybe UserSuggestion
createdBy appState =
    Maybe.map UserConfig.toUserSuggestion appState.config.user


{-| Keeps the highlighted hint in view without moving focus, which stays in the
input. Scrolls only when the hint is outside the visible part of the list, and
only far enough to bring it in.
-}
scrollTypeHintIntoView : String -> Int -> Cmd Msg
scrollTypeHintIntoView path index =
    BrowserDom.getElement (Question.typeHintId path index)
        |> Task.andThen
            (\hint ->
                let
                    listId =
                        Question.typeHintsId path
                in
                Task.map2 Tuple.pair (BrowserDom.getElement listId) (BrowserDom.getViewportOf listId)
                    |> Task.andThen
                        (\( list, viewport ) ->
                            let
                                hintTop =
                                    viewport.viewport.y + hint.element.y - list.element.y
                            in
                            if hintTop < viewport.viewport.y then
                                BrowserDom.setViewportOf listId 0 hintTop

                            else if hintTop + hint.element.height > viewport.viewport.y + list.element.height then
                                BrowserDom.setViewportOf listId 0 (hintTop + hint.element.height - list.element.height)

                            else
                                Task.succeed ()
                        )
            )
        |> Task.attempt (always Noop)


{-| The hints of an API integration come from the server, which knows the
question's integration; those of one the client has built in, from the client,
by its settings. When it has nothing to search, there are none.
-}
loadTypeHints : AppState -> { a | mbKmEditorUuid : Maybe Uuid, questionnaire : ProjectQuestionnaire } -> String -> String -> String -> Cmd Msg
loadTypeHints appState cfg path questionUuid value =
    let
        builtInSearch =
            KnowledgeModel.getQuestion questionUuid cfg.questionnaire.knowledgeModel
                |> Maybe.andThen Question.getIntegrationUuid
                |> Maybe.andThen (\integrationUuid -> KnowledgeModel.getIntegration integrationUuid cfg.questionnaire.knowledgeModel)
                |> Maybe.andThen
                    (\integration ->
                        case integration of
                            Integration.PluginIntegration data ->
                                BuiltInIntegration.find appState.extensions.integrations data
                                    |> Maybe.map
                                        (\builtIn ->
                                            builtIn.search
                                                (BuiltInIntegration.requestContext appState cfg.questionnaire.workspaceUuid)
                                                { settings = data.pluginIntegrationSettings, q = value }
                                        )

                            _ ->
                                Nothing
                    )
    in
    case builtInSearch of
        Just (Just search) ->
            Task.attempt (TypeHintsLoaded path value) search

        Just Nothing ->
            Task.dispatch (TypeHintsLoaded path value (Ok []))

        Nothing ->
            let
                request =
                    case cfg.mbKmEditorUuid of
                        Just kmEditorUuid ->
                            TypeHintRequest.fromKmEditorQuestion kmEditorUuid (Uuid.fromUuidString questionUuid) value

                        Nothing ->
                            TypeHintRequest.fromProject cfg.questionnaire.uuid (Uuid.fromUuidString questionUuid) value
            in
            TypeHintsApi.fetchTypeHints appState request (TypeHintsLoaded path value)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.itemUsagesMenu of
        Just ( itemUuid, menu ) ->
            Sub.map (ItemUsagesMenuMsg itemUuid) (Menu.subscriptions (Item.usagesMenuId itemUuid) menu)

        Nothing ->
            Sub.none


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = TypeHintDebounceMsg
    }



-- VIEW


{-| What the content is drawn from, besides its model: every field is either a
primitive or held in the questionnaire's model, so the questionnaire can pass the
whole record to one lazy call and skip the content altogether when nothing in it
changed — after the app refreshes the time, or while the page scrolls.
-}
type alias ViewState =
    { structure : Structure
    , questionnaire : ProjectQuestionnaire
    , highlightedPath : Maybe String
    , commentsOpenPath : Maybe String
    , openActions : Maybe ( String, QuestionActions )
    }


view : Gettext.Locale -> Time.Zone -> Int -> Flags -> ViewState -> Model -> Html Msg
view locale timeZone minute baseFlags state model =
    let
        context =
            { locale = locale
            , timeZone = timeZone
            , minute = Time.millisToPosix minute
            , baseFlags = baseFlags
            , state = state
            , model = model
            , itemOptions = itemOptionsByList locale state.questionnaire state.structure.nodes
            , itemUsages = ProjectQuestionnaire.getItemUsagesInItemSelectQuestions state.questionnaire
            , project =
                if List.isEmpty model.integrationPlugins then
                    ""

                else
                    PluginElement.projectString (ProjectQuestionnaire.toProjectCommon state.questionnaire)
            }

        chapterHeadingId =
            Maybe.map (.chapter >> .uuid >> chapterId) state.structure.chapter
    in
    div [ class "c-questionnaire-content" ]
        [ case state.structure.chapter of
            Just chapter ->
                Lazy.lazy2 viewChapter chapter (state.highlightedPath == Just chapter.chapter.uuid)

            Nothing ->
                text ""
        , if state.structure.chapterEmpty then
            Alert.toHtml (Alert.info (gettext "This chapter contains no questions." locale))

          else
            text ""
        , Html.Keyed.node "div"
            (class "c-questionnaire-list"
                :: attribute "role" "list"
                :: (case chapterHeadingId of
                        Just headingId ->
                            [ attribute "aria-labelledby" headingId ]

                        Nothing ->
                            [ attribute "aria-label" (gettext "Questionnaire" locale) ]
                   )
            )
            (List.map (viewNode context) state.structure.nodes)
        , case state.structure.links of
            Just links ->
                Lazy.lazy2 viewChapterLinks locale links

            Nothing ->
                text ""
        , viewRemoveItemModal locale state.questionnaire model
        ]


type alias Context =
    { locale : Gettext.Locale
    , timeZone : Time.Zone
    , minute : Time.Posix
    , baseFlags : Flags
    , state : ViewState
    , model : Model
    , itemOptions : Dict String (List Question.ItemOption)
    , itemUsages : Dict String (List ( String, String ))

    -- The project as a plugin's element is given it, once per render: it can
    -- change without the nodes being built again (a file uploaded, say). No
    -- plugin, no project.
    , project : String
    }


viewNode : Context -> ContentNode -> ( String, Html Msg )
viewNode ctx node =
    case node of
        QuestionNode data ->
            ( data.questionPath, viewQuestion ctx data )

        ItemHeaderNode data ->
            let
                templateQuestions =
                    KnowledgeModel.getQuestionItemTemplateQuestions data.parentQuestionUuid ctx.state.questionnaire.knowledgeModel

                itemTitle =
                    ProjectQuestionnaire.getItemTitle ctx.state.questionnaire (String.split "." data.itemPath) templateQuestions
                        |> Maybe.withDefault ""

                usages =
                    Dict.get data.itemUuid ctx.itemUsages
                        |> Maybe.unwrap "" Item.encodeUsages
            in
            ( data.itemPath ++ "-header"
            , Lazy.lazy7 Item.viewHeader ctx.locale data ctx.baseFlags itemTitle (replyStringAt ctx data.parentQuestionPath) usages (itemUsagesMenu data.itemUuid ctx.model)
            )

        ItemEmptyNode data ->
            ( data.itemPath ++ "-empty", Lazy.lazy2 Item.viewEmpty ctx.locale data )

        ItemFooterNode data ->
            ( data.itemPath ++ "-footer", Lazy.lazy2 Item.viewFooter ctx.locale data )

        ItemsEndNode data ->
            ( data.questionPath ++ "-items-end"
            , Lazy.lazy4 Item.viewItemsEnd ctx.locale data ctx.baseFlags (replyStringAt ctx data.questionPath)
            )

        -- Split off by `structure`; never in the list.
        ChapterNode data ->
            ( data.chapter.uuid, text "" )

        ChapterEmptyNode ->
            ( "chapter-empty", text "" )

        ChapterLinksNode data ->
            ( "chapter-links-" ++ data.chapterUuid, text "" )


replyStringAt : Context -> String -> String
replyStringAt ctx path =
    ReplyString.fromReply (Dict.get path ctx.state.questionnaire.replies)


viewQuestion : Context -> QuestionNodeData -> Html Msg
viewQuestion ctx data =
    let
        path =
            data.questionPath

        questionnaire =
            ctx.state.questionnaire

        mbReply =
            Dict.get path questionnaire.replies

        flags =
            ctx.baseFlags
                |> Flags.withCommentsOpen (ctx.state.commentsOpenPath == Just path)
                |> Flags.withTodo (ProjectQuestionnaire.hasTodo questionnaire path)
                |> Flags.withHighlight (ctx.state.highlightedPath == Just path)
                |> Flags.withCopiedLink (ctx.model.copiedLinkPath == Just path)

        actions =
            case ctx.state.openActions of
                Just ( openPath, openActions ) ->
                    if openPath == path then
                        openActions

                    else
                        ctx.model.questionActions

                Nothing ->
                    ctx.model.questionActions

        replyString =
            ReplyString.fromReply mbReply

        answeredAt =
            ReplyString.answeredAt { locale = ctx.locale, timeZone = ctx.timeZone, minute = ctx.minute } mbReply

        commentCount =
            ProjectQuestionnaire.getUnresolvedCommentCount path questionnaire
    in
    case data.question of
        Question.OptionsQuestion _ _ ->
            Lazy.lazy7 Question.viewOptions ctx.locale actions data flags answeredAt replyString commentCount

        Question.ListQuestion _ _ ->
            Lazy.lazy6 Question.viewList ctx.locale actions data flags replyString commentCount

        Question.ValueQuestion _ _ ->
            Lazy.lazy8 Question.viewValue ctx.locale ctx.timeZone actions data flags answeredAt replyString commentCount

        Question.MultiChoiceQuestion _ _ ->
            Lazy.lazy7 Question.viewMultiChoice ctx.locale actions data flags answeredAt replyString commentCount

        Question.ItemSelectQuestion _ _ ->
            let
                listQuestionUuid =
                    Question.getListQuestionUuid data.question

                extra =
                    { itemOptions = Maybe.withDefault [] (Maybe.andThen (\uuid -> Dict.get uuid ctx.itemOptions) listQuestionUuid)
                    , itemMissing = ProjectQuestionnaire.itemSelectQuestionItemMissing questionnaire listQuestionUuid path
                    , mbItemPath = ProjectQuestionnaire.itemSelectQuestionItemPath questionnaire listQuestionUuid path
                    }
            in
            Lazy.lazy8 Question.viewItemSelect ctx.locale actions data flags (Question.encodeItemSelectExtra extra) answeredAt replyString commentCount

        Question.FileQuestion _ _ ->
            let
                fileString =
                    mbReply
                        |> Maybe.andThen (ReplyValue.getFileUuid << .value)
                        |> Maybe.andThen (ProjectQuestionnaire.getFile questionnaire)
                        |> Maybe.map (Json.Encode.encode 0 << ProjectFileSimple.encode)
                        |> Maybe.withDefault ""
            in
            Lazy.lazy8 Question.viewFile ctx.locale actions data flags answeredAt replyString fileString commentCount

        Question.IntegrationQuestion _ _ ->
            viewIntegration ctx actions data flags answeredAt replyString commentCount


viewIntegration : Context -> QuestionActions -> QuestionNodeData -> Flags -> String -> String -> Int -> Html Msg
viewIntegration ctx actions data flags answeredAt replyString commentCount =
    let
        answering =
            case data.questionSpecificData of
                IntegrationQuestionSpecificNodeData specific ->
                    specific.answering

                _ ->
                    NotConfigured

        answered =
            not (String.isEmpty replyString)
    in
    case answering of
        AnsweredFromHints ->
            let
                -- Only the question whose hints are showing is given them; every
                -- other gets the same stored `Nothing`, so a keystroke in a search
                -- does not re-render every integration question.
                typeHints =
                    case ctx.model.typeHints of
                        Just hints ->
                            if hints.path == data.questionPath then
                                ctx.model.typeHints

                            else
                                ctx.model.typeHintsNothing

                        Nothing ->
                            ctx.model.typeHintsNothing
            in
            Lazy.lazy8 Question.viewIntegrationApi ctx.locale actions data flags typeHints answeredAt replyString commentCount

        AnsweredByPlugin _ ->
            Lazy.lazy8 Question.viewIntegrationPlugin ctx.locale actions data flags answeredAt replyString ctx.project commentCount

        PluginMissing ->
            Question.viewMessage ctx.locale actions data flags answered commentCount (Alert.danger (gettext "Missing plugin." ctx.locale))

        NotConfigured ->
            Question.viewMessage ctx.locale actions data flags answered commentCount (Alert.warning (gettext "The integration for this question is not configured properly." ctx.locale))


{-| The items an item select offers, for every list question an item select on
screen selects from: worked out once per render per list, not once per question.
-}
itemOptionsByList : Gettext.Locale -> ProjectQuestionnaire -> List ContentNode -> Dict String (List Question.ItemOption)
itemOptionsByList locale questionnaire nodes =
    let
        selected =
            List.foldl
                (\node acc ->
                    case node of
                        QuestionNode data ->
                            case ( Question.getListQuestionUuid data.question, data.questionSpecificData ) of
                                ( Just listQuestionUuid, ItemSelectQuestionSpecificNodeData specific ) ->
                                    Dict.insert listQuestionUuid specific.itemTemplateQuestions acc

                                _ ->
                                    acc

                        _ ->
                            acc
                )
                Dict.empty
                nodes

        optionsFor listQuestionUuid templateQuestions =
            questionnaire.replies
                |> Dict.filter (\key _ -> String.endsWith listQuestionUuid key && ProjectQuestionnaire.isPathVisible questionnaire key)
                |> Dict.toList
                |> List.concatMap
                    (\( listPath, reply ) ->
                        ReplyValue.getItemUuids reply.value
                            |> List.indexedMap
                                (\index itemUuid ->
                                    { itemUuid = itemUuid
                                    , itemTitle =
                                        ProjectQuestionnaire.getItemTitle questionnaire (String.split "." listPath ++ [ itemUuid ]) templateQuestions
                                            |> Maybe.withDefault (String.format (gettext "Item %s" locale) [ String.fromInt (index + 1) ])
                                    }
                                )
                    )
    in
    Dict.map optionsFor selected


viewChapter : ChapterNodeData -> Bool -> Html Msg
viewChapter data highlighted =
    div
        [ class "c-questionnaire-chapter"
        , Html.Attributes.classList [ ( "is-highlighted", highlighted ) ]
        , attribute "data-path" data.chapter.uuid
        ]
        [ h2 [ class "c-questionnaire-chapter__title", id (chapterId data.chapter.uuid) ]
            [ text (data.chapterNumber ++ ". " ++ data.chapter.title) ]
        , case data.chapter.text of
            Just chapterText ->
                div [ class "c-questionnaire-chapter__text" ] [ Markdown.view { headingOffset = 2 } chapterText ]

            Nothing ->
                text ""
        ]


{-| The id of a chapter's heading, which names the list and takes the focus when
the chapter is opened.
-}
chapterId : String -> String
chapterId chapterUuid =
    "chapter-" ++ chapterUuid


{-| The chapters either side, as the wizard had them: two large buttons sharing
the width, each saying which way it goes in a lighter line above the chapter's
name, with the arrow at its outer edge and the words towards the middle.
-}
viewChapterLinks : Gettext.Locale -> ChapterLinksNodeData -> Html Msg
viewChapterLinks locale data =
    let
        link direction label icon chapter =
            Html.button
                [ type_ "button"
                , class ("c-questionnaire-chapter-link c-questionnaire-chapter-link--" ++ direction)
                , Html.Events.onClick (OpenChapter chapter.uuid)
                ]
                [ span [ class "c-questionnaire-chapter-link__label" ] [ text label ]
                , span [ class "c-questionnaire-chapter-link__title" ] [ text chapter.title ]
                , Icon.toHtml icon
                ]
    in
    Html.node "nav"
        [ class "c-questionnaire-chapter-links", attribute "aria-label" (gettext "Previous and next chapter" locale) ]
        (Maybe.values
            [ Maybe.map (link "previous" (gettext "Previous chapter" locale) (Icon.solid "fa-chevron-left")) data.previousChapter
            , Maybe.map (link "next" (gettext "Next chapter" locale) (Icon.solid "fa-chevron-right")) data.nextChapter
            ]
        )


viewRemoveItemModal : Gettext.Locale -> ProjectQuestionnaire -> Model -> Html Msg
viewRemoveItemModal locale questionnaire model =
    let
        usages =
            case model.removeItem of
                Just item ->
                    ProjectQuestionnaire.getItemUsageInItemSelectQuestions questionnaire item.itemUuid

                Nothing ->
                    []

        usageList =
            if List.isEmpty usages then
                []

            else
                [ Html.p [] [ text (gettext "There are some item select questions using this item:" locale) ]
                , ul [ class "c-questionnaire-item-usages" ]
                    (List.map
                        (\( path, label ) ->
                            li [] [ Html.button [ Html.Attributes.type_ "button", class "c-questionnaire-text-button", Html.Events.onClick (ScrollToPath path) ] [ text label ] ]
                        )
                        usages
                    )
                ]
    in
    ConfirmModal.dangerConfirm { locale = locale }
        { title = gettext "Remove item" locale
        , open = Maybe.isJust model.removeItem
        , body = Html.p [] [ text (gettext "Are you sure you want to remove this item?" locale) ] :: usageList
        , confirmLabel = gettext "Remove" locale
        , onConfirm = RemoveItemConfirm
        , onClose = RemoveItemCancel
        , result = ActionResult.Unset
        , testId = Just "remove-item"
        }
