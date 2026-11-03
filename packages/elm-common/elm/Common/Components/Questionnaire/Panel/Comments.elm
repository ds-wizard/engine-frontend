module Common.Components.Questionnaire.Panel.Comments exposing
    ( Deleting
    , Model
    , Msg
    , Outcome(..)
    , Return
    , UpdateConfig
    , ViewConfig
    , init
    , setViewPrivate
    , subscriptions
    , threadElementId
    , update
    , view
    )

{-| The comments on one question: the threads people have started on it, and
everything that can be done to them.

**Every change is an event.** Writing, editing, deleting, resolving, reopening and
assigning all come back as `ProjectEvent`s with uuids made here; the project page
sends them over the socket and `Questionnaire.applyProjectEvent` applies them —
the same function that applies the one somebody else made a moment ago. The panel
never calls the API itself except to ask who can be assigned.

**Two kinds of thread.** A comment everybody with the project can read, and an
editor's note only editors can (`Feature.projectCommentPrivate`). They are two
tabs over the same list, and which one is showing decides what a new thread will
be.

**Structure**: the threads are a list, each thread a
list item named by who started it, its comments a list inside it. Deleting asks
through a dialog that takes focus, rather than an overlay a stray click dismisses.
What a thread is — resolved, an editor's note, assigned to somebody — is said in
words, not by its color.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom as Dom
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.ProjectDetail.Comment exposing (Comment)
import Common.Api.Models.ProjectDetail.CommentThread as CommentThread exposing (CommentThread)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Api.Models.User as User
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Api.Projects as ProjectsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Markdown as Markdown
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Ports.Copy as Copy
import Common.Routes as Routes
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, id)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import String.Format as String
import Task
import Time
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.EmptyState as EmptyState
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.ItemIcon as ItemIcon
import Ui.Menu as Menu
import Ui.Tabs as Tabs
import Ui.TestId as TestId
import Ui.Text as Text
import Ui.Tooltip as Tooltip
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid


type alias Model =
    { projectUuid : Uuid

    -- What is being written, keyed by the thread it belongs to ("" for a new one).
    , inputs : Dict String String
    , editing : Maybe { comment : Uuid, text : String }
    , deleting : Maybe Deleting
    , menus : Dict String Menu.Model
    , assignQuery : String
    , assignDebounce : Debounce String
    , assignSuggestions : ActionResult (List UserSuggestion)
    , viewPrivate : Bool
    , copiedThread : Maybe Uuid
    }


{-| What a confirmation is about: a whole thread, or one comment in one.
-}
type alias Deleting =
    { path : String
    , thread : CommentThread
    , comment : Maybe Comment
    }


init : Uuid -> Model
init projectUuid =
    { projectUuid = projectUuid
    , inputs = Dict.empty
    , editing = Nothing
    , deleting = Nothing
    , menus = Dict.empty
    , assignQuery = ""
    , assignDebounce = Debounce.init
    , assignSuggestions = Unset
    , viewPrivate = False
    , copiedThread = Nothing
    }


{-| Which tab is showing, when a link to a thread decides it.
-}
setViewPrivate : Bool -> Model -> Model
setViewPrivate private model =
    { model | viewPrivate = private }


{-| The element a thread's deep link scrolls to.
-}
threadElementId : Uuid -> String
threadElementId threadUuid =
    "comment-thread-" ++ Uuid.toString threadUuid



-- UPDATE


type Msg
    = Typed String (Maybe Uuid) String
    | Send String (Maybe Uuid) Bool
    | StartEdit Comment
    | EditTyped String
    | CancelEdit
    | SendEdit String CommentThread Comment
    | AskDelete String CommentThread (Maybe Comment)
    | CancelDelete
    | ConfirmDelete
    | Resolve String CommentThread
    | Reopen String CommentThread
    | Assign String CommentThread (Maybe UserSuggestion)
    | AssignQueryTyped Bool String
    | AssignDebounceMsg Bool Debounce.Msg
    | SuggestionsLoaded (Result ApiError (Pagination UserSuggestion))
    | MenuMsg String Menu.Msg
    | SetViewPrivate Bool
    | SetViewResolved Bool
    | CopyLink String Uuid
    | ClearCopied
    | OpenThreadOf String
    | Noop


{-| What the panel asks the questionnaire for: another question's comments, or the
resolved threads shown or hidden (which the questionnaire remembers).
-}
type Outcome
    = OpenComments String
    | ViewResolved Bool


type alias Return msg =
    { seed : Seed
    , model : Model
    , cmd : Cmd msg
    , event : Maybe ProjectEvent
    , outcome : Maybe Outcome
    }


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg }


update : AppState -> UpdateConfig msg -> Msg -> Model -> Return msg
update appState cfg msg model =
    let
        plain newModel cmd =
            { seed = appState.seed, model = newModel, cmd = cmd, event = Nothing, outcome = Nothing }

        withEvent newModel toEvent =
            let
                ( uuid, seed ) =
                    Uuid.step appState.seed
            in
            { seed = seed, model = newModel, cmd = Cmd.none, event = Just (toEvent uuid), outcome = Nothing }

        createdBy =
            Maybe.map UserConfig.toUserSuggestion appState.config.user

        -- An item acts and leaves its menu open unless the menu is told to
        -- close, so whatever an item sends closes the menus it may have come
        -- from. Only an open one is closed: closing returns focus to its
        -- trigger, and a menu that was not used should not take it.
        closeMenus menuMsg keys ret =
            let
                closeOne key ( m, cmds ) =
                    case Dict.get key m.menus of
                        Just menu ->
                            if Menu.isOpen menu then
                                let
                                    ( closed, menuCmd ) =
                                        Menu.update key 8 menuMsg menu
                                in
                                ( { m | menus = Dict.insert key closed m.menus }
                                , Cmd.map (cfg.wrapMsg << MenuMsg key) menuCmd :: cmds
                                )

                            else
                                ( m, cmds )

                        Nothing ->
                            ( m, cmds )

                ( newModel, menuCmds ) =
                    List.foldl closeOne ( ret.model, [] ) keys
            in
            { ret | model = newModel, cmd = Cmd.batch (ret.cmd :: menuCmds) }
    in
    case msg of
        Typed path mbThread value ->
            plain { model | inputs = Dict.insert (inputKey path mbThread) value model.inputs } Cmd.none

        Send path mbThread private ->
            let
                text_ =
                    String.trim (Maybe.withDefault "" (Dict.get (inputKey path mbThread) model.inputs))
            in
            if String.isEmpty text_ then
                plain model Cmd.none

            else
                let
                    ( threadUuid, seed1 ) =
                        case mbThread of
                            Just uuid ->
                                ( uuid, appState.seed )

                            Nothing ->
                                Uuid.step appState.seed

                    ( commentUuid, seed2 ) =
                        Uuid.step seed1

                    ( eventUuid, seed3 ) =
                        Uuid.step seed2
                in
                { seed = seed3
                , model = { model | inputs = Dict.remove (inputKey path mbThread) model.inputs }
                , cmd = Cmd.none
                , event =
                    Just
                        (ProjectEvent.AddComment
                            { uuid = eventUuid
                            , path = path
                            , threadUuid = threadUuid
                            , commentUuid = commentUuid
                            , newThread = Maybe.isNothing mbThread
                            , private = private
                            , text = text_
                            , createdAt = appState.currentTime
                            , createdBy = createdBy
                            }
                        )
                , outcome = Nothing
                }

        StartEdit comment ->
            -- The text is where the keyboard goes, so the menu closes without
            -- pulling focus back to its trigger.
            plain { model | editing = Just { comment = comment.uuid, text = comment.text } }
                (Task.attempt (always (cfg.wrapMsg Noop)) (Dom.focus (commentEditId comment.uuid)))
                |> closeMenus Menu.closeWithoutFocus [ commentMenuId comment.uuid ]

        EditTyped value ->
            plain { model | editing = Maybe.map (\editing -> { editing | text = value }) model.editing } Cmd.none

        CancelEdit ->
            plain { model | editing = Nothing } Cmd.none

        SendEdit path thread comment ->
            case model.editing of
                Just editing ->
                    withEvent { model | editing = Nothing }
                        (\uuid ->
                            ProjectEvent.EditComment
                                { uuid = uuid
                                , path = path
                                , threadUuid = thread.uuid
                                , commentUuid = comment.uuid
                                , private = thread.private
                                , text = String.trim editing.text
                                , createdAt = appState.currentTime
                                , createdBy = createdBy
                                }
                        )

                Nothing ->
                    plain model Cmd.none

        AskDelete path thread mbComment ->
            -- The dialog takes focus, so the menu leaves it alone.
            plain { model | deleting = Just { path = path, thread = thread, comment = mbComment } } Cmd.none
                |> closeMenus Menu.closeWithoutFocus
                    [ Maybe.unwrap (threadMenuId thread.uuid) (commentMenuId << .uuid) mbComment ]

        CancelDelete ->
            plain { model | deleting = Nothing } Cmd.none

        ConfirmDelete ->
            case model.deleting of
                Just deleting ->
                    withEvent { model | deleting = Nothing }
                        (\uuid ->
                            case deleting.comment of
                                Just comment ->
                                    ProjectEvent.DeleteComment
                                        { uuid = uuid
                                        , path = deleting.path
                                        , threadUuid = deleting.thread.uuid
                                        , commentUuid = comment.uuid
                                        , private = deleting.thread.private
                                        , createdAt = appState.currentTime
                                        , createdBy = createdBy
                                        }

                                Nothing ->
                                    ProjectEvent.DeleteCommentThread
                                        { uuid = uuid
                                        , path = deleting.path
                                        , threadUuid = deleting.thread.uuid
                                        , private = deleting.thread.private
                                        , createdAt = appState.currentTime
                                        , createdBy = createdBy
                                        }
                        )

                Nothing ->
                    plain model Cmd.none

        Resolve path thread ->
            withEvent model
                (\uuid ->
                    ProjectEvent.ResolveCommentThread
                        { uuid = uuid
                        , path = path
                        , threadUuid = thread.uuid
                        , private = thread.private
                        , commentCount = List.length thread.comments
                        , createdAt = appState.currentTime
                        , createdBy = createdBy
                        }
                )

        Reopen path thread ->
            withEvent model
                (\uuid ->
                    ProjectEvent.ReopenCommentThread
                        { uuid = uuid
                        , path = path
                        , threadUuid = thread.uuid
                        , private = thread.private
                        , commentCount = List.length thread.comments
                        , createdAt = appState.currentTime
                        , createdBy = createdBy
                        }
                )
                |> closeMenus Menu.close [ threadMenuId thread.uuid ]

        Assign path thread mbUser ->
            withEvent { model | assignQuery = "", assignSuggestions = Unset }
                (\uuid ->
                    ProjectEvent.AssignCommentThread
                        { uuid = uuid
                        , path = path
                        , threadUuid = thread.uuid
                        , private = thread.private
                        , assignedTo = mbUser
                        , createdAt = appState.currentTime
                        , createdBy = createdBy
                        }
                )
                |> closeMenus Menu.close [ threadMenuId thread.uuid, assignMenuId thread.uuid ]

        AssignQueryTyped private query ->
            let
                ( debounce, debounceCmd ) =
                    Debounce.push (suggestionsDebounce private) query model.assignDebounce
            in
            plain { model | assignQuery = query, assignDebounce = debounce, assignSuggestions = Loading }
                (Cmd.map cfg.wrapMsg debounceCmd)

        AssignDebounceMsg private debounceMsg ->
            let
                load query =
                    ProjectsApi.getUserSuggestions appState model.projectUuid private query SuggestionsLoaded

                ( debounce, debounceCmd ) =
                    Debounce.update (suggestionsDebounce private) (Debounce.takeLast load) debounceMsg model.assignDebounce
            in
            plain { model | assignDebounce = debounce } (Cmd.map cfg.wrapMsg debounceCmd)

        SuggestionsLoaded result ->
            let
                suggestions =
                    case result of
                        Ok pagination ->
                            Success pagination.items

                        Err _ ->
                            Error (gettext "Unable to get the people who can be assigned." appState.locale)
            in
            plain { model | assignSuggestions = suggestions } Cmd.none

        MenuMsg key menuMsg ->
            let
                before =
                    Maybe.withDefault Menu.init (Dict.get key model.menus)

                ( menu, menuCmd ) =
                    Menu.update key 8 menuMsg before

                opened =
                    Menu.isOpen menu && not (Menu.isOpen before)

                -- Opening the list of people to assign to is the moment to ask
                -- who they are: whoever opened it wants to read a list, not to
                -- be told to type first, and the answer to the empty search is
                -- the list they expect. The thread is one of the ones on show,
                -- so it is private exactly when this tab is.
                withSuggestions =
                    if opened && String.startsWith assignMenuPrefix key then
                        ( { model | menus = Dict.insert key menu model.menus, assignQuery = "", assignSuggestions = Loading }
                        , ProjectsApi.getUserSuggestions appState model.projectUuid model.viewPrivate "" SuggestionsLoaded
                        )

                    else
                        ( { model | menus = Dict.insert key menu model.menus }, Cmd.none )
            in
            plain (Tuple.first withSuggestions)
                (Cmd.batch
                    [ Cmd.map (cfg.wrapMsg << MenuMsg key) menuCmd
                    , Cmd.map cfg.wrapMsg (Tuple.second withSuggestions)
                    ]
                )

        SetViewPrivate private ->
            plain { model | viewPrivate = private } Cmd.none

        SetViewResolved resolved ->
            { seed = appState.seed, model = model, cmd = Cmd.none, event = Nothing, outcome = Just (ViewResolved resolved) }

        CopyLink path threadUuid ->
            plain { model | copiedThread = Just threadUuid }
                (Copy.copyToClipboard (threadLink appState model.projectUuid path threadUuid))

        ClearCopied ->
            plain { model | copiedThread = Nothing } Cmd.none

        OpenThreadOf path ->
            { seed = appState.seed, model = model, cmd = Cmd.none, event = Nothing, outcome = Just (OpenComments path) }

        Noop ->
            plain model Cmd.none


inputKey : String -> Maybe Uuid -> String
inputKey path mbThread =
    path ++ "-" ++ Maybe.unwrap "new" Uuid.toString mbThread


assignMenuPrefix : String
assignMenuPrefix =
    "comment-assign-"


assignMenuId : Uuid -> String
assignMenuId threadUuid =
    assignMenuPrefix ++ Uuid.toString threadUuid


commentEditId : Uuid -> String
commentEditId commentUuid =
    "comment-edit-" ++ Uuid.toString commentUuid


commentMenuId : Uuid -> String
commentMenuId commentUuid =
    "comment-menu-" ++ Uuid.toString commentUuid


suggestionsDebounce : Bool -> Debounce.Config Msg
suggestionsDebounce private =
    { strategy = Debounce.later 500, transform = AssignDebounceMsg private }


{-| The address of a thread, to give to somebody else.
-}
threadLink : AppState -> Uuid -> String -> Uuid -> String
threadLink appState projectUuid path threadUuid =
    AppState.getClientUrlRoot appState ++ Routes.toUrl appState (Routes.projectsDetailQuestionnaire projectUuid (Just path) (Just threadUuid))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (List.map (\( key, menu ) -> Sub.map (MenuMsg key) (Menu.subscriptions key menu)) (Dict.toList model.menus))



-- VIEW


type alias ViewConfig =
    { path : String
    , threads : ActionResult (List CommentThread)
    , questionnaire : ProjectQuestionnaire
    , viewResolved : Bool
    , readonly : Bool

    -- The questions either side that also have comments, for moving between them.
    , previousPath : Maybe String
    , nextPath : Maybe String
    , position : Maybe ( Int, Int )
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    div [ class "c-questionnaire-comments", classList [ ( "is-private", model.viewPrivate ) ] ]
        -- The way out of this question and the two tabs are one block, which
        -- stays where it is while the threads scroll under it.
        [ div [ class "c-questionnaire-comments__head" ]
            (viewNavigation appState cfg ++ viewTabs appState cfg model)
        , -- What the tabs above control, when there are tabs: one element whose
          -- contents change, so their `aria-controls` never points at nothing.
          Tabs.panel
            { id = tabsId
            , label =
                if model.viewPrivate then
                    gettext "Editor notes" appState.locale

                else
                    gettext "Comments" appState.locale
            }
            [ AsyncBlock.view
                { loading = ActionResult.isLoading cfg.threads
                , error =
                    case cfg.threads of
                        Error message ->
                            Just message

                        _ ->
                            Nothing
                , loadingLabel = gettext "Loading comments…" appState.locale
                , content = [ viewThreads appState cfg model (ActionResult.withDefault [] cfg.threads) ]
                }
            ]
        , viewDeleteModal appState model
        ]


viewNavigation : AppState -> ViewConfig -> List (Html Msg)
viewNavigation appState cfg =
    case cfg.position of
        Just ( index, total ) ->
            [ div [ class "c-questionnaire-comments__navigation" ]
                [ -- Each opens its tooltip towards the middle of the panel: at
                  -- the ends of the bar a centered bubble runs off the side.
                  Button.toHtml
                    (Button.iconOnly (Icon.solid "fa-chevron-up") (gettext "Previous question with comments" appState.locale) (OpenThreadOf (Maybe.withDefault cfg.path cfg.previousPath))
                        |> Button.quiet
                        |> Button.small
                        |> Button.withTooltipAt Tooltip.Start
                        |> Button.withDisabled (Maybe.isNothing cfg.previousPath)
                    )
                , Text.bodySmall (String.format (gettext "Question %s of %s" appState.locale) [ String.fromInt index, String.fromInt total ])
                , Button.toHtml
                    (Button.iconOnly (Icon.solid "fa-chevron-down") (gettext "Next question with comments" appState.locale) (OpenThreadOf (Maybe.withDefault cfg.path cfg.nextPath))
                        |> Button.quiet
                        |> Button.small
                        |> Button.withTooltipAt Tooltip.End
                        |> Button.withDisabled (Maybe.isNothing cfg.nextPath)
                    )
                ]
            ]

        Nothing ->
            []


viewTabs : AppState -> ViewConfig -> Model -> List (Html Msg)
viewTabs appState cfg model =
    let
        count private =
            ActionResult.withDefault [] cfg.threads
                |> List.filter (\thread -> thread.private == private && not thread.resolved)
                |> List.length

        tab private label =
            Tabs.tab
                { id =
                    if private then
                        "private"

                    else
                        "public"
                , label = label
                , onSelect = SetViewPrivate private
                }
                |> Tabs.withBadge (Badge.toHtml (Badge.neutral (String.fromInt (count private))))
    in
    if Feature.projectCommentPrivate appState cfg.questionnaire then
        [ Tabs.toHtmlWithTabIds identity
            { id = tabsId
            , label = gettext "Comments and editor notes" appState.locale
            , active =
                if model.viewPrivate then
                    "private"

                else
                    "public"
            , full = True
            , tabs =
                [ tab False (gettext "Comments" appState.locale)
                , tab True (gettext "Editor notes" appState.locale)
                ]
            }
        ]

    else
        []


viewThreads : AppState -> ViewConfig -> Model -> List CommentThread -> Html Msg
viewThreads appState cfg model threads =
    let
        mine =
            List.filter (\thread -> thread.private == model.viewPrivate) threads
                |> List.sortWith CommentThread.compare

        ( resolved, unresolved ) =
            List.partition .resolved mine

        anyResolved =
            not (List.isEmpty resolved)

        resolvedToggle =
            if anyResolved then
                [ Field.toHtml
                    (Field.toggle
                        { id = "questionnaire-comments-resolved"
                        , label = gettext "Show resolved" appState.locale
                        , checked = cfg.viewResolved
                        , onCheck = SetViewResolved
                        }
                    )
                ]

            else
                []

        shown =
            if cfg.viewResolved then
                unresolved ++ resolved

            else
                unresolved

        -- What an editor's note is, said where the notes are: the difference
        -- between a note and a comment is who can see it, and nothing else on
        -- the tab says so. The lock is what it feels like.
        privateNote =
            if model.viewPrivate then
                [ div [ class "c-questionnaire-comments__note" ]
                    [ Alert.toHtml (Alert.private (gettext "Editor notes are only visible by the people who can edit this project." appState.locale)) ]
                ]

            else
                []

        emptyState =
            if List.isEmpty shown then
                [ EmptyState.toHtml
                    (EmptyState.emptyState
                        (if model.viewPrivate then
                            { title = gettext "No editor notes yet" appState.locale
                            , description = gettext "Leave something here for whoever edits this question next." appState.locale
                            }

                         else
                            { title = gettext "No comments yet" appState.locale
                            , description = gettext "Ask something about this question, or leave a note for whoever reads it next." appState.locale
                            }
                        )
                        |> EmptyState.withIllustration
                            (Icon.toHtml
                                (if model.viewPrivate then
                                    Icon.solid "fa-lock"

                                 else
                                    Icon.regular "fa-comment"
                                )
                            )
                        |> EmptyState.withHeadingLevel 3
                    )
                ]

            else
                []
    in
    div [ class "c-questionnaire-comments__body" ]
        (resolvedToggle
            ++ privateNote
            ++ emptyState
            ++ [ ul [ class "c-questionnaire-comments__threads" ] (List.map (viewThread appState cfg model) shown) ]
            ++ viewNewThread appState cfg model
        )


viewThread : AppState -> ViewConfig -> Model -> CommentThread -> Html Msg
viewThread appState cfg model thread =
    let
        name =
            Maybe.unwrap (gettext "Anonymous user" appState.locale) User.fullName thread.createdBy

        assigned =
            Maybe.map (\user -> ( Just user.uuid == Maybe.map .uuid appState.config.user, User.fullName user )) thread.assignedTo

        reply =
            if thread.resolved || cfg.readonly then
                []

            else
                [ viewEditor appState
                    { path = cfg.path
                    , thread = Just thread.uuid
                    , private = thread.private
                    , value = Maybe.withDefault "" (Dict.get (inputKey cfg.path (Just thread.uuid)) model.inputs)
                    , placeholder = gettext "Reply…" appState.locale
                    , send = gettext "Reply" appState.locale
                    }
                ]
    in
    li
        [ class "c-questionnaire-comments__thread"
        , classList [ ( "is-resolved", thread.resolved ), ( "is-private", thread.private ) ]
        , id (threadElementId thread.uuid)
        , attribute "data-comment-thread-uuid" (Uuid.toString thread.uuid)
        , TestId.attribute "comments_thread"
        , attribute "aria-label"
            (String.format (ngettext ( "Thread by %s, 1 comment", "Thread by %s, %s comments" ) (List.length thread.comments) appState.locale)
                [ name, String.fromInt (List.length thread.comments) ]
            )
        ]
        (viewThreadHeader appState cfg model thread assigned
            ++ [ ul [ class "c-questionnaire-comments__comments" ]
                    (List.indexedMap (viewComment appState cfg model thread) (List.sortWith commentOrder thread.comments))
               ]
            ++ reply
        )


{-| The thread's own strip, across the top of it: what is to be done with the
thread, and what state it is in.

Everything here belongs to the thread rather than to any one comment in it, and
before this it sat on the thread's first comment — where it took the room the
person's name needed and read as though it were about their comment. Resolve
comes first, where a list of things to do has its box.

-}
viewThreadHeader : AppState -> ViewConfig -> Model -> CommentThread -> Maybe ( Bool, String ) -> List (Html Msg)
viewThreadHeader appState cfg model thread assigned =
    let
        quiet button =
            Button.toHtml (button |> Button.quiet |> Button.small |> Button.withTooltipAt Tooltip.End)

        resolve =
            if not cfg.readonly && not thread.resolved && Feature.projectCommentThreadResolve appState cfg.questionnaire thread then
                -- At the start of the strip, so its tooltip opens towards the
                -- panel; from the end it would open over the panel's edge.
                [ Button.toHtml
                    (Button.iconOnly (Icon.regular "fa-circle-check") (gettext "Resolve this thread" appState.locale) (Resolve cfg.path thread)
                        |> Button.quiet
                        |> Button.small
                        |> Button.withTooltipAt Tooltip.Start
                        |> Button.withTestId "comments_thread_resolve"
                    )
                ]

            else
                []

        state =
            if thread.resolved then
                -- A badge, not a line of text: settled is what the reader is
                -- looking for when they show the settled threads, and a shape
                -- is found before words are read. The inverse one, which is
                -- the badge made unmissable and says nothing by hue — a
                -- settled thread is not a thing that went right, and green
                -- beside an editor note's purple said two things at once.
                [ Badge.toHtml
                    (Badge.inverse (gettext "Resolved" appState.locale)
                        |> Badge.withIcon (Icon.solid "fa-check")
                    )
                ]

            else
                case assigned of
                    Just ( True, _ ) ->
                        [ Icon.toHtml (Icon.solid "fa-user-pen"), text (gettext "Assigned to you" appState.locale) ]

                    Just ( False, name ) ->
                        [ Icon.toHtml (Icon.solid "fa-user-check"), text (String.format (gettext "Assigned to %s" appState.locale) [ name ]) ]

                    Nothing ->
                        []

        copy =
            [ quiet
                (Button.iconOnly (Icon.solid "fa-link")
                    (if model.copiedThread == Just thread.uuid then
                        gettext "Copied!" appState.locale

                     else
                        gettext "Copy link to this thread" appState.locale
                    )
                    (CopyLink cfg.path thread.uuid)
                    |> Button.withTestId "comments_thread_copy-link"
                )
            ]

        -- A settled thread is waiting on nobody, so who it was assigned to is
        -- neither shown, nor changed, nor marked.
        assign =
            if not cfg.readonly && not thread.resolved && Feature.projectCommentThreadAssign appState cfg.questionnaire thread then
                [ viewAssign appState cfg model thread ]

            else
                []

        items =
            Maybe.values
                [ if not cfg.readonly && Feature.projectCommentThreadReopen appState cfg.questionnaire thread then
                    Just (Menu.item (gettext "Reopen this thread" appState.locale) (Reopen cfg.path thread) |> Menu.withIcon (Icon.solid "fa-rotate-left") |> Menu.withTestId "comments_thread_reopen")

                  else
                    Nothing
                , if not cfg.readonly && not thread.resolved && Maybe.isJust thread.assignedTo && Feature.projectCommentThreadRemoveAssign appState cfg.questionnaire thread then
                    Just (Menu.item (gettext "Remove assignment" appState.locale) (Assign cfg.path thread Nothing) |> Menu.withIcon (Icon.solid "fa-user-slash") |> Menu.withTestId "comments_thread_remove-assignment")

                  else
                    Nothing
                , if not cfg.readonly && Feature.projectCommentThreadDelete appState cfg.questionnaire thread then
                    Just (Menu.dangerItem (gettext "Delete this thread" appState.locale) (AskDelete cfg.path thread Nothing) |> Menu.withIcon (Icon.solid "fa-trash") |> Menu.withTestId "comments_thread_delete")

                  else
                    Nothing
                ]

        menu =
            if List.isEmpty items then
                []

            else
                [ Menu.toHtml
                    { id = threadMenuId thread.uuid
                    , trigger = Menu.kebab (gettext "What to do with this thread" appState.locale) |> Menu.withTriggerTestId "comments_thread_menu"
                    , alignment = Menu.End
                    , items = items
                    , toMsg = MenuMsg (threadMenuId thread.uuid)
                    }
                    (Maybe.withDefault Menu.init (Dict.get (threadMenuId thread.uuid) model.menus))
                ]
    in
    [ div
        [ class "c-questionnaire-comments__thread-header"
        , classList
            [ ( "is-resolved", thread.resolved )
            , ( "is-mine", not thread.resolved && Maybe.unwrap False Tuple.first assigned )
            ]
        ]
        (resolve
            ++ [ span [ class "c-questionnaire-comments__thread-state" ] state ]
            ++ copy
            ++ assign
            ++ menu
        )
    ]


{-| The tabs, and the panel under them that they control.
-}
tabsId : String
tabsId =
    "questionnaire-comments"


{-| Not the thread's own id (`threadElementId`), which the link to a thread
scrolls to: two elements cannot share one.
-}
threadMenuId : Uuid -> String
threadMenuId threadUuid =
    "comment-thread-actions-" ++ Uuid.toString threadUuid


commentOrder : Comment -> Comment -> Order
commentOrder a b =
    compare (Time.posixToMillis a.createdAt) (Time.posixToMillis b.createdAt)


viewComment : AppState -> ViewConfig -> Model -> CommentThread -> Int -> Comment -> Html Msg
viewComment appState cfg model thread index comment =
    let
        first =
            index == 0

        name =
            Maybe.unwrap (gettext "Anonymous user" appState.locale) User.fullName comment.createdBy

        avatar =
            case comment.createdBy of
                Just user ->
                    avatarOf user.firstName user.lastName user.imageUrl (Just user.gravatarHash)

                Nothing ->
                    UserAvatar.anonymous

        edited =
            if Time.posixToMillis comment.updatedAt > Time.posixToMillis comment.createdAt then
                [ Text.bodySmall (gettext "(edited)" appState.locale) ]

            else
                []

        body =
            case model.editing of
                Just editing ->
                    if editing.comment == comment.uuid then
                        [ -- In the editor's box, as a new comment and a reply are,
                          -- so the focus ring keeps off Save.
                          div [ class "c-questionnaire-comments__editor" ]
                            [ Field.toHtml
                                (Field.textarea
                                    { id = commentEditId comment.uuid
                                    , label = gettext "Edit the comment" appState.locale
                                    , value = editing.text
                                    , onInput = EditTyped
                                    }
                                    |> Field.withHiddenLabel
                                    |> Field.withSubmitShortcut (SendEdit cfg.path thread comment)
                                    |> Field.withRows (linesIn editing.text)
                                )
                            , div [ class "c-questionnaire-comments__actions" ]
                                [ Button.toHtml (Button.primary (gettext "Save" appState.locale) (SendEdit cfg.path thread comment) |> Button.small |> Button.withTestId "comments_edit_submit")
                                , Button.toHtml (Button.ghost (gettext "Cancel" appState.locale) CancelEdit |> Button.small |> Button.withTestId "comments_edit_cancel")
                                ]
                            ]
                        ]

                    else
                        [ Markdown.view { headingOffset = 3 } comment.text ]

                Nothing ->
                    [ Markdown.view { headingOffset = 3 } comment.text ]
    in
    li [ class "c-questionnaire-comments__comment", attribute "data-comment-uuid" (Uuid.toString comment.uuid), TestId.attribute "comments_comment" ]
        (div [ class "c-questionnaire-comments__header" ]
            ([ ItemIcon.toHtml avatar
             , div [ class "c-questionnaire-comments__who" ]
                [ -- The name has one line and is cut short rather than wrapped:
                  -- a thread is read down its left edge, and a name that wraps
                  -- pushes what was said away from it.
                  span [ class "c-questionnaire-comments__author" ] [ text name ]
                , span [ class "c-questionnaire-comments__when" ] (Timestamp.view appState comment.createdAt :: edited)
                ]
             ]
                ++ viewCommentActions appState cfg model thread comment first
            )
            :: body
        )


{-| Somebody's face, wherever they appear in the panel.
-}
avatarOf : String -> String -> Maybe String -> Maybe String -> ItemIcon.ItemIcon
avatarOf firstName lastName imageUrl gravatarHash =
    UserAvatar.user
        { firstName = firstName
        , lastName = lastName
        , imageUrl = imageUrl
        , gravatarHash = gravatarHash
        }


{-| What can be done with one comment, which is only ever to change its words
or take it away. Everything that belongs to the thread is in the thread's own
header.

The first comment has no delete of its own: taking it away is taking the thread
away, and that is what the thread's menu is for.

-}
viewCommentActions : AppState -> ViewConfig -> Model -> CommentThread -> Comment -> Bool -> List (Html Msg)
viewCommentActions appState cfg model thread comment first =
    let
        items =
            Maybe.values
                [ if not cfg.readonly && Feature.projectCommentEdit appState cfg.questionnaire thread comment then
                    Just (Menu.item (gettext "Edit this comment" appState.locale) (StartEdit comment) |> Menu.withIcon (Icon.solid "fa-pen") |> Menu.withTestId "comments_comment_edit")

                  else
                    Nothing
                , if not first && not cfg.readonly && Feature.projectCommentDelete appState cfg.questionnaire thread comment then
                    Just (Menu.dangerItem (gettext "Delete this comment" appState.locale) (AskDelete cfg.path thread (Just comment)) |> Menu.withIcon (Icon.solid "fa-trash") |> Menu.withTestId "comments_comment_delete")

                  else
                    Nothing
                ]

        menu =
            if List.isEmpty items then
                []

            else
                [ Menu.toHtml
                    { id = commentMenuId comment.uuid
                    , trigger = Menu.kebab (gettext "Comment actions" appState.locale) |> Menu.withTriggerTestId "comments_comment_menu"
                    , alignment = Menu.End
                    , items = items
                    , toMsg = MenuMsg (commentMenuId comment.uuid)
                    }
                    (Maybe.withDefault Menu.init (Dict.get (commentMenuId comment.uuid) model.menus))
                ]
    in
    [ div [ class "c-questionnaire-comments__buttons" ] menu ]


{-| Who to assign the thread to: a menu with a box to search by, whose people
come from the project (editors only for an editor's note).
-}
viewAssign : AppState -> ViewConfig -> Model -> CommentThread -> Html Msg
viewAssign appState cfg model thread =
    let
        suggestions =
            case model.assignSuggestions of
                Success users ->
                    List.map
                        (\user ->
                            Menu.item (User.fullName user) (Assign cfg.path thread (Just user))
                                |> Menu.withTestId ("comments_thread_assign_" ++ Uuid.toString user.uuid)
                                |> Menu.withLeading (ItemIcon.toHtml (ItemIcon.withSize ItemIcon.Small (avatarOf user.firstName user.lastName user.imageUrl (Just user.gravatarHash))))
                        )
                        users

                Loading ->
                    [ Menu.note [ Text.bodySmall (gettext "Loading…" appState.locale) ] ]

                Error message ->
                    [ Menu.note [ Text.bodySmall message ] ]

                Unset ->
                    [ Menu.note [ Text.bodySmall (gettext "Loading…" appState.locale) ] ]

        items =
            Menu.searchInput
                { placeholder = gettext "Search people…" appState.locale
                , value = model.assignQuery
                , onInput = AssignQueryTyped thread.private
                }
                :: suggestions
    in
    Menu.toHtml
        { id = assignMenuId thread.uuid
        , trigger =
            Menu.custom
                { label = gettext "Assign this thread" appState.locale
                , class = "ui-button ui-button--quiet ui-button--sm ui-button--icon c-questionnaire-comments__assign"
                , content = [ Icon.toHtml (Icon.solid "fa-user-plus") ]
                }
                |> Menu.withTriggerTooltipAt Tooltip.End
                |> Menu.withTriggerTestId "comments_thread_assign"
        , alignment = Menu.End
        , items = items
        , toMsg = MenuMsg (assignMenuId thread.uuid)
        }
        (Maybe.withDefault Menu.init (Dict.get (assignMenuId thread.uuid) model.menus))


viewNewThread : AppState -> ViewConfig -> Model -> List (Html Msg)
viewNewThread appState cfg model =
    if cfg.readonly || not (Feature.projectCommentAdd appState cfg.questionnaire) then
        []

    else
        [ viewEditor appState
            { path = cfg.path
            , thread = Nothing
            , private = model.viewPrivate
            , value = Maybe.withDefault "" (Dict.get (inputKey cfg.path Nothing) model.inputs)
            , placeholder =
                if model.viewPrivate then
                    gettext "Write an editor's note…" appState.locale

                else
                    gettext "Write a comment…" appState.locale
            , send = gettext "Comment" appState.locale
            }
        ]


{-| How tall a box is: two lines to start with, then a line for every line
typed into it, up to a point past which the panel would be nothing else.

Lines, not rows: a box that measured what it had rendered would grow as the
words wrapped, and shrink again when the panel was made wider.

-}
linesIn : String -> Int
linesIn value =
    String.lines value
        |> List.length
        |> clamp 2 12


viewEditor : AppState -> { path : String, thread : Maybe Uuid, private : Bool, value : String, placeholder : String, send : String } -> Html Msg
viewEditor appState config =
    div [ class "c-questionnaire-comments__editor" ]
        [ -- A plain box, as the wizard had: a comment is written as text and
          -- shown as markdown, and a rich editor here would be a toolbar over
          -- two lines of prose.
          Field.toHtml
            (Field.textarea
                { id = "comment-" ++ inputKey config.path config.thread
                , label = config.placeholder
                , value = config.value
                , onInput = Typed config.path config.thread
                }
                |> Field.withHiddenLabel
                |> Field.withPlaceholder config.placeholder
                |> Field.withSubmitShortcut (Send config.path config.thread config.private)
                |> Field.withRows (linesIn config.value)
            )
        , if String.isEmpty (String.trim config.value) then
            text ""

          else
            let
                -- A new thread's box, or a reply's inside a thread.
                testIdPrefix =
                    case config.thread of
                        Just _ ->
                            "comments_reply"

                        Nothing ->
                            "comments_new-thread"
            in
            div [ class "c-questionnaire-comments__actions" ]
                [ Button.toHtml (Button.primary config.send (Send config.path config.thread config.private) |> Button.small |> Button.withTestId (testIdPrefix ++ "_submit"))
                , Button.toHtml (Button.ghost (gettext "Cancel" appState.locale) (Typed config.path config.thread "") |> Button.small |> Button.withTestId (testIdPrefix ++ "_cancel"))
                ]
        ]


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        ( title, question ) =
            case Maybe.andThen .comment model.deleting of
                Just _ ->
                    ( gettext "Delete comment" appState.locale, gettext "Are you sure you want to delete this comment?" appState.locale )

                Nothing ->
                    ( gettext "Delete thread" appState.locale, gettext "Are you sure you want to delete this thread and every comment in it?" appState.locale )
    in
    ConfirmModal.deleteWithTestId appState
        { title = title
        , open = Maybe.isJust model.deleting
        , question = question
        , entity = text ""
        , onConfirm = ConfirmDelete
        , onClose = CancelDelete
        , result = Unset
        , testId = Just "comments-delete"
        }
