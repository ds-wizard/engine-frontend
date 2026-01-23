module Wizard.Components.Questionnaire2.Components.CommentsRightPanel exposing
    ( Model
    , Msg
    , ViewCommentsOverviewProps
    , ViewQuestionCommentsProps
    , init
    , resetUserSuggestionDropdownModels
    , setViewPrivateAndResolved
    , subscriptions
    , update
    , viewCommentsOverview
    , viewQuestionComments
    )

import ActionResult exposing (ActionResult)
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Browser.Events
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (fa, faListingActions, faQuestionnaireComments, faQuestionnaireCommentsResolve)
import Common.Components.Tooltip exposing (tooltip, tooltipLeft)
import Common.Utils.Markdown as Markdown
import Common.Utils.ShortcutUtils as Shortcut
import Common.Utils.TimeUtils as TimeUtils
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, i, input, label, li, p, small, span, strong, text, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, placeholder, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Extra as Html
import Json.Decode as D
import List.Extensions as List
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Shortcut
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.ProjectDetail.Comment as Comment exposing (Comment)
import Wizard.Api.Models.ProjectDetail.CommentThread as CommentThread exposing (CommentThread)
import Wizard.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Api.Models.User as User
import Wizard.Components.Html exposing (resizableTextarea)
import Wizard.Components.Questionnaire.UserSuggestionDropdown as UserSuggestionDropdown
import Wizard.Components.Questionnaire2.QuestionnaireUpdateReturnData as QuestionnaireUpdateReturnData exposing (QuestionnaireUpdateReturnData)
import Wizard.Components.UserIcon as UserIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Utils.Feature as Feature


type alias Model =
    { uuid : Uuid
    , viewPrivate : Bool
    , commentInputs : Dict String String
    , commentEditInputs : Dict String String
    , commentDeleting : Maybe Uuid
    , commentDeletingListenClicks : Bool
    , commentsViewResolved : Bool
    , commentDropdownStates : Dict String Dropdown.State
    , userSuggestionDropdownModels : Dict String UserSuggestionDropdown.Model
    }


init : Uuid -> Model
init uuid =
    { uuid = uuid
    , viewPrivate = False
    , commentInputs = Dict.empty
    , commentEditInputs = Dict.empty
    , commentDeleting = Nothing
    , commentDeletingListenClicks = False
    , commentsViewResolved = False
    , commentDropdownStates = Dict.empty
    , userSuggestionDropdownModels = Dict.empty
    }


resetUserSuggestionDropdownModels : Model -> Model
resetUserSuggestionDropdownModels model =
    { model | userSuggestionDropdownModels = Dict.empty }


setViewPrivateAndResolved : Bool -> Bool -> Model -> Model
setViewPrivateAndResolved viewPrivate viewResolved model =
    { model | viewPrivate = viewPrivate, commentsViewResolved = viewResolved }


type Msg
    = UpdateViewPrivate Bool
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
    | CommentDropdownMsg String Dropdown.State
    | UserSuggestionDropdownMsg String Uuid Bool UserSuggestionDropdown.Msg


update : AppState -> Msg -> Model -> QuestionnaireUpdateReturnData Model Msg
update appState msg model =
    case msg of
        UpdateViewPrivate viewPrivate ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | viewPrivate = viewPrivate }

        CommentInput path mbThreadUuid value ->
            let
                key =
                    path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid

                commentInputs =
                    Dict.insert key value model.commentInputs
            in
            QuestionnaireUpdateReturnData.fromModel appState
                { model | commentInputs = commentInputs }

        CommentSubmit path mbThreadUuid text private ->
            let
                key =
                    path ++ "-" ++ Maybe.unwrap "0" Uuid.toString mbThreadUuid

                ( newThreadUuid, threadSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                ( commentUuid, commentSeed ) =
                    Random.step Uuid.uuidGenerator threadSeed

                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator commentSeed

                threadUuid =
                    Maybe.withDefault newThreadUuid mbThreadUuid

                newThread =
                    Maybe.isNothing mbThreadUuid

                event =
                    ProjectEvent.AddComment
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = threadUuid
                        , newThread = newThread
                        , commentUuid = commentUuid
                        , text = text
                        , private = private
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }

                commentInputs =
                    Dict.remove key model.commentInputs
            in
            { seed = eventSeed
            , model = { model | commentInputs = commentInputs }
            , cmd = Cmd.none
            , event = Just event
            }

        CommentEditInput commentUuid value ->
            let
                commentEditInputs =
                    Dict.insert (Uuid.toString commentUuid) value model.commentEditInputs
            in
            QuestionnaireUpdateReturnData.fromModel appState
                { model | commentEditInputs = commentEditInputs }

        CommentEditCancel commentUuid ->
            let
                commentEditInputs =
                    Dict.remove (Uuid.toString commentUuid) model.commentEditInputs
            in
            QuestionnaireUpdateReturnData.fromModel appState
                { model | commentEditInputs = commentEditInputs }

        CommentEditSubmit path threadUuid commentUuid text private ->
            let
                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                event =
                    ProjectEvent.EditComment
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = threadUuid
                        , commentUuid = commentUuid
                        , text = text
                        , private = private
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }

                commentEditInputs =
                    Dict.remove (Uuid.toString commentUuid) model.commentEditInputs
            in
            { seed = eventSeed
            , model = { model | commentEditInputs = commentEditInputs }
            , cmd = Cmd.none
            , event = Just event
            }

        CommentDelete mbCommentUuid ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | commentDeleting = mbCommentUuid, commentDeletingListenClicks = False }

        CommentDeleteListenClicks ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | commentDeletingListenClicks = True }

        CommentDeleteSubmit path threadUuid commentUuid private ->
            let
                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                event =
                    ProjectEvent.DeleteComment
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = threadUuid
                        , commentUuid = commentUuid
                        , private = private
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = eventSeed
            , model = model
            , cmd = Cmd.none
            , event = Just event
            }

        CommentDropdownMsg commentUuid state ->
            QuestionnaireUpdateReturnData.fromModel appState
                { model | commentDropdownStates = Dict.insert commentUuid state model.commentDropdownStates }

        UserSuggestionDropdownMsg uuid threadUuid editorNote userSuggestionDropdownMsg ->
            let
                ( userSuggestionModalModel, userSuggestionCmd ) =
                    Dict.get uuid model.userSuggestionDropdownModels
                        |> Maybe.withDefault (UserSuggestionDropdown.init model.uuid threadUuid editorNote)
                        |> UserSuggestionDropdown.update appState userSuggestionDropdownMsg
            in
            QuestionnaireUpdateReturnData.fromModelCmd appState
                { model | userSuggestionDropdownModels = Dict.insert uuid userSuggestionModalModel model.userSuggestionDropdownModels }
                (Cmd.map (UserSuggestionDropdownMsg uuid threadUuid editorNote) userSuggestionCmd)

        CommentThreadDelete path commentThread ->
            let
                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                event =
                    ProjectEvent.DeleteCommentThread
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = commentThread.uuid
                        , private = commentThread.private
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = eventSeed
            , model = model
            , cmd = Cmd.none
            , event = Just event
            }

        CommentThreadResolve path commentThread ->
            let
                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                event =
                    ProjectEvent.ResolveCommentThread
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = commentThread.uuid
                        , private = commentThread.private
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        , commentCount = List.length commentThread.comments
                        }
            in
            { seed = eventSeed
            , model = model
            , cmd = Cmd.none
            , event = Just event
            }

        CommentThreadReopen path commentThread ->
            let
                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                event =
                    ProjectEvent.ReopenCommentThread
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = commentThread.uuid
                        , private = commentThread.private
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        , commentCount = List.length commentThread.comments
                        }
            in
            { seed = eventSeed
            , model = model
            , cmd = Cmd.none
            , event = Just event
            }

        CommentThreadAssign path commentThread mbUser ->
            let
                ( eventUuid, eventSeed ) =
                    Random.step Uuid.uuidGenerator appState.seed

                event =
                    ProjectEvent.AssignCommentThread
                        { uuid = eventUuid
                        , path = path
                        , threadUuid = commentThread.uuid
                        , private = commentThread.private
                        , assignedTo = mbUser
                        , createdAt = appState.currentTime
                        , createdBy = Maybe.map UserConfig.toUserSuggestion appState.config.user
                        }
            in
            { seed = eventSeed
            , model = model
            , cmd = Cmd.none
            , event = Just event
            }


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

        userSuggestionDropdownSubs =
            Dict.toList model.userSuggestionDropdownModels
                |> List.map (\( uuid, userSuggestionModalModel ) -> Sub.map (UserSuggestionDropdownMsg uuid userSuggestionModalModel.threadUuid userSuggestionModalModel.editorNote) (UserSuggestionDropdown.subscriptions userSuggestionModalModel))
    in
    Sub.batch
        (commentDeleteSub :: commentDropdownSubs ++ userSuggestionDropdownSubs)


type alias ViewCommentsOverviewProps msg =
    { locale : Gettext.Locale
    , onOpenComments : String -> msg
    , onToggleCommentsViewResolved : Bool -> msg
    , questionnaire : ProjectQuestionnaire
    , viewResolved : Bool
    }


viewCommentsOverview : ViewCommentsOverviewProps msg -> Html msg
viewCommentsOverview props =
    let
        chapterComments group =
            let
                anyUnresolvedComments =
                    List.any (\c -> c.unresolvedComments > 0) group.comments
            in
            if not props.viewResolved && not anyUnresolvedComments then
                Html.nothing

            else
                div []
                    [ strong [] [ text group.chapter.title ]
                    , ul [ class "fa-ul" ] (List.map questionComments group.comments)
                    ]

        questionComments comment =
            if not props.viewResolved && comment.unresolvedComments == 0 then
                Html.nothing

            else
                let
                    resolvedCommentCount =
                        if props.viewResolved && comment.resolvedComments > 0 then
                            Badge.success [ class "rounded-pill ms-1" ]
                                [ fa "fas fa-check"
                                , text (String.fromInt comment.resolvedComments)
                                ]

                        else
                            Html.nothing
                in
                li []
                    [ span [ class "fa-li" ] [ fa "far fa-comment" ]
                    , a
                        [ onClick (props.onOpenComments comment.path)
                        , class "d-flex align-items-baseline"
                        ]
                        [ span [ class "flex-grow-1" ] [ text <| Question.getTitle comment.question ]
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
            ProjectQuestionnaire.getComments props.questionnaire

        commentsEmpty =
            if props.viewResolved then
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
                        (String.formatHtml (gettext "Click the %s icon to add new comments to a question." props.locale) [ faQuestionnaireComments ])
                    ]
                ]

            else
                List.map chapterComments (groupComments questionnaireComments)

        viewCommentsResolvedSelect_ =
            viewCommentsResolvedSelect
                { commentsViewResolved = props.viewResolved
                , locale = props.locale
                , onToggleCommentsViewResolved = props.onToggleCommentsViewResolved
                , questionnaire = props.questionnaire
                , viewResolved = props.viewResolved
                }
    in
    div
        [ class "questionnaireRightPanelList questionnaireRightPanelList--noPadding"
        , dataCy "question_comments_overview"
        ]
        [ viewCommentsResolvedSelect_
        , div [ class "p-3" ] content
        ]


type alias ViewQuestionCommentsProps msg =
    { onOpenComments : String -> msg
    , onToggleCommentsViewResolved : Bool -> msg
    , questionnaire : ProjectQuestionnaire
    , questionPath : String
    , viewResolved : Bool
    , wrapMsg : Msg -> msg
    }


viewQuestionComments : AppState -> ViewQuestionCommentsProps msg -> Model -> ActionResult (List CommentThread) -> Html msg
viewQuestionComments appState props model commentThreads =
    ActionResultBlock.view
        { viewContent = viewQuestionCommentsLoaded appState props model
        , actionResult = commentThreads
        , locale = appState.locale
        }


viewQuestionCommentsLoaded : AppState -> ViewQuestionCommentsProps msg -> Model -> List CommentThread -> Html msg
viewQuestionCommentsLoaded appState props model commentThreads =
    let
        filter =
            if props.viewResolved then
                always True

            else
                \thread -> thread.unresolvedComments > 0

        questionnaireComments =
            ProjectQuestionnaire.getComments props.questionnaire

        comments =
            questionnaireComments
                |> List.filter filter
                |> List.map .path

        nextPrevNavigation =
            if List.length comments > 1 then
                case List.elemIndex props.questionPath comments of
                    Just index ->
                        let
                            previousCommentsPath =
                                Maybe.withDefault "" <|
                                    List.findPreviousInfinite props.questionPath comments

                            nextCommentsPath =
                                Maybe.withDefault "" <|
                                    List.findNextInfinite props.questionPath comments

                            commentCountTooltip =
                                if props.viewResolved then
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
                            [ class "questionnaireComments__navigation"
                            ]
                            [ a
                                [ onClick (props.onOpenComments previousCommentsPath)
                                , dataCy "comments_nav_prev"
                                ]
                                [ fa "fas fa-arrow-left me-2"
                                , text (gettext "Previous" appState.locale)
                                ]
                            , numberText
                            , a
                                [ onClick (props.onOpenComments nextCommentsPath)
                                , dataCy "comments_nav_next"
                                ]
                                [ text (gettext "Next" appState.locale)
                                , fa "fas fa-arrow-right ms-2"
                                ]
                            ]

                    Nothing ->
                        Html.nothing

            else
                Html.nothing

        viewCommentsResolvedSelect_ =
            viewCommentsResolvedSelect
                { commentsViewResolved = props.viewResolved
                , locale = appState.locale
                , onToggleCommentsViewResolved = props.onToggleCommentsViewResolved
                , questionnaire = props.questionnaire
                , viewResolved = props.viewResolved
                }

        navigationView =
            if Feature.projectCommentPrivate appState props.questionnaire then
                viewCommentsNavigation appState props model commentThreads

            else
                Html.nothing

        editorNoteExplanation =
            Html.viewIf model.viewPrivate <|
                div [ class "alert alert-editor-notes" ]
                    [ i [ class "fa fas fa-lock" ] []
                    , span [ class "ms-2" ] [ text (gettext "Editor notes are only visible to project Editors and Owners." appState.locale) ]
                    ]

        viewFilteredCommentThreads condition =
            commentThreads
                |> List.filter (\thread -> thread.private == model.viewPrivate)
                |> List.filter condition
                |> List.sortWith CommentThread.compare
                |> List.map (viewCommentThread appState props model props.questionPath)

        resolvedThreadsView =
            Html.map props.wrapMsg <|
                if props.viewResolved then
                    div [] (viewFilteredCommentThreads (\thread -> thread.resolved))

                else
                    Html.nothing

        commentThreadsView =
            Html.map props.wrapMsg <|
                div [] (viewFilteredCommentThreads (\thread -> not thread.resolved))

        newThreadForm =
            Html.map props.wrapMsg <|
                div [ class "px-3 mb-5 mt-3" ]
                    [ viewCommentReplyForm appState
                        { submitText = gettext "Comment" appState.locale
                        , placeholderText = gettext "Create a new comment..." appState.locale
                        , model = model
                        , path = props.questionPath
                        , mbThreadUuid = Nothing
                        , private = model.viewPrivate
                        }
                    ]
    in
    div
        [ class "questionnaireRightPanelList questionnaireRightPanelList--noPadding questionnaireComments" ]
        [ nextPrevNavigation
        , viewCommentsResolvedSelect_
        , navigationView
        , resolvedThreadsView
        , commentThreadsView
        , editorNoteExplanation
        , newThreadForm
        ]


type alias ViewCommentsResolvedSelectProps msg =
    { commentsViewResolved : Bool
    , locale : Gettext.Locale
    , onToggleCommentsViewResolved : Bool -> msg
    , questionnaire : ProjectQuestionnaire
    , viewResolved : Bool
    }


viewCommentsResolvedSelect : ViewCommentsResolvedSelectProps msg -> Html msg
viewCommentsResolvedSelect props =
    let
        questionnaireComments =
            ProjectQuestionnaire.getComments props.questionnaire

        anyResolvedComments =
            List.any ((<) 0 << .resolvedComments) questionnaireComments
    in
    Html.viewIf anyResolvedComments <|
        div [ class "bg-light border-bottom px-3 py-2" ]
            [ label [ class "form-check-label form-check-toggle" ]
                [ input [ type_ "checkbox", class "form-check-input", onCheck props.onToggleCommentsViewResolved, checked props.commentsViewResolved ] []
                , span [] [ text (gettext "View resolved comments" props.locale) ]
                ]
            ]


viewCommentsNavigation : AppState -> ViewQuestionCommentsProps msg -> Model -> List CommentThread -> Html msg
viewCommentsNavigation appState props model commentThreads =
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
                Html.nothing

            else
                Badge.light [ class "rounded-pill" ] [ text (String.fromInt count) ]

        toResolvedBadge count =
            if props.viewResolved && count > 0 then
                Badge.success [ class "rounded-pill" ]
                    [ fa "fas fa-check"
                    , text (String.fromInt count)
                    ]

            else
                Html.nothing
    in
    ul [ class "nav nav-underline-tabs" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", not model.viewPrivate ) ]
                , onClick (props.wrapMsg (UpdateViewPrivate False))
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
                , classList [ ( "active", model.viewPrivate ) ]
                , onClick (props.wrapMsg (UpdateViewPrivate True))
                , dataCy "comments_nav_private-notes"
                ]
                [ span [ attribute "data-content" (gettext "Editor notes" appState.locale) ]
                    [ text (gettext "Editor notes" appState.locale) ]
                , toBadge privateThreadsCount
                , toResolvedBadge resolvedPrivateThreadsCount
                ]
            ]
        ]


viewCommentThread : AppState -> ViewQuestionCommentsProps msg -> Model -> String -> CommentThread -> Html Msg
viewCommentThread appState props model path commentThread =
    let
        comments =
            List.sortWith Comment.compare commentThread.comments

        deleteOverlay =
            if model.commentDeleting == Maybe.map .uuid (List.head comments) then
                viewCommentDeleteOverlay appState
                    { deleteMsg = CommentThreadDelete path commentThread
                    , deleteText = gettext "Delete this comment thread?" appState.locale
                    , extraClass = "questionnaireComments__deleteOverlay--thread"
                    }

            else
                Html.nothing

        replyForm =
            if commentThread.resolved then
                Html.nothing

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
                        [ class "questionnaireComments__commentThreadAssignedHeader"
                        , classList [ ( "questionnaireComments__commentThreadAssignedHeader--you", assignedToYou ) ]
                        ]
                        assignedContent

                Nothing ->
                    Html.nothing

        commentViews =
            List.indexedMap (viewComment appState props model path commentThread) comments
    in
    div
        [ class "questionnaireComments__commentThread"
        , classList
            [ ( "questionnaireComments__commentThread--resolved", commentThread.resolved )
            , ( "questionnaireComments__commentThread--private", commentThread.private )
            ]
        , attribute "data-comment-thread-uuid" (Uuid.toString commentThread.uuid)
        ]
        (assignedHeader
            :: commentViews
            ++ [ replyForm, deleteOverlay ]
        )


viewComment : AppState -> ViewQuestionCommentsProps msg -> Model -> String -> CommentThread -> Int -> Comment -> Html Msg
viewComment appState props model path commentThread index comment =
    let
        commentHeader =
            viewCommentHeader appState props model path commentThread index comment

        mbEditValue =
            Dict.get (Uuid.toString comment.uuid) model.commentEditInputs

        content =
            case mbEditValue of
                Just editValue ->
                    let
                        submitMsg =
                            CommentEditSubmit path commentThread.uuid comment.uuid editValue commentThread.private
                    in
                    Shortcut.shortcutElement
                        [ Shortcut.submitShortcut appState.navigator.isMac submitMsg
                        ]
                        []
                        [ resizableTextarea 2
                            editValue
                            [ class "form-control mb-1", onInput (CommentEditInput comment.uuid) ]
                            []
                        , div []
                            [ button
                                [ class "btn btn-primary btn-sm me-1"
                                , disabled (String.isEmpty editValue)
                                , onClick submitMsg
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
                    div [] [ Markdown.toHtml [ class "questionnaireComments__commentText" ] comment.text ]

        deleteOverlay =
            if index /= 0 && model.commentDeleting == Just comment.uuid then
                viewCommentDeleteOverlay appState
                    { deleteMsg = CommentDeleteSubmit path commentThread.uuid comment.uuid commentThread.private
                    , deleteText = gettext "Delete this comment?" appState.locale
                    , extraClass = "questionnaireComments__deleteOverlay--comment"
                    }

            else
                Html.nothing
    in
    div [ class "questionnaireComments__comment" ]
        [ commentHeader
        , content
        , deleteOverlay
        ]


viewCommentHeader : AppState -> ViewQuestionCommentsProps msg -> Model -> String -> CommentThread -> Int -> Comment -> Html Msg
viewCommentHeader appState props model path commentThread index comment =
    let
        resolveAction =
            if index == 0 && Feature.projectCommentThreadResolve appState props.questionnaire commentThread then
                a
                    ([ class "ms-1"
                     , onClick (CommentThreadResolve path commentThread)
                     , dataCy "comments_comment_resolve"
                     ]
                        ++ tooltipLeft (gettext "Resolve comment thread" appState.locale)
                    )
                    [ faQuestionnaireCommentsResolve ]

            else
                Html.nothing

        assignAction =
            if index == 0 && Feature.projectCommentThreadAssign appState props.questionnaire commentThread then
                let
                    viewConfig =
                        { wrapMsg = UserSuggestionDropdownMsg (Uuid.toString comment.uuid) commentThread.uuid commentThread.private
                        , selectMsg = CommentThreadAssign path commentThread << Just
                        }

                    userSuggestionDropdownModel =
                        Dict.get (Uuid.toString comment.uuid) model.userSuggestionDropdownModels
                            |> Maybe.withDefault (UserSuggestionDropdown.init props.questionnaire.uuid commentThread.uuid commentThread.private)
                in
                UserSuggestionDropdown.view viewConfig appState userSuggestionDropdownModel

            else
                Html.nothing

        removeAssignedAction =
            Dropdown.anchorItem
                [ onClick (CommentThreadAssign path commentThread Nothing) ]
                [ text (gettext "Remove assignment" appState.locale) ]

        removeAssignedActionVisible =
            index == 0 && Feature.projectCommentThreadRemoveAssign appState props.questionnaire commentThread

        reopenAction =
            Dropdown.anchorItem
                [ onClick (CommentThreadReopen path commentThread) ]
                [ text (gettext "Reopen" appState.locale) ]

        reopenActionVisible =
            index == 0 && Feature.projectCommentThreadReopen appState props.questionnaire commentThread

        editAction =
            Dropdown.anchorItem
                [ onClick (CommentEditInput comment.uuid comment.text) ]
                [ text (gettext "Edit" appState.locale) ]

        editActionVisible =
            Feature.projectCommentEdit appState props.questionnaire commentThread comment

        deleteAction =
            Dropdown.anchorItem
                [ onClick (CommentDelete (Just comment.uuid))
                , dataCy "comments_comment_menu_delete"
                , class "text-danger"
                ]
                [ text (gettext "Delete" appState.locale) ]

        deleteActionVisible =
            (index == 0 && Feature.projectCommentThreadDelete appState props.questionnaire commentThread)
                || (index /= 0 && Feature.projectCommentDelete appState props.questionnaire commentThread comment)

        actions =
            []
                |> List.insertIf removeAssignedAction removeAssignedActionVisible
                |> List.insertIf reopenAction reopenActionVisible
                |> List.insertIf editAction editActionVisible
                |> List.insertIf deleteAction deleteActionVisible

        dropdown =
            if List.isEmpty actions then
                Html.nothing

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
                            [ faListingActions ]
                    , items = actions
                    }

        createdLabel =
            TimeUtils.toReadableDateTime appState.timeZone comment.createdAt

        editedLabel =
            if comment.createdAt /= comment.updatedAt then
                span (tooltip (TimeUtils.toReadableDateTime appState.timeZone comment.updatedAt))
                    [ text <| " (" ++ gettext "edited" appState.locale ++ ")" ]

            else
                Html.nothing

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
    div [ class "questionnaireComments__commentHeader" ]
        [ UserIcon.view userForIcon
        , div [ class "user" ]
            [ strong [ class "d-block text-truncate" ]
                [ text (Maybe.unwrap (gettext "Anonymous user" appState.locale) User.fullName comment.createdBy)
                ]
            , small [ class "text-muted" ] [ text createdLabel, editedLabel ]
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

        ( newThreadFormSubmit, shortcuts ) =
            if String.isEmpty commentValue then
                ( Html.nothing, [] )

            else
                let
                    submitMsg =
                        CommentSubmit path mbThreadUuid commentValue private
                in
                ( div []
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
                , [ Shortcut.submitShortcut appState.navigator.isMac submitMsg ]
                )
    in
    Shortcut.shortcutElement shortcuts
        [ class "questionnaireComments__commentReplyForm", classList [ ( "questionnaireComments__commentReplyForm--private", private ) ] ]
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
    div [ class "questionnaireComments__deleteOverlay", class extraClass ]
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
