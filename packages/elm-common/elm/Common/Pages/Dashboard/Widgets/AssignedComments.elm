module Common.Pages.Dashboard.Widgets.AssignedComments exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

{-| The latest unresolved comment threads assigned to the current user.

As in the wizard, the widget is not shown while it loads or when there is
nothing assigned: an empty "assigned comments" box on every dashboard would be
noise. A failure is shown, so it does not look like there is nothing.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Common.Api.ProjectCommentThreads as ProjectCommentThreadsApi
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setCommentThreads)
import Gettext exposing (gettext)
import Html
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Widget as Widget exposing (Widget)
import Uuid


type alias Model =
    { commentThreads : ActionResult (List ProjectCommentThreadAssigned)
    }


initialModel : Model
initialModel =
    { commentThreads = ActionResult.Loading
    }


type Msg
    = GetCommentThreadsCompleted (Result ApiError (Pagination ProjectCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        pagination =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "updatedAt") PaginationQueryString.SortDESC
                |> PaginationQueryString.withSize (Just 3)

        filters =
            PaginationQueryFilters.create
                [ ( "resolved", Just "false" ) ]
                []
    in
    ProjectCommentThreadsApi.getCommentThreads
        appState
        filters
        pagination
        GetCommentThreadsCompleted


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetCommentThreadsCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setCommentThreads
                , defaultError = gettext "Unable to get assigned comments." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = appState.locale
                }


view : AppState -> Model -> Maybe (Widget msg)
view appState model =
    let
        title =
            gettext "Unresolved Assigned Comments" appState.locale
    in
    case model.commentThreads of
        Error error ->
            Just (Widget.widget { title = title, body = [ Alert.toHtml (Alert.danger error) ] })

        Success commentThreads ->
            if List.isEmpty commentThreads then
                Nothing

            else
                Widget.widget
                    { title = title
                    , body = [ ListItem.list (List.map (viewCommentThread appState) commentThreads) ]
                    }
                    |> withViewAll appState Routes.commentsIndex
                    |> Just

        _ ->
            Nothing


withViewAll : AppState -> Routes.Route -> Widget msg -> Widget msg
withViewAll appState route =
    if Routes.isReachable appState route then
        Widget.withAction (Button.link (gettext "View all" appState.locale) (Routes.toUrl appState route))

    else
        identity


viewCommentThread : AppState -> ProjectCommentThreadAssigned -> ListItem.ListItem msg
viewCommentThread appState commentThread =
    let
        url =
            Routes.toUrlIn appState commentThread.workspaceUuid (Routes.projectsDetailQuestionnaire commentThread.projectUuid (Just commentThread.path) (Just commentThread.commentThreadUuid))
    in
    ListItem.item (Link.toHtml (Link.link { label = commentThread.text, url = url }))
        |> ListItem.withIcon (ItemIcon.toHtml (UserAvatar.item commentThread.projectName Nothing))
        |> ListItem.withTestId ("assigned-comments_item_" ++ Uuid.toString commentThread.commentThreadUuid)
        |> ListItem.withFragments
            [ Html.text commentThread.projectName
            , Timestamp.viewUpdated appState commentThread.updatedAt
            ]
        |> ListItem.withRowLink
