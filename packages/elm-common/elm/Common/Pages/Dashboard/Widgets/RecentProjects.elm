module Common.Pages.Dashboard.Widgets.RecentProjects exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

{-| The current user's three most recently updated projects (not templates).

Always shown: loading, failed, empty — the empty state is the nudge towards
creating the first project, which the "Create Project" widget next to it offers.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Project exposing (Project)
import Common.Api.Projects as ProjectsApi
import Common.Components.Timestamp as Timestamp
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Common.Routes as Routes
import Common.Utils.Bool as Bool
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setProjects)
import Gettext exposing (gettext)
import Ui.Button as Button
import Ui.EmptyState as EmptyState
import Ui.ItemIcon as ItemIcon
import Ui.Link as Link
import Ui.ListItem as ListItem
import Ui.Widget as Widget exposing (Widget)
import Uuid


type alias Model =
    { projects : ActionResult (List Project)
    }


initialModel : Model
initialModel =
    { projects = ActionResult.Loading
    }


type Msg
    = GetProjectsComplete (Result ApiError (Pagination Project))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        pagination =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "updatedAt") PaginationQueryString.SortDESC
                |> PaginationQueryString.withSize (Just 3)

        mbUserUuid =
            Maybe.map (Uuid.toString << .uuid) appState.config.user

        filters =
            PaginationQueryFilters.create
                [ ( "isTemplate", Just (Bool.toString False) )
                , ( "userUuids", mbUserUuid )
                ]
                []
    in
    ProjectsApi.getList appState
        filters
        pagination
        GetProjectsComplete


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetProjectsComplete result ->
            RequestHelpers.applyResultTransform
                { setResult = setProjects
                , defaultError = gettext "Unable to get projects." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = appState.locale
                }


view : AppState -> Model -> Widget msg
view appState model =
    let
        withViewAll =
            case model.projects of
                Success (_ :: _) ->
                    if Routes.isReachable appState (Routes.projectsIndex appState) then
                        Widget.withAction (Button.link (gettext "View all" appState.locale) (Routes.toUrl appState (Routes.projectsIndex appState)))

                    else
                        identity

                _ ->
                    identity

        viewContent projects =
            if List.isEmpty projects then
                [ EmptyState.emptyState
                    { title = gettext "No projects yet" appState.locale
                    , description = gettext "You have no projects yet, start by creating some." appState.locale
                    }
                    |> EmptyState.withHeadingLevel 3
                    |> EmptyState.toHtml
                ]

            else
                [ ListItem.list (List.map (viewProject appState) projects) ]
    in
    Widget.widget
        { title = gettext "Recent Projects" appState.locale
        , body =
            [ WidgetHelpers.asyncBody appState viewContent model.projects ]
        }
        |> withViewAll


viewProject : AppState -> Project -> ListItem.ListItem msg
viewProject appState project =
    ListItem.item (Link.toHtml (Link.link { label = project.name, url = Routes.toUrlIn appState project.workspaceUuid (Routes.projectsDetail project.uuid) }))
        |> ListItem.withIcon (ItemIcon.toHtml (UserAvatar.item project.name Nothing))
        |> ListItem.withFragments [ Timestamp.viewUpdated appState project.updatedAt ]
        |> ListItem.withTestId ("recent-projects_item_" ++ Uuid.toString project.uuid)
        |> ListItem.withRowLink
