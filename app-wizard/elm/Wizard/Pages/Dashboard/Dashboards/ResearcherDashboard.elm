module Wizard.Pages.Dashboard.Dashboards.ResearcherDashboard exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Utils.Bool as Bool
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setCommentThreads, setProjects)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Uuid
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Models.ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Wizard.Api.ProjectCommentThreads as ProjectCommentThreadsApi
import Wizard.Api.Projects as ProjectsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.AssignedComments as AssignedComments
import Wizard.Pages.Dashboard.Widgets.CreateProjectWidget as CreateProjectWidget
import Wizard.Pages.Dashboard.Widgets.RecentProjectsWidget as RecentProjectsWidget
import Wizard.Pages.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { projects : ActionResult (List Project)
    , commentThreads : ActionResult (List ProjectCommentThreadAssigned)
    }


initialModel : Model
initialModel =
    { projects = ActionResult.Loading
    , commentThreads = ActionResult.Loading
    }


type Msg
    = GetProjectsComplete (Result ApiError (Pagination Project))
    | GetCommentThreadsComplete (Result ApiError (Pagination ProjectCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch [ fetchProjects appState, fetchProjectCommentThreads appState ]


fetchProjects : AppState -> Cmd Msg
fetchProjects appState =
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


fetchProjectCommentThreads : AppState -> Cmd Msg
fetchProjectCommentThreads appState =
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
        GetCommentThreadsComplete


update : msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update logoutMsg msg appState model =
    case msg of
        GetProjectsComplete result ->
            RequestHelpers.applyResultTransform
                { setResult = setProjects
                , defaultError = gettext "Unable to get projects." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                , locale = appState.locale
                }

        GetCommentThreadsComplete result ->
            RequestHelpers.applyResultTransform
                { setResult = setCommentThreads
                , defaultError = gettext "Unable to get assigned comments." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                , locale = appState.locale
                }


view : AppState -> Model -> Html msg
view appState model =
    div []
        [ div [ class "row gx-3" ]
            [ WelcomeWidget.view appState
            , AssignedComments.view appState model.commentThreads
            , RecentProjectsWidget.view appState model.projects
            , CreateProjectWidget.view appState
            ]
        ]
