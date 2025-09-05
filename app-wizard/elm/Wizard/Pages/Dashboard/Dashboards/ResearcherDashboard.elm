module Wizard.Pages.Dashboard.Dashboards.ResearcherDashboard exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Bool.Extra as Bool
import Common.Data.ApiError exposing (ApiError)
import Common.Data.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setCommentThreads, setQuestionnaires)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Uuid
import Wizard.Api.CommentThreads as CommentThreadsApi
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.AssignedComments as AssignedComments
import Wizard.Pages.Dashboard.Widgets.CreateProjectWidget as CreateProjectWidget
import Wizard.Pages.Dashboard.Widgets.RecentProjectsWidget as RecentProjectsWidget
import Wizard.Pages.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    , commentThreads : ActionResult (List QuestionnaireCommentThreadAssigned)
    }


initialModel : Model
initialModel =
    { questionnaires = ActionResult.Loading
    , commentThreads = ActionResult.Loading
    }


type Msg
    = GetQuestionnairesComplete (Result ApiError (Pagination Questionnaire))
    | GetCommentThreadsComplete (Result ApiError (Pagination QuestionnaireCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    Cmd.batch [ fetchQuestionnaires appState, fetchCommentThreads appState ]


fetchQuestionnaires : AppState -> Cmd Msg
fetchQuestionnaires appState =
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
    QuestionnairesApi.getQuestionnaires appState
        filters
        pagination
        GetQuestionnairesComplete


fetchCommentThreads : AppState -> Cmd Msg
fetchCommentThreads appState =
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
    CommentThreadsApi.getCommentThreads
        appState
        filters
        pagination
        GetCommentThreadsComplete


update : msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update logoutMsg msg appState model =
    case msg of
        GetQuestionnairesComplete result ->
            RequestHelpers.applyResultTransform
                { setResult = setQuestionnaires
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
            , RecentProjectsWidget.view appState model.questionnaires
            , CreateProjectWidget.view appState
            ]
        ]
