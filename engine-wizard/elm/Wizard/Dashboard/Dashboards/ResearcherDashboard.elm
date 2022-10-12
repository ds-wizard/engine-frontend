module Wizard.Dashboard.Dashboards.ResearcherDashboard exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Auth.Session as Session
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.CreateProjectWidget as CreateProjectWidget
import Wizard.Dashboard.Widgets.RecentProjectsWidget as RecentProjectsWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { questionnaires : ActionResult (List Questionnaire) }


initialModel : Model
initialModel =
    { questionnaires = ActionResult.Loading }


type Msg
    = GetQuestionnairesComplete (Result ApiError (Pagination Questionnaire))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        pagination =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "updatedAt") PaginationQueryString.SortDESC
                |> PaginationQueryString.withSize (Just 3)

        mbUserUuid =
            Session.getUserUuid appState.session
    in
    QuestionnairesApi.getQuestionnaires
        { isTemplate = Just False
        , userUuids = mbUserUuid
        , userUuidsOp = Nothing
        , projectTags = Nothing
        , projectTagsOp = Nothing
        , packageIds = Nothing
        , packageIdsOp = Nothing
        }
        pagination
        appState
        GetQuestionnairesComplete


update : Msg -> AppState -> Model -> Model
update (GetQuestionnairesComplete result) appState model =
    case result of
        Ok data ->
            { model | questionnaires = ActionResult.Success data.items }

        Err error ->
            { model | questionnaires = ApiError.toActionResult appState (lg "apiError.questionnaires.getListError" appState) error }


view : AppState -> Model -> Html msg
view appState model =
    div []
        [ div [ class "row gx-3" ]
            [ WelcomeWidget.view appState
            , RecentProjectsWidget.view appState model.questionnaires
            , CreateProjectWidget.view appState
            ]
        ]
