module Wizard.Pages.Dashboard.Widgets.AssignedCommentsWidget exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setCommentThreads)
import Common.Utils.TimeDistance exposing (locale)
import Gettext exposing (gettext)
import Html exposing (Html, div, h2, strong, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import String.Format as String
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Api.Models.ProjectCommentThreadAssigned exposing (ProjectCommentThreadAssigned)
import Wizard.Api.ProjectCommentThreads as ProjectCommentThreadsApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


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


type alias UpdateConfig msg =
    { locale : Gettext.Locale
    , logoutMsg : msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        GetCommentThreadsCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setCommentThreads
                , defaultError = gettext "Unable to get assigned comments." cfg.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = cfg.locale
                }


view : AppState -> Model -> Html msg
view appState model =
    case model.commentThreads of
        Unset ->
            Html.nothing

        Loading ->
            Html.nothing

        Error error ->
            WidgetHelpers.widget <| [ WidgetHelpers.widgetError error ]

        Success commentThreadList ->
            if List.isEmpty commentThreadList then
                Html.nothing

            else
                WidgetHelpers.widget <| viewCommentThreads appState commentThreadList


viewCommentThreads : AppState -> List ProjectCommentThreadAssigned -> List (Html msg)
viewCommentThreads appState commentThread =
    [ div [ class "RecentProjectsWidget d-flex flex-column h-100" ]
        [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Unresolved Assigned Comments" appState.locale) ]
        , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewCommentThread appState) commentThread)
        , div [ class "mt-4" ]
            [ linkTo Routes.commentsIndex
                []
                [ text (gettext "View all" appState.locale) ]
            ]
        ]
    ]


viewCommentThread : AppState -> ProjectCommentThreadAssigned -> Html msg
viewCommentThread appState commentThread =
    let
        updatedText =
            inWordsWithConfig { withAffix = True } (locale appState.locale) commentThread.updatedAt appState.currentTime
    in
    linkTo (Routes.projectsDetailQuestionnaire commentThread.projectUuid (Just commentThread.path) (Just commentThread.commentThreadUuid))
        [ class "p-2 py-3 d-flex rounded-3" ]
        [ ItemIcon.view { text = commentThread.projectName, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text commentThread.text ]
            , div [ class "d-flex align-items-center" ]
                [ div [ class "flex-grow-1 text-lighter fst-italic" ] [ text (String.format (gettext "Updated %s" appState.locale) [ updatedText ]) ]
                ]
            ]
        ]
