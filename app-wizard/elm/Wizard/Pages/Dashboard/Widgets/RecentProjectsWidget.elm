module Wizard.Pages.Dashboard.Widgets.RecentProjectsWidget exposing
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
import Common.Components.FontAwesome exposing (faArrowRight)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Utils.Bool as Bool
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setProjects)
import Common.Utils.TimeDistance exposing (locale)
import Gettext exposing (gettext)
import Html exposing (Html, br, div, h2, p, strong, text)
import Html.Attributes exposing (class)
import String.Format as String
import Time.Distance exposing (inWordsWithConfig)
import Uuid
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Html exposing (linkTo)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


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


type alias UpdateConfig msg =
    { locale : Gettext.Locale
    , logoutMsg : msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        GetProjectsComplete result ->
            RequestHelpers.applyResultTransform
                { setResult = setProjects
                , defaultError = gettext "Unable to get projects." cfg.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , transform = .items
                , locale = cfg.locale
                }


view : AppState -> Model -> Html msg
view appState model =
    WidgetHelpers.widget <|
        case model.projects of
            Unset ->
                []

            Loading ->
                [ WidgetHelpers.widgetLoader ]

            Error error ->
                [ WidgetHelpers.widgetError error ]

            Success projectList ->
                if List.isEmpty projectList then
                    viewRecentProjectsEmpty appState

                else
                    viewRecentProjects appState projectList


viewRecentProjects : AppState -> List Project -> List (Html msg)
viewRecentProjects appState projects =
    [ div [ class "RecentProjectsWidget d-flex flex-column h-100" ]
        [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Recent Projects" appState.locale) ]
        , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewProject appState) projects)
        , div [ class "mt-4" ]
            [ linkTo (Routes.projectsIndex appState) [] [ text (gettext "View all" appState.locale) ] ]
        ]
    ]


viewProject : AppState -> Project -> Html msg
viewProject appState project =
    let
        updatedText =
            inWordsWithConfig { withAffix = True } (locale appState.locale) project.updatedAt appState.currentTime
    in
    linkTo (Routes.projectsDetail project.uuid)
        [ class "p-2 py-3 d-flex rounded-3" ]
        [ ItemIcon.view { text = project.name, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text project.name ]
            , div [ class "d-flex align-items-center" ]
                [ div [ class "flex-grow-1 text-lighter fst-italic" ] [ text (String.format (gettext "Updated %s" appState.locale) [ updatedText ]) ]
                ]
            ]
        ]


viewRecentProjectsEmpty : AppState -> List (Html msg)
viewRecentProjectsEmpty appState =
    [ div [ class "text-lighter d-flex flex-column justify-content-center h-100" ]
        [ p [ class "fs-5 m-0 mt-3 text-center" ]
            [ text (gettext "You have no projects yet, start by creating some." appState.locale)
            , br [] []
            , faArrowRight
            ]
        ]
    ]
