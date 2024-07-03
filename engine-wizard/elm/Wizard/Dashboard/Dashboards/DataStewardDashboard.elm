module Wizard.Dashboard.Dashboards.DataStewardDashboard exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Api.CommentThreads as CommentThreadsApi
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Api.Packages as PackagesApi
import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Data.Package exposing (Package)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Setters exposing (setCommentThreads, setPackages, setTemplates)
import Wizard.Common.Api exposing (applyResultTransform)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.AssignedComments as AssignedComments
import Wizard.Dashboard.Widgets.CreateKnowledgeModelWidget as CreateKnowledgeModelWidget
import Wizard.Dashboard.Widgets.CreateProjectTemplateWidget as CreateProjectTemplateWidget
import Wizard.Dashboard.Widgets.ImportDocumentTemplateWidget as ImportDocumentTemplateWidget
import Wizard.Dashboard.Widgets.ImportKnowledgeModelWidget as ImportKnowledgeModelWidget
import Wizard.Dashboard.Widgets.OutdatedPackagesWidget as OutdatedPackagesWidget
import Wizard.Dashboard.Widgets.OutdatedTemplatesWidget as OutdatedTemplatesWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { packages : ActionResult (List Package)
    , templates : ActionResult (List DocumentTemplate)
    , commentThreads : ActionResult (List QuestionnaireCommentThreadAssigned)
    }


initialModel : Model
initialModel =
    { packages = ActionResult.Loading
    , templates = ActionResult.Loading
    , commentThreads = ActionResult.Loading
    }


type Msg
    = GetPackagesComplete (Result ApiError (Pagination Package))
    | GetTemplatesComplete (Result ApiError (Pagination DocumentTemplate))
    | GetCommentThreadsComplete (Result ApiError (Pagination QuestionnaireCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        packagesCmd =
            PackagesApi.getOutdatedPackages appState GetPackagesComplete

        templatesCmd =
            DocumentTemplatesApi.getOutdatedTemplates appState GetTemplatesComplete
    in
    Cmd.batch [ packagesCmd, templatesCmd, fetchCommentThreads appState ]


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
        filters
        pagination
        appState
        GetCommentThreadsComplete


update : msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update logoutMsg msg appState model =
    case msg of
        GetPackagesComplete result ->
            applyResultTransform appState
                { setResult = setPackages
                , defaultError = gettext "Unable to get Knowledge Models." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                }

        GetTemplatesComplete result ->
            applyResultTransform appState
                { setResult = setTemplates
                , defaultError = gettext "Unable to get document templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                }

        GetCommentThreadsComplete result ->
            applyResultTransform appState
                { setResult = setCommentThreads
                , defaultError = gettext "Unable to get assigned comments." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                }


view : AppState -> Model -> Html msg
view appState model =
    div []
        [ div [ class "row gx-3" ]
            [ WelcomeWidget.view appState
            , AssignedComments.view appState model.commentThreads
            , OutdatedPackagesWidget.view appState model.packages
            , OutdatedTemplatesWidget.view appState model.templates
            , CreateKnowledgeModelWidget.view appState
            , CreateProjectTemplateWidget.view appState
            , ImportKnowledgeModelWidget.view appState
            , ImportDocumentTemplateWidget.view appState
            ]
        ]
