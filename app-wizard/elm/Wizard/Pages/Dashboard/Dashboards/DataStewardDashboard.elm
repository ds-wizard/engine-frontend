module Wizard.Pages.Dashboard.Dashboards.DataStewardDashboard exposing
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
import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setCommentThreads, setPackages, setTemplates)
import Wizard.Api.CommentThreads as CommentThreadsApi
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Api.Packages as PackagesApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.AssignedComments as AssignedComments
import Wizard.Pages.Dashboard.Widgets.CreateKnowledgeModelWidget as CreateKnowledgeModelWidget
import Wizard.Pages.Dashboard.Widgets.CreateProjectTemplateWidget as CreateProjectTemplateWidget
import Wizard.Pages.Dashboard.Widgets.ImportDocumentTemplateWidget as ImportDocumentTemplateWidget
import Wizard.Pages.Dashboard.Widgets.ImportKnowledgeModelWidget as ImportKnowledgeModelWidget
import Wizard.Pages.Dashboard.Widgets.OutdatedPackagesWidget as OutdatedPackagesWidget
import Wizard.Pages.Dashboard.Widgets.OutdatedTemplatesWidget as OutdatedTemplatesWidget
import Wizard.Pages.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


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
    = GetPackagesCompleted (Result ApiError (Pagination Package))
    | GetDocumentTemplatesCompleted (Result ApiError (Pagination DocumentTemplate))
    | GetCommentThreadsCompleted (Result ApiError (Pagination QuestionnaireCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        packagesCmd =
            PackagesApi.getOutdatedPackages appState GetPackagesCompleted

        templatesCmd =
            DocumentTemplatesApi.getOutdatedTemplates appState GetDocumentTemplatesCompleted
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
        appState
        filters
        pagination
        GetCommentThreadsCompleted


update : msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update logoutMsg msg appState model =
    case msg of
        GetPackagesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setPackages
                , defaultError = gettext "Unable to get Knowledge Models." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                , locale = appState.locale
                }

        GetDocumentTemplatesCompleted result ->
            RequestHelpers.applyResultTransform
                { setResult = setTemplates
                , defaultError = gettext "Unable to get document templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , transform = .items
                , locale = appState.locale
                }

        GetCommentThreadsCompleted result ->
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
            , OutdatedPackagesWidget.view appState model.packages
            , OutdatedTemplatesWidget.view appState model.templates
            , CreateKnowledgeModelWidget.view appState
            , CreateProjectTemplateWidget.view appState
            , ImportKnowledgeModelWidget.view appState
            , ImportDocumentTemplateWidget.view appState
            ]
        ]
