module Wizard.Pages.Dashboard.Dashboards.AdminDashboard exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiErrorOld exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setCommentThreads, setPackages, setTemplates, setUsage)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List.Extensions as List
import Maybe.Extra as Maybe
import Wizard.Api.CommentThreads as CommentThreadsApi
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Api.Models.Package exposing (Package)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Api.Models.Usage exposing (Usage)
import Wizard.Api.Packages as PackagesApi
import Wizard.Api.Tenants as TenantsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.AddOpenIDWidget as AddOpenIDWidget
import Wizard.Pages.Dashboard.Widgets.AssignedComments as AssignedComments
import Wizard.Pages.Dashboard.Widgets.ConfigureLookAndFeelWidget as ConfigureLookAndFeel
import Wizard.Pages.Dashboard.Widgets.ConfigureOrganizationWidget as ConfigureOrganizationWidget
import Wizard.Pages.Dashboard.Widgets.ConnectRegistryWidget as ConnectRegistryWidget
import Wizard.Pages.Dashboard.Widgets.OutdatedPackagesWidget as OutdatedPackagesWidget
import Wizard.Pages.Dashboard.Widgets.OutdatedTemplatesWidget as OutdatedTemplatesWidget
import Wizard.Pages.Dashboard.Widgets.UsageWidget as UsageWidget
import Wizard.Pages.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { usage : ActionResult Usage
    , packages : ActionResult (List Package)
    , templates : ActionResult (List DocumentTemplate)
    , commentThreads : ActionResult (List QuestionnaireCommentThreadAssigned)
    }


initialModel : Model
initialModel =
    { usage = ActionResult.Loading
    , packages = ActionResult.Loading
    , templates = ActionResult.Loading
    , commentThreads = ActionResult.Loading
    }


type Msg
    = GetUsageCompleted (Result ApiErrorOld.ApiError Usage)
    | GetPackagesCompleted (Result ApiError (Pagination Package))
    | GetTemplatesCompleted (Result ApiError (Pagination DocumentTemplate))
    | GetCommentThreadsCompleted (Result ApiError (Pagination QuestionnaireCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        packagesCmd =
            PackagesApi.getOutdatedPackages appState GetPackagesCompleted

        templatesCmd =
            DocumentTemplatesApi.getOutdatedTemplates appState GetTemplatesCompleted

        usageCmd =
            TenantsApi.getTenantUsage appState UuidOrCurrent.current GetUsageCompleted
    in
    Cmd.batch [ packagesCmd, templatesCmd, usageCmd, fetchCommentThreads appState ]


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
        GetUsageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setUsage
                , defaultError = gettext "Unable to get usage." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                , locale = appState.locale
                }

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

        GetTemplatesCompleted result ->
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
    let
        organizationWidgetVisible =
            appState.config.organization.name == "My Organization" || appState.config.organization.organizationId == "myorg"

        lookAndFeelWidgetVisible =
            not (Admin.isEnabled appState.config.admin)
                && (Maybe.isNothing appState.config.lookAndFeel.appTitle || Maybe.isNothing appState.config.lookAndFeel.appTitleShort)

        registryWidgetVisible =
            not <| RegistryConfig.isEnabled appState.config.registry

        openIDWidgetVisible =
            List.isEmpty appState.config.authentication.external.services

        ctaWidgets =
            []
                |> List.insertIf (ConfigureOrganizationWidget.view appState) organizationWidgetVisible
                |> List.insertIf (ConfigureLookAndFeel.view appState) lookAndFeelWidgetVisible
                |> List.insertIf (ConnectRegistryWidget.view appState) registryWidgetVisible
                |> List.insertIf (AddOpenIDWidget.view appState) openIDWidgetVisible
    in
    div []
        [ div [ class "row gx-3" ]
            (WelcomeWidget.view appState
                :: AssignedComments.view appState model.commentThreads
                :: OutdatedPackagesWidget.view appState model.packages
                :: OutdatedTemplatesWidget.view appState model.templates
                :: ctaWidgets
                ++ [ UsageWidget.view appState model.usage
                   ]
            )
        ]
