module Wizard.Dashboard.Dashboards.AdminDashboard exposing
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
import Maybe.Extra as Maybe
import Shared.Api.CommentThreads as CommentThreadsApi
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Tenants as TenantsApi
import Shared.Common.UuidOrCurrent as UuidOrCurrent
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.BootstrapConfig.RegistryConfig as RegistryConfig
import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Data.Package exposing (Package)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Shared.Data.Usage exposing (Usage)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Setters exposing (setCommentThreads, setPackages, setTemplates, setUsage)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.Api exposing (applyResult, applyResultTransform)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.AddOpenIDWidget as AddOpenIDWidget
import Wizard.Dashboard.Widgets.AssignedComments as AssignedComments
import Wizard.Dashboard.Widgets.ConfigureLookAndFeelWidget as ConfigureLookAndFeel
import Wizard.Dashboard.Widgets.ConfigureOrganizationWidget as ConfigureOrganizationWidget
import Wizard.Dashboard.Widgets.ConnectRegistryWidget as ConnectRegistryWidget
import Wizard.Dashboard.Widgets.OutdatedPackagesWidget as OutdatedPackagesWidget
import Wizard.Dashboard.Widgets.OutdatedTemplatesWidget as OutdatedTemplatesWidget
import Wizard.Dashboard.Widgets.UsageWidget as UsageWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


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
    = GetUsageComplete (Result ApiError Usage)
    | GetPackagesComplete (Result ApiError (Pagination Package))
    | GetTemplatesComplete (Result ApiError (Pagination DocumentTemplate))
    | GetCommentThreadsComplete (Result ApiError (Pagination QuestionnaireCommentThreadAssigned))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        packagesCmd =
            PackagesApi.getOutdatedPackages appState GetPackagesComplete

        templatesCmd =
            DocumentTemplatesApi.getOutdatedTemplates appState GetTemplatesComplete

        usageCmd =
            TenantsApi.getTenantUsage UuidOrCurrent.current appState GetUsageComplete
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
        filters
        pagination
        appState
        GetCommentThreadsComplete


update : msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update logoutMsg msg appState model =
    case msg of
        GetUsageComplete result ->
            applyResult appState
                { setResult = setUsage
                , defaultError = gettext "Unable to get usage." appState.locale
                , model = model
                , result = result
                , logoutMsg = logoutMsg
                }

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
                |> listInsertIf (ConfigureOrganizationWidget.view appState) organizationWidgetVisible
                |> listInsertIf (ConfigureLookAndFeel.view appState) lookAndFeelWidgetVisible
                |> listInsertIf (ConnectRegistryWidget.view appState) registryWidgetVisible
                |> listInsertIf (AddOpenIDWidget.view appState) openIDWidgetVisible
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
