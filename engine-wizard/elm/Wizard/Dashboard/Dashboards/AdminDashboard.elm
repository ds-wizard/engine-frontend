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
import Shared.Api.Packages as PackagesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Api.Usage as UsageApi
import Shared.Data.BootstrapConfig.RegistryConfig as RegistryConfig
import Shared.Data.Package exposing (Package)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Template exposing (Template)
import Shared.Data.Usage exposing (Usage)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Setters exposing (setPackages, setTemplates, setUsage)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.Api exposing (applyResult, applyResultTransform)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.AddOpenIDWidget as AddOpenIDWidget
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
    , templates : ActionResult (List Template)
    }


initialModel : Model
initialModel =
    { usage = ActionResult.Loading
    , packages = ActionResult.Loading
    , templates = ActionResult.Loading
    }


type Msg
    = GetUsageComplete (Result ApiError Usage)
    | GetPackagesComplete (Result ApiError (Pagination Package))
    | GetTemplatesComplete (Result ApiError (Pagination Template))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        packagesCmd =
            PackagesApi.getOutdatedPackages appState GetPackagesComplete

        templatesCmd =
            TemplatesApi.getOutdatedTemplates appState GetTemplatesComplete

        usageCmd =
            UsageApi.getUsage appState GetUsageComplete
    in
    Cmd.batch [ packagesCmd, templatesCmd, usageCmd ]


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


view : AppState -> Model -> Html msg
view appState model =
    let
        organizationWidgetVisible =
            appState.config.organization.name == "My Organization" || appState.config.organization.organizationId == "myorg"

        lookAndFeelWidgetVisible =
            Maybe.isNothing appState.config.lookAndFeel.appTitle || Maybe.isNothing appState.config.lookAndFeel.appTitleShort

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
                :: OutdatedPackagesWidget.view appState model.packages
                :: OutdatedTemplatesWidget.view appState model.templates
                :: ctaWidgets
                ++ [ UsageWidget.view appState model.usage
                   ]
            )
        ]
