module Wizard.Dashboard.Dashboards.AdminDashboard exposing
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
import Maybe.Extra as Maybe
import Shared.Api.Usage as UsageApi
import Shared.Data.BootstrapConfig.RegistryConfig as RegistryConfig
import Shared.Data.Usage exposing (Usage)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (listInsertIf)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.AddOpenIDWidget as AddOpenIDWidget
import Wizard.Dashboard.Widgets.ConfigureLookAndFeelWidget as ConfigureLookAndFeel
import Wizard.Dashboard.Widgets.ConfigureOrganizationWidget as ConfigureOrganizationWidget
import Wizard.Dashboard.Widgets.ConnectRegistryWidget as ConnectRegistryWidget
import Wizard.Dashboard.Widgets.UsageWidget as UsageWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { usage : ActionResult Usage }


initialModel : Model
initialModel =
    { usage = ActionResult.Loading }


type Msg
    = GetUsageComplete (Result ApiError Usage)


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsageApi.getUsage appState GetUsageComplete


update : Msg -> AppState -> Model -> Model
update (GetUsageComplete result) appState model =
    case result of
        Ok data ->
            { model | usage = ActionResult.Success data }

        Err error ->
            { model | usage = ApiError.toActionResult appState (lg "apiError.usage.getError" appState) error }


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
                :: ctaWidgets
                ++ [ UsageWidget.view appState model.usage
                   ]
            )
        ]
