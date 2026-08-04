module Wizard.Pages.Dashboard.Widgets.UsageWidget exposing
    ( Model
    , Msg
    , UpdateConfig
    , enabled
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Data.WizardRolePermission as RolePermission
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setUsage)
import Gettext exposing (gettext)
import Html exposing (Html, h2, text)
import Html.Attributes exposing (class)
import Wizard.Api.Models.Usage exposing (Usage)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Components.UsageTable as UsageTable
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers


enabled : AppState -> Bool
enabled =
    AppState.userHasPerm RolePermission.settingsManage


type alias Model =
    { usage : ActionResult Usage
    }


initialModel : Model
initialModel =
    { usage = ActionResult.Loading
    }


type Msg
    = GetUsageCompleted (Result ApiError Usage)


fetchData : AppState -> Cmd Msg
fetchData appState =
    TenantsApi.getTenantUsage appState UuidOrCurrent.current GetUsageCompleted


type alias UpdateConfig msg =
    { locale : Gettext.Locale
    , logoutMsg : msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        GetUsageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setUsage
                , defaultError = gettext "Unable to get usage." cfg.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = cfg.locale
                }


view : AppState -> Model -> Html msg
view appState model =
    WidgetHelpers.widget <|
        case model.usage of
            ActionResult.Unset ->
                []

            ActionResult.Loading ->
                [ WidgetHelpers.widgetLoader ]

            ActionResult.Error error ->
                [ WidgetHelpers.widgetError error ]

            ActionResult.Success usageData ->
                [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Usage" appState.locale) ]
                , UsageTable.view appState False usageData
                ]
