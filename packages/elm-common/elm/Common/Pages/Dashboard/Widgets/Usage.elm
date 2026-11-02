module Common.Pages.Dashboard.Widgets.Usage exposing
    ( Model
    , Msg
    , enabled
    , fetchData
    , initialModel
    , update
    , view
    )

{-| How much of each of the instance's limits is used, for those who manage the
settings.
-}

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Usage exposing (Usage)
import Common.Api.Tenants as TenantsApi
import Common.Components.UsageTable as UsageTable
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Data.WizardRolePermission as RolePermission
import Common.Pages.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setUsage)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Widget as Widget exposing (Widget)


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


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUsageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setUsage
                , defaultError = gettext "Unable to get usage." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }


view : AppState -> Model -> Widget msg
view appState model =
    Widget.widget
        { title = gettext "Usage" appState.locale
        , body = [ WidgetHelpers.asyncBody appState (List.singleton << viewUsage appState) model.usage ]
        }


viewUsage : AppState -> Usage -> Html msg
viewUsage appState usage =
    UsageTable.view appState
        { showSoftLimits = False
        , rows =
            [ UsageTable.count "users" (gettext "Users" appState.locale) usage.users
            , UsageTable.count "active-users" (gettext "Active Users" appState.locale) usage.activeUsers
            , UsageTable.count "knowledge-model-editors" (gettext "Knowledge Model Editors" appState.locale) usage.knowledgeModelEditors
            , UsageTable.count "knowledge-models" (gettext "Knowledge Models" appState.locale) usage.knowledgeModels
            , UsageTable.count "document-template-editors" (gettext "Document Template Editors" appState.locale) usage.documentTemplateDrafts
            , UsageTable.count "document-templates" (gettext "Document Templates" appState.locale) usage.documentTemplates
            , UsageTable.count "projects" (gettext "Projects" appState.locale) usage.projects
            , UsageTable.count "documents" (gettext "Documents" appState.locale) usage.documents
            , UsageTable.bytes "storage" (gettext "Storage" appState.locale) usage.storage
            ]
        }
