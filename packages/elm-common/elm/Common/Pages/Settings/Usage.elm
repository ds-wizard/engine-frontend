module Common.Pages.Settings.Usage exposing
    ( Model
    , Msg
    , gotUsage
    , init
    , initialModel
    , update
    , view
    , viewWith
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Usage exposing (Usage)
import Common.Api.Tenants as TenantsApi
import Common.Components.UsageTable as UsageTable
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Layout as Layout


type alias Model =
    { usage : ActionResult Usage }


type Msg
    = GetUsageComplete (Result ApiError Usage)


initialModel : Model
initialModel =
    { usage = Loading }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel
    , TenantsApi.getTenantUsage appState UuidOrCurrent.current GetUsageComplete
    )


{-| The usage, as loaded by a page that shows this one with more of its own
from the same answer.
-}
gotUsage : Result ApiError Usage -> Msg
gotUsage =
    GetUsageComplete


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUsageComplete result ->
            RequestHelpers.applyResult
                { setResult = \usage m -> { m | usage = usage }
                , defaultError = gettext "Unable to get usage." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    viewWith appState []


{-| The page with rows of a wrapping page's own at the end of the table — the
usage of what only that client has, from the same answer.
-}
viewWith : AppState -> List UsageTable.Row -> Model -> { body : List (Html msg), footer : Maybe (Html msg) }
viewWith appState extraRows model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "Usage" appState.locale) []
            , Section.actionResult appState (viewUsage appState extraRows) model.usage
            ]
        ]
    , footer = Nothing
    }


viewUsage : AppState -> List UsageTable.Row -> Usage -> Html msg
viewUsage appState extraRows usage =
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
                ++ extraRows
        }
