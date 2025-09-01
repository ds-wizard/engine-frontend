module Wizard.Api.Tenants exposing
    ( getCurrentConfig
    , getTenant
    , getTenantUsage
    , getTenants
    , postTenant
    , putCurrentConfig
    , putTenant
    , putTenantLimits
    )

import Json.Encode as E
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Uuid exposing (Uuid)
import Wizard.Api.Models.EditableConfig as EditableConfig exposing (EditableConfig)
import Wizard.Api.Models.Tenant as Tenant exposing (Tenant)
import Wizard.Api.Models.TenantDetail as TenantDetail exposing (TenantDetail)
import Wizard.Api.Models.Usage as Usage exposing (Usage)
import Wizard.Common.AppState as AppState exposing (AppState)


getTenants : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Tenant) msg -> Cmd msg
getTenants appState filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "enabled", PaginationQueryFilters.getValue "enabled" filters )
                , ( "states", PaginationQueryFilters.getValue "states" filters )
                ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/tenants" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "tenants" Tenant.decoder)


getTenant : AppState -> Uuid -> ToMsg TenantDetail msg -> Cmd msg
getTenant appState uuid =
    Request.get (AppState.toServerInfo appState) ("/tenants/" ++ Uuid.toString uuid) TenantDetail.decoder


postTenant : AppState -> E.Value -> ToMsg () msg -> Cmd msg
postTenant appState body =
    Request.postWhatever (AppState.toServerInfo appState) "/tenants" body


putTenant : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putTenant appState uuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/tenants/" ++ Uuid.toString uuid) body


getCurrentConfig : AppState -> ToMsg EditableConfig msg -> Cmd msg
getCurrentConfig appState =
    Request.get (AppState.toServerInfo appState) "/tenants/current/config" EditableConfig.decoder


putCurrentConfig : AppState -> E.Value -> ToMsg () msg -> Cmd msg
putCurrentConfig appState body =
    Request.putWhatever (AppState.toServerInfo appState) "/tenants/current/config" body


getTenantUsage : AppState -> UuidOrCurrent -> ToMsg Usage msg -> Cmd msg
getTenantUsage appState tenantUuid =
    Request.get (AppState.toServerInfo appState) ("/tenants/" ++ UuidOrCurrent.toString tenantUuid ++ "/usages/wizard") Usage.decoder


putTenantLimits : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putTenantLimits appState tenantUuid body =
    Request.putWhatever (AppState.toServerInfo appState) ("/tenants/" ++ Uuid.toString tenantUuid ++ "/limits") body
