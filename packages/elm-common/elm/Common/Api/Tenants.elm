module Common.Api.Tenants exposing
    ( getTenant
    , getTenantSuggestions
    , getTenantUsage
    , getTenants
    , postTenant
    , putTenant
    , putTenantLimits
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.Tenant as Tenant exposing (Tenant)
import Common.Api.Models.TenantDetail as TenantDetail exposing (TenantDetail)
import Common.Api.Models.TenantSuggestion as TenantSuggestion exposing (TenantSuggestion)
import Common.Api.Models.Usage as Usage exposing (Usage)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


getTenants : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Tenant) msg -> Cmd msg
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
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "tenants" Tenant.decoder)


getTenantSuggestions : Context ctx -> PaginationQueryString -> ToMsg (Pagination TenantSuggestion) msg -> Cmd msg
getTenantSuggestions appState pqs =
    let
        queryString =
            PaginationQueryString.toApiUrl pqs

        url =
            "/tenants/suggestions" ++ queryString

        wrapPagination items =
            let
                pagination =
                    Pagination.empty
            in
            { pagination | items = items }
    in
    Request.get (Context.toServerInfo appState) url (D.map wrapPagination (D.list TenantSuggestion.decoder))


getTenant : Context ctx -> Uuid -> ToMsg TenantDetail msg -> Cmd msg
getTenant appState uuid =
    Request.get (Context.toServerInfo appState) ("/tenants/" ++ Uuid.toString uuid) TenantDetail.decoder


postTenant : Context ctx -> E.Value -> ToMsg () msg -> Cmd msg
postTenant appState body =
    Request.postWhatever (Context.toServerInfo appState) "/tenants" body


putTenant : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putTenant appState uuid body =
    Request.putWhatever (Context.toServerInfo appState) ("/tenants/" ++ Uuid.toString uuid) body


getTenantUsage : Context ctx -> UuidOrCurrent -> ToMsg Usage msg -> Cmd msg
getTenantUsage appState tenantUuid =
    Request.get (Context.toServerInfo appState) ("/tenants/" ++ UuidOrCurrent.toString tenantUuid ++ "/usages") Usage.decoder


putTenantLimits : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putTenantLimits appState tenantUuid body =
    Request.putWhatever (Context.toServerInfo appState) ("/tenants/" ++ Uuid.toString tenantUuid ++ "/limits") body
