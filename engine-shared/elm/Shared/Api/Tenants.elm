module Shared.Api.Tenants exposing
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
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet, jwtPost, jwtPut)
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.EditableConfig as EditableConfig exposing (EditableConfig)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Tenant as Tenant exposing (Tenant)
import Shared.Data.TenantDetail as TenantDetail exposing (TenantDetail)
import Shared.Data.Usage as Usage exposing (Usage)
import Uuid exposing (Uuid)


getTenants : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Tenant) msg -> Cmd msg
getTenants filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "enabled", PaginationQueryFilters.getValue "enabled" filters )
                ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/tenants" ++ queryString
    in
    jwtGet url (Pagination.decoder "tenants" Tenant.decoder)


getTenant : Uuid -> AbstractAppState a -> ToMsg TenantDetail msg -> Cmd msg
getTenant uuid =
    jwtGet ("/tenants/" ++ Uuid.toString uuid) TenantDetail.decoder


postTenant : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postTenant =
    jwtPost "/tenants"


putTenant : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putTenant uuid =
    jwtPut ("/tenants/" ++ Uuid.toString uuid)


getCurrentConfig : AbstractAppState a -> ToMsg EditableConfig msg -> Cmd msg
getCurrentConfig =
    jwtGet "/tenants/current/config" EditableConfig.decoder


putCurrentConfig : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putCurrentConfig =
    jwtPut "/tenants/current/config"


getTenantUsage : UuidOrCurrent -> AbstractAppState a -> ToMsg Usage msg -> Cmd msg
getTenantUsage tenantUuid =
    jwtGet ("/tenants/" ++ UuidOrCurrent.toString tenantUuid ++ "/usages/wizard") Usage.decoder


putTenantLimits : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putTenantLimits tenantUuid =
    jwtPut ("/tenants/" ++ Uuid.toString tenantUuid ++ "/limits")
