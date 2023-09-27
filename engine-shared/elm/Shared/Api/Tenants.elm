module Shared.Api.Tenants exposing
    ( deleteCurrentLogo
    , deletePlan
    , getCurrentConfig
    , getCurrentPlans
    , getTenant
    , getTenants
    , postPlan
    , postTenant
    , putCurrentConfig
    , putPlan
    , putTenant
    , uploadCurrentLogo
    )

import File exposing (File)
import Json.Decode as D
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPost, jwtPostFile, jwtPut)
import Shared.Data.EditableConfig as EditableConfig exposing (EditableConfig)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Plan as Plan exposing (Plan)
import Shared.Data.Tenant as Tenant exposing (Tenant)
import Shared.Data.TenantDetail as TenantDetail exposing (TenantDetail)
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


postPlan : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postPlan uuid =
    jwtPost ("/tenants/" ++ Uuid.toString uuid ++ "/plans")


putPlan : Uuid -> Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putPlan tenantUuid planUuid =
    jwtPut ("/tenants/" ++ Uuid.toString tenantUuid ++ "/plans/" ++ Uuid.toString planUuid)


deletePlan : Uuid -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deletePlan tenantUuid planUuid =
    jwtDelete ("/tenants/" ++ Uuid.toString tenantUuid ++ "/plans/" ++ Uuid.toString planUuid)


getCurrentPlans : AbstractAppState a -> ToMsg (List Plan) msg -> Cmd msg
getCurrentPlans =
    jwtGet "/tenants/current/plans" (D.list Plan.decoder)


getCurrentConfig : AbstractAppState a -> ToMsg EditableConfig msg -> Cmd msg
getCurrentConfig =
    jwtGet "/tenants/current/config" EditableConfig.decoder


putCurrentConfig : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putCurrentConfig =
    jwtPut "/tenants/current/config"


uploadCurrentLogo : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
uploadCurrentLogo =
    jwtPostFile "/tenants/current/logo"


deleteCurrentLogo : AbstractAppState a -> ToMsg () msg -> Cmd msg
deleteCurrentLogo =
    jwtDelete "/tenants/current/logo"
