module Shared.Api.Apps exposing
    ( deletePlan
    , getApp
    , getApps
    , getCurrentPlans
    , postApp
    , postPlan
    , putApp
    , putPlan
    )

import Json.Decode as D
import Json.Encode as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPost, jwtPut)
import Shared.Data.App as App exposing (App)
import Shared.Data.AppDetail as AppDetail exposing (AppDetail)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Plan as Plan exposing (Plan)
import Shared.Utils exposing (boolToString)
import Uuid exposing (Uuid)


type alias GetAppsFilters =
    { enabled : Maybe Bool }


getApps : GetAppsFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination App) msg -> Cmd msg
getApps filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams [ ( "enabled", Maybe.map boolToString filters.enabled ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/apps" ++ queryString
    in
    jwtGet url (Pagination.decoder "apps" App.decoder)


getApp : Uuid -> AbstractAppState a -> ToMsg AppDetail msg -> Cmd msg
getApp uuid =
    jwtGet ("/apps/" ++ Uuid.toString uuid) AppDetail.decoder


postApp : E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postApp =
    jwtPost "/apps"


putApp : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putApp uuid =
    jwtPut ("/apps/" ++ Uuid.toString uuid)


postPlan : Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
postPlan uuid =
    jwtPost ("/apps/" ++ Uuid.toString uuid ++ "/plans")


putPlan : Uuid -> Uuid -> E.Value -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putPlan appUuid planUuid =
    jwtPut ("/apps/" ++ Uuid.toString appUuid ++ "/plans/" ++ Uuid.toString planUuid)


deletePlan : Uuid -> Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deletePlan appUuid planUuid =
    jwtDelete ("/apps/" ++ Uuid.toString appUuid ++ "/plans/" ++ Uuid.toString planUuid)


getCurrentPlans : AbstractAppState a -> ToMsg (List Plan) msg -> Cmd msg
getCurrentPlans =
    jwtGet "/apps/current/plans" (D.list Plan.decoder)
