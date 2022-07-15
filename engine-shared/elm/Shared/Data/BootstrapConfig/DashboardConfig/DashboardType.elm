module Shared.Data.BootstrapConfig.DashboardConfig.DashboardType exposing
    ( DashboardType(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type DashboardType
    = Welcome
    | RoleBased


decoder : Decoder DashboardType
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "WelcomeDashboardType" ->
                        D.succeed Welcome

                    "RoleBasedDashboardType" ->
                        D.succeed RoleBased

                    dashboardType ->
                        D.fail <| "Unknown dashboard type: " ++ dashboardType
            )


encode : DashboardType -> E.Value
encode dashboardType =
    case dashboardType of
        Welcome ->
            E.string "WelcomeDashboardType"

        RoleBased ->
            E.string "RoleBasedDashboardType"
