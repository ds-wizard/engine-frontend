module Shared.Data.BootstrapConfig.DashboardConfig exposing
    ( DashboardConfig
    , decoder
    , default
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardType as DashboardType exposing (DashboardType)


type alias DashboardConfig =
    { dashboardType : DashboardType
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    }


default : DashboardConfig
default =
    { dashboardType = DashboardType.Welcome
    , welcomeInfo = Nothing
    , welcomeWarning = Nothing
    }



-- JSON


decoder : Decoder DashboardConfig
decoder =
    D.succeed DashboardConfig
        |> D.required "dashboardType" DashboardType.decoder
        |> D.required "welcomeInfo" (D.maybe D.string)
        |> D.required "welcomeWarning" (D.maybe D.string)


encode : DashboardConfig -> E.Value
encode config =
    E.object
        [ ( "dashboardType", DashboardType.encode config.dashboardType )
        , ( "welcomeInfo", E.maybe E.string config.welcomeInfo )
        , ( "welcomeWarning", E.maybe E.string config.welcomeWarning )
        ]
