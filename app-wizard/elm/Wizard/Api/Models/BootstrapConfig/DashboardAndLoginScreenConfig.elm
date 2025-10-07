module Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig exposing
    ( DashboardAndLoginScreenConfig
    , decoder
    , default
    , encode
    )

import Common.Api.Models.Announcement as Announcement exposing (Announcement)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.DashboardType as DashboardType exposing (DashboardType)


type alias DashboardAndLoginScreenConfig =
    { dashboardType : DashboardType
    , loginInfo : Maybe String
    , loginInfoSidebar : Maybe String
    , announcements : List Announcement
    }


default : DashboardAndLoginScreenConfig
default =
    { dashboardType = DashboardType.Welcome
    , loginInfo = Nothing
    , loginInfoSidebar = Nothing
    , announcements = []
    }



-- JSON


decoder : Decoder DashboardAndLoginScreenConfig
decoder =
    D.succeed DashboardAndLoginScreenConfig
        |> D.required "dashboardType" DashboardType.decoder
        |> D.required "loginInfo" (D.maybe D.string)
        |> D.required "loginInfoSidebar" (D.maybe D.string)
        |> D.required "announcements" (D.list Announcement.decoder)


encode : DashboardAndLoginScreenConfig -> E.Value
encode config =
    E.object
        [ ( "dashboardType", DashboardType.encode config.dashboardType )
        , ( "loginInfo", E.maybe E.string config.loginInfo )
        , ( "loginInfoSidebar", E.maybe E.string config.loginInfoSidebar )
        , ( "announcements", E.list Announcement.encode config.announcements )
        ]
