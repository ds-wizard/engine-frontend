module Wizard.Settings.Common.Forms.DashboardAndLoginScreenConfigForm exposing
    ( DashboardAndLoginScreenConfigForm
    , dashboardRoleBased
    , dashboardWelcome
    , init
    , initEmpty
    , toDashboardConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig exposing (DashboardAndLoginScreenConfig)
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement as Announcement exposing (Announcement)
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.DashboardType as DashboardType


type alias DashboardAndLoginScreenConfigForm =
    { dashboardType : String
    , loginInfo : Maybe String
    , loginInfoSidebar : Maybe String
    , announcements : List Announcement
    }


initEmpty : Form FormError DashboardAndLoginScreenConfigForm
initEmpty =
    Form.initial [] validation


init : DashboardAndLoginScreenConfig -> Form FormError DashboardAndLoginScreenConfigForm
init config =
    let
        dashboardType =
            case config.dashboardType of
                DashboardType.Welcome ->
                    dashboardWelcome

                DashboardType.RoleBased ->
                    dashboardRoleBased

        announcements =
            List.map (Field.group << Announcement.toFormInitials) config.announcements

        fields =
            [ ( "dashboardType", Field.string dashboardType )
            , ( "loginInfo", Field.maybeString config.loginInfo )
            , ( "loginInfoSidebar", Field.maybeString config.loginInfoSidebar )
            , ( "announcements", Field.list announcements )
            ]
    in
    Form.initial fields validation


validation : Validation FormError DashboardAndLoginScreenConfigForm
validation =
    V.succeed DashboardAndLoginScreenConfigForm
        |> V.andMap (V.field "dashboardType" V.string)
        |> V.andMap (V.field "loginInfo" V.maybeString)
        |> V.andMap (V.field "loginInfoSidebar" V.maybeString)
        |> V.andMap (V.field "announcements" (V.list Announcement.validation))


toDashboardConfig : DashboardAndLoginScreenConfigForm -> DashboardAndLoginScreenConfig
toDashboardConfig form =
    let
        dashboardType =
            if form.dashboardType == dashboardRoleBased then
                DashboardType.RoleBased

            else
                DashboardType.Welcome
    in
    { dashboardType = dashboardType
    , loginInfo = form.loginInfo
    , loginInfoSidebar = form.loginInfoSidebar
    , announcements = form.announcements
    }


dashboardWelcome : String
dashboardWelcome =
    "welcome"


dashboardRoleBased : String
dashboardRoleBased =
    "roleBased"
