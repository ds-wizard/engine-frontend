module Wizard.Settings.Common.Forms.DashboardConfigForm exposing
    ( DashboardConfigForm
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
import Shared.Data.BootstrapConfig.DashboardConfig exposing (DashboardConfig)
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardType as DashboardType
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias DashboardConfigForm =
    { dashboardType : String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    }


initEmpty : Form FormError DashboardConfigForm
initEmpty =
    Form.initial [] validation


init : DashboardConfig -> Form FormError DashboardConfigForm
init config =
    let
        dashboardType =
            case config.dashboardType of
                DashboardType.Welcome ->
                    dashboardWelcome

                DashboardType.RoleBased ->
                    dashboardRoleBased

        fields =
            [ ( "dashboardType", Field.string dashboardType )
            , ( "welcomeInfo", Field.maybeString config.welcomeInfo )
            , ( "welcomeWarning", Field.maybeString config.welcomeWarning )
            ]
    in
    Form.initial fields validation


validation : Validation FormError DashboardConfigForm
validation =
    V.succeed DashboardConfigForm
        |> V.andMap (V.field "dashboardType" V.string)
        |> V.andMap (V.field "welcomeInfo" V.maybeString)
        |> V.andMap (V.field "welcomeWarning" V.maybeString)


toDashboardConfig : DashboardConfigForm -> DashboardConfig
toDashboardConfig form =
    let
        dashboardType =
            if form.dashboardType == dashboardRoleBased then
                DashboardType.RoleBased

            else
                DashboardType.Welcome
    in
    { dashboardType = dashboardType
    , welcomeInfo = form.welcomeInfo
    , welcomeWarning = form.welcomeWarning
    }


dashboardWelcome : String
dashboardWelcome =
    "welcome"


dashboardRoleBased : String
dashboardRoleBased =
    "roleBased"
