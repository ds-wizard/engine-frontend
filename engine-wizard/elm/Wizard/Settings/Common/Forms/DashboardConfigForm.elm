module Wizard.Settings.Common.Forms.DashboardConfigForm exposing
    ( DashboardConfigForm
    , dashboardDmp
    , dashboardWelcome
    , init
    , initEmpty
    , toDashboardConfig
    , validation
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Shared.Auth.Role as Role
import Shared.Data.BootstrapConfig.DashboardConfig exposing (DashboardConfig)
import Shared.Data.BootstrapConfig.DashboardConfig.DashboardWidget exposing (DashboardWidget(..))
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias DashboardConfigForm =
    { widgets : String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    }


initEmpty : Form FormError DashboardConfigForm
initEmpty =
    Form.initial [] validation


init : DashboardConfig -> Form FormError DashboardConfigForm
init config =
    let
        widgets =
            case config.widgets of
                Just _ ->
                    dashboardDmp

                Nothing ->
                    dashboardWelcome

        fields =
            [ ( "widgets", Field.string widgets )
            , ( "welcomeInfo", Field.maybeString config.welcomeInfo )
            , ( "welcomeWarning", Field.maybeString config.welcomeWarning )
            ]
    in
    Form.initial fields validation


validation : Validation FormError DashboardConfigForm
validation =
    V.succeed DashboardConfigForm
        |> V.andMap (V.field "widgets" V.string)
        |> V.andMap (V.field "welcomeInfo" V.maybeString)
        |> V.andMap (V.field "welcomeWarning" V.maybeString)


toDashboardConfig : DashboardConfigForm -> DashboardConfig
toDashboardConfig form =
    let
        widgets =
            if form.widgets == dashboardDmp then
                Just dashboardDmpSettings

            else
                Nothing
    in
    { widgets = widgets
    , welcomeInfo = form.welcomeInfo
    , welcomeWarning = form.welcomeWarning
    }


dashboardWelcome : String
dashboardWelcome =
    "welcome"


dashboardDmp : String
dashboardDmp =
    "dmp"


dashboardDmpSettings : Dict String (List DashboardWidget)
dashboardDmpSettings =
    Dict.fromList
        [ ( Role.admin, [ DMPWorkflowDashboardWidget, LevelsQuestionnaireDashboardWidget ] )
        , ( Role.dataSteward, [ DMPWorkflowDashboardWidget, LevelsQuestionnaireDashboardWidget ] )
        , ( Role.researcher, [ DMPWorkflowDashboardWidget, LevelsQuestionnaireDashboardWidget ] )
        ]
