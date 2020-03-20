module Wizard.Settings.Common.ClientConfigForm exposing
    ( ClientConfigForm
    , dashboardDmp
    , dashboardWelcome
    , init
    , initEmpty
    , toEditableClientConfig
    , validation
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Config.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)
import Wizard.Common.Config.DashboardWidget exposing (DashboardWidget(..))
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Field as Field
import Wizard.Common.Form.Validate as V
import Wizard.Settings.Common.EditableClientConfig exposing (EditableClientConfig)
import Wizard.Users.Common.Role as Role


type alias ClientConfigForm =
    { appTitle : Maybe String
    , appTitleShort : Maybe String
    , dashboard : String
    , privacyUrl : Maybe String
    , customMenuLinks : List CustomMenuLink
    , supportEmail : Maybe String
    , supportRepositoryName : Maybe String
    , supportRepositoryUrl : Maybe String
    }


initEmpty : Form CustomFormError ClientConfigForm
initEmpty =
    Form.initial [] validation


init : EditableClientConfig -> Form CustomFormError ClientConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validation : Validation CustomFormError ClientConfigForm
validation =
    V.succeed ClientConfigForm
        |> V.andMap (V.field "appTitle" V.maybeString)
        |> V.andMap (V.field "appTitleShort" V.maybeString)
        |> V.andMap (V.field "dashboard" V.string)
        |> V.andMap (V.field "privacyUrl" V.maybeString)
        |> V.andMap (V.field "customMenuLinks" (V.list CustomMenuLink.validation))
        |> V.andMap (V.field "supportEmail" V.maybeString)
        |> V.andMap (V.field "supportRepositoryName" V.maybeString)
        |> V.andMap (V.field "supportRepositoryUrl" V.maybeString)


configToFormInitials : EditableClientConfig -> List ( String, Field.Field )
configToFormInitials config =
    let
        dashboard =
            case config.dashboard of
                Just _ ->
                    dashboardDmp

                Nothing ->
                    dashboardWelcome

        customMenuLinks =
            List.map
                (\l ->
                    Field.group
                        [ ( "icon", Field.string l.icon )
                        , ( "title", Field.string l.title )
                        , ( "url", Field.string l.url )
                        , ( "newWindow", Field.bool l.newWindow )
                        ]
                )
                config.customMenuLinks
    in
    [ ( "appTitle", Field.maybeString config.appTitle )
    , ( "appTitleShort", Field.maybeString config.appTitleShort )
    , ( "dashboard", Field.string dashboard )
    , ( "privacyUrl", Field.maybeString config.privacyUrl )
    , ( "customMenuLinks", Field.list customMenuLinks )
    , ( "supportEmail", Field.maybeString config.supportEmail )
    , ( "supportRepositoryName", Field.maybeString config.supportRepositoryName )
    , ( "supportRepositoryUrl", Field.maybeString config.supportRepositoryUrl )
    ]


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


toEditableClientConfig : ClientConfigForm -> EditableClientConfig
toEditableClientConfig form =
    let
        dashboard =
            if form.dashboard == dashboardDmp then
                Just dashboardDmpSettings

            else
                Nothing
    in
    { appTitle = form.appTitle
    , appTitleShort = form.appTitleShort
    , dashboard = dashboard
    , privacyUrl = form.privacyUrl
    , customMenuLinks = form.customMenuLinks
    , supportEmail = form.supportEmail
    , supportRepositoryName = form.supportRepositoryName
    , supportRepositoryUrl = form.supportRepositoryUrl
    }
