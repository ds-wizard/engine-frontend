module Wizard.Settings.Common.ConfigForm exposing
    ( ConfigForm
    , dashboardDmp
    , dashboardWelcome
    , init
    , initEmpty
    , toEditableConfig
    , validation
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Config.CustomMenuLink as CustomMenuLink exposing (CustomMenuLink)
import Wizard.Common.Config.DashboardWidget exposing (DashboardWidget(..))
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableConfig exposing (EditableConfig)
import Wizard.Users.Common.Role as Role


type alias ConfigForm =
    { levelsEnabled : Bool
    , publicQuestionnaireEnabled : Bool
    , questionnaireAccessibilityEnabled : Bool
    , registrationEnabled : Bool
    , appTitle : Maybe String
    , appTitleShort : Maybe String
    , welcomeInfo : Maybe String
    , welcomeWarning : Maybe String
    , loginInfo : Maybe String
    , dashboard : String
    , privacyUrl : Maybe String
    , customMenuLinks : List CustomMenuLink
    , supportEmail : Maybe String
    , supportRepositoryName : Maybe String
    , supportRepositoryUrl : Maybe String
    }


initEmpty : Form CustomFormError ConfigForm
initEmpty =
    Form.initial [] validation


init : EditableConfig -> Form CustomFormError ConfigForm
init config =
    Form.initial (configToFormInitials config) validation


validateMaybeString : Validation CustomFormError (Maybe String)
validateMaybeString =
    V.oneOf [ V.emptyString |> V.map (\_ -> Nothing), V.string |> V.map Just ]


fieldMaybeString : Maybe String -> Field.Field
fieldMaybeString =
    Field.string << Maybe.withDefault ""


validation : Validation CustomFormError ConfigForm
validation =
    V.succeed ConfigForm
        |> V.andMap (V.field "levelsEnabled" V.bool)
        |> V.andMap (V.field "publicQuestionnaireEnabled" V.bool)
        |> V.andMap (V.field "questionnaireAccessibilityEnabled" V.bool)
        |> V.andMap (V.field "registrationEnabled" V.bool)
        |> V.andMap (V.field "appTitle" validateMaybeString)
        |> V.andMap (V.field "appTitleShort" validateMaybeString)
        |> V.andMap (V.field "welcomeInfo" validateMaybeString)
        |> V.andMap (V.field "welcomeWarning" validateMaybeString)
        |> V.andMap (V.field "loginInfo" validateMaybeString)
        |> V.andMap (V.field "dashboard" V.string)
        |> V.andMap (V.field "privacyUrl" validateMaybeString)
        |> V.andMap (V.field "customMenuLinks" (V.list CustomMenuLink.validation))
        |> V.andMap (V.field "supportEmail" validateMaybeString)
        |> V.andMap (V.field "supportRepositoryName" validateMaybeString)
        |> V.andMap (V.field "supportRepositoryUrl" validateMaybeString)


configToFormInitials : EditableConfig -> List ( String, Field.Field )
configToFormInitials config =
    let
        dashboard =
            case config.client.dashboard of
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
                config.client.customMenuLinks
    in
    [ ( "levelsEnabled", Field.bool config.features.levels.enabled )
    , ( "publicQuestionnaireEnabled", Field.bool config.features.publicQuestionnaire.enabled )
    , ( "questionnaireAccessibilityEnabled", Field.bool config.features.questionnaireAccessibility.enabled )
    , ( "registrationEnabled", Field.bool config.features.registration.enabled )
    , ( "appTitle", fieldMaybeString config.client.appTitle )
    , ( "appTitleShort", fieldMaybeString config.client.appTitleShort )
    , ( "welcomeInfo", fieldMaybeString config.client.welcomeInfo )
    , ( "welcomeWarning", fieldMaybeString config.client.welcomeWarning )
    , ( "loginInfo", fieldMaybeString config.client.loginInfo )
    , ( "dashboard", Field.string dashboard )
    , ( "privacyUrl", fieldMaybeString config.client.privacyUrl )
    , ( "customMenuLinks", Field.list customMenuLinks )
    , ( "supportEmail", fieldMaybeString config.client.supportEmail )
    , ( "supportRepositoryName", fieldMaybeString config.client.supportRepositoryName )
    , ( "supportRepositoryUrl", fieldMaybeString config.client.supportRepositoryUrl )
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


toEditableConfig : ConfigForm -> EditableConfig
toEditableConfig form =
    let
        dashboard =
            if form.dashboard == dashboardDmp then
                Just dashboardDmpSettings

            else
                Nothing
    in
    { features =
        { levels = { enabled = form.levelsEnabled }
        , publicQuestionnaire = { enabled = form.publicQuestionnaireEnabled }
        , questionnaireAccessibility = { enabled = form.questionnaireAccessibilityEnabled }
        , registration = { enabled = form.registrationEnabled }
        }
    , client =
        { appTitle = form.appTitle
        , appTitleShort = form.appTitleShort
        , welcomeInfo = form.welcomeInfo
        , welcomeWarning = form.welcomeWarning
        , loginInfo = form.loginInfo
        , dashboard = dashboard
        , privacyUrl = form.privacyUrl
        , customMenuLinks = form.customMenuLinks
        , supportEmail = form.supportEmail
        , supportRepositoryName = form.supportRepositoryName
        , supportRepositoryUrl = form.supportRepositoryUrl
        }
    }
