module Wizard.Pages.Settings.Update exposing
    ( fetchData
    , update
    )

import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Authentication.Update
import Wizard.Pages.Settings.DashboardAndLoginScreen.Update
import Wizard.Pages.Settings.Features.Update
import Wizard.Pages.Settings.Generic.Update
import Wizard.Pages.Settings.LookAndFeel.Update
import Wizard.Pages.Settings.Models exposing (Model)
import Wizard.Pages.Settings.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.OpenId.Update
import Wizard.Pages.Settings.OpenIdCreate.Update
import Wizard.Pages.Settings.OpenIdDetail.Update
import Wizard.Pages.Settings.Organization.Update
import Wizard.Pages.Settings.PluginSettings.Update
import Wizard.Pages.Settings.Plugins.Update
import Wizard.Pages.Settings.PrivacyAndSupport.Update
import Wizard.Pages.Settings.Projects.Update
import Wizard.Pages.Settings.Registry.Update
import Wizard.Pages.Settings.RoleCreate.Update
import Wizard.Pages.Settings.RoleDetail.Update
import Wizard.Pages.Settings.Roles.Update
import Wizard.Pages.Settings.Routes exposing (Route(..))
import Wizard.Pages.Settings.Submission.Update
import Wizard.Pages.Settings.Usage.Update


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    let
        genericFetch wrapMsg =
            Cmd.map wrapMsg <|
                Wizard.Pages.Settings.Generic.Update.fetchData appState
    in
    case route of
        OrganizationRoute ->
            genericFetch OrganizationMsg

        AuthenticationRoute ->
            Cmd.map AuthenticationMsg <|
                Wizard.Pages.Settings.Authentication.Update.fetchData appState

        OpenIdRoute ->
            Cmd.map OpenIdMsg <|
                Wizard.Pages.Settings.OpenId.Update.fetchData appState

        OpenIdCreateRoute ->
            Cmd.map OpenIdCreateMsg <|
                Wizard.Pages.Settings.OpenIdCreate.Update.fetchData appState

        OpenIdDetailRoute uuid ->
            Cmd.map OpenIdDetailMsg <|
                Wizard.Pages.Settings.OpenIdDetail.Update.fetchData appState uuid

        RolesRoute ->
            Cmd.map RolesMsg <|
                Wizard.Pages.Settings.Roles.Update.fetchData appState

        RoleCreateRoute ->
            Cmd.map RoleCreateMsg <|
                Wizard.Pages.Settings.RoleCreate.Update.fetchData

        RoleDetailRoute uuid ->
            Cmd.map RoleDetailMsg <|
                Wizard.Pages.Settings.RoleDetail.Update.fetchData appState uuid

        PrivacyAndSupportRoute ->
            genericFetch PrivacyAndSupportMsg

        FeaturesRoute ->
            genericFetch FeaturesMsg

        PluginsRoute ->
            Cmd.none

        PluginSettingsRoute pluginUuid ->
            Cmd.map PluginSettingsMsg <|
                Wizard.Pages.Settings.PluginSettings.Update.fetchData appState pluginUuid

        DashboardAndLoginScreenRoute ->
            genericFetch DashboardMsg

        LookAndFeelRoute ->
            genericFetch LookAndFeelMsg

        RegistryRoute ->
            Cmd.map RegistryMsg <|
                Wizard.Pages.Settings.Registry.Update.fetchData appState

        ProjectsRoute ->
            genericFetch QuestionnairesMsg

        SubmissionRoute ->
            Cmd.map SubmissionMsg <|
                Wizard.Pages.Settings.Submission.Update.fetchData appState

        UsageRoute ->
            Cmd.map UsageMsg <|
                Wizard.Pages.Settings.Usage.Update.fetchData appState


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        OrganizationMsg organizationMsg ->
            let
                ( organizationModel, cmd ) =
                    Wizard.Pages.Settings.Organization.Update.update (wrapMsg << OrganizationMsg) organizationMsg appState model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        AuthenticationMsg authenticationMsg ->
            let
                ( authenticationModel, cmd ) =
                    Wizard.Pages.Settings.Authentication.Update.update (wrapMsg << AuthenticationMsg) authenticationMsg appState model.authenticationModel
            in
            ( { model | authenticationModel = authenticationModel }, cmd )

        OpenIdMsg openIdMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << OpenIdMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( openIdModel, cmd ) =
                    Wizard.Pages.Settings.OpenId.Update.update updateConfig appState openIdMsg model.openIdModel
            in
            ( { model | openIdModel = openIdModel }, cmd )

        OpenIdCreateMsg openIdCreateMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << OpenIdCreateMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( openIdCreateModel, cmd ) =
                    Wizard.Pages.Settings.OpenIdCreate.Update.update updateConfig appState openIdCreateMsg model.openIdCreateModel
            in
            ( { model | openIdCreateModel = openIdCreateModel }, cmd )

        OpenIdDetailMsg openIdDetailMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << OpenIdDetailMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( openIdDetailModel, cmd ) =
                    Wizard.Pages.Settings.OpenIdDetail.Update.update updateConfig appState openIdDetailMsg model.openIdDetailModel
            in
            ( { model | openIdDetailModel = openIdDetailModel }, cmd )

        RolesMsg rolesMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << RolesMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( rolesModel, cmd ) =
                    Wizard.Pages.Settings.Roles.Update.update updateConfig appState rolesMsg model.rolesModel
            in
            ( { model | rolesModel = rolesModel }, cmd )

        RoleCreateMsg roleCreateMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << RoleCreateMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( roleCreateModel, cmd ) =
                    Wizard.Pages.Settings.RoleCreate.Update.update updateConfig appState roleCreateMsg model.roleCreateModel
            in
            ( { model | roleCreateModel = roleCreateModel }, cmd )

        RoleDetailMsg roleDetailMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << RoleDetailMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

                ( roleDetailModel, cmd ) =
                    Wizard.Pages.Settings.RoleDetail.Update.update updateConfig appState roleDetailMsg model.roleDetailModel
            in
            ( { model | roleDetailModel = roleDetailModel }, cmd )

        PrivacyAndSupportMsg privacyAndSupportMsg ->
            let
                ( privacyAndSupportModel, cmd ) =
                    Wizard.Pages.Settings.PrivacyAndSupport.Update.update (wrapMsg << PrivacyAndSupportMsg) privacyAndSupportMsg appState model.privacyAndSupportModel
            in
            ( { model | privacyAndSupportModel = privacyAndSupportModel }, cmd )

        FeaturesMsg featuresMsg ->
            let
                ( featuresModel, cmd ) =
                    Wizard.Pages.Settings.Features.Update.update (wrapMsg << FeaturesMsg) featuresMsg appState model.featuresModel
            in
            ( { model | featuresModel = featuresModel }, cmd )

        PluginsMsg pluginsMsg ->
            let
                ( pluginsModel, cmd ) =
                    Wizard.Pages.Settings.Plugins.Update.update (wrapMsg << PluginsMsg) pluginsMsg appState model.pluginsModel
            in
            ( { model | pluginsModel = pluginsModel }, cmd )

        PluginSettingsMsg pluginSettingsMsg ->
            let
                ( pluginSettingsModel, cmd ) =
                    Wizard.Pages.Settings.PluginSettings.Update.update appState pluginSettingsMsg model.pluginSettingsModel
            in
            ( { model | pluginSettingsModel = pluginSettingsModel }, Cmd.map (wrapMsg << PluginSettingsMsg) cmd )

        DashboardMsg dashboardMsg ->
            let
                ( dashboardModel, cmd ) =
                    Wizard.Pages.Settings.DashboardAndLoginScreen.Update.update (wrapMsg << DashboardMsg) dashboardMsg appState model.dashboardModel
            in
            ( { model | dashboardModel = dashboardModel }, cmd )

        LookAndFeelMsg lookAndFeelMsg ->
            let
                ( lookAndFeelModel, cmd ) =
                    Wizard.Pages.Settings.LookAndFeel.Update.update (wrapMsg << LookAndFeelMsg) lookAndFeelMsg appState model.lookAndFeelModel
            in
            ( { model | lookAndFeelModel = lookAndFeelModel }, cmd )

        RegistryMsg registryMsg ->
            let
                ( registryModel, cmd ) =
                    Wizard.Pages.Settings.Registry.Update.update (wrapMsg << RegistryMsg) registryMsg appState model.registryModel
            in
            ( { model | registryModel = registryModel }, cmd )

        QuestionnairesMsg questionnairesMsg ->
            let
                ( questionnairesModel, cmd ) =
                    Wizard.Pages.Settings.Projects.Update.update (wrapMsg << QuestionnairesMsg) questionnairesMsg appState model.questionnairesModel
            in
            ( { model | questionnairesModel = questionnairesModel }, cmd )

        SubmissionMsg documentSubmissionMsg ->
            let
                ( documentSubmissionModel, cmd ) =
                    Wizard.Pages.Settings.Submission.Update.update (wrapMsg << SubmissionMsg) documentSubmissionMsg appState model.documentSubmissionModel
            in
            ( { model | documentSubmissionModel = documentSubmissionModel }, cmd )

        UsageMsg usageMsg ->
            let
                ( usageModel, cmd ) =
                    Wizard.Pages.Settings.Usage.Update.update usageMsg appState model.usageModel
            in
            ( { model | usageModel = usageModel }, cmd )
