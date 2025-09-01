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
import Wizard.Pages.Settings.KnowledgeModels.Update
import Wizard.Pages.Settings.LookAndFeel.Update
import Wizard.Pages.Settings.Models exposing (Model)
import Wizard.Pages.Settings.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Organization.Update
import Wizard.Pages.Settings.PrivacyAndSupport.Update
import Wizard.Pages.Settings.Projects.Update
import Wizard.Pages.Settings.Registry.Update
import Wizard.Pages.Settings.Routes exposing (Route(..))
import Wizard.Pages.Settings.Submission.Update
import Wizard.Pages.Settings.Usage.Update


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState _ =
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

        PrivacyAndSupportRoute ->
            genericFetch PrivacyAndSupportMsg

        FeaturesRoute ->
            genericFetch FeaturesMsg

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

        KnowledgeModelsRoute ->
            genericFetch KnowledgeModelsMsg

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

        KnowledgeModelsMsg knowledgeModelsMsg ->
            let
                ( knowledgeModelsModel, cmd ) =
                    Wizard.Pages.Settings.KnowledgeModels.Update.update (wrapMsg << KnowledgeModelsMsg) knowledgeModelsMsg appState model.knowledgeModelsModel
            in
            ( { model | knowledgeModelsModel = knowledgeModelsModel }, cmd )

        UsageMsg usageMsg ->
            let
                ( usageModel, cmd ) =
                    Wizard.Pages.Settings.Usage.Update.update usageMsg appState model.usageModel
            in
            ( { model | usageModel = usageModel }, cmd )
