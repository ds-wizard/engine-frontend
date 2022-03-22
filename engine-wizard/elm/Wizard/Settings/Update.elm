module Wizard.Settings.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Authentication.Update
import Wizard.Settings.Dashboard.Update
import Wizard.Settings.Generic.Update
import Wizard.Settings.KnowledgeModels.Update
import Wizard.Settings.LookAndFeel.Update
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))
import Wizard.Settings.Organization.Update
import Wizard.Settings.Plans.Update
import Wizard.Settings.PrivacyAndSupport.Update
import Wizard.Settings.Projects.Update
import Wizard.Settings.Registry.Update
import Wizard.Settings.Routes exposing (Route(..))
import Wizard.Settings.Submission.Update
import Wizard.Settings.Template.Update
import Wizard.Settings.Usage.Update


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState _ =
    let
        genericFetch wrapMsg =
            Cmd.map wrapMsg <|
                Wizard.Settings.Generic.Update.fetchData appState
    in
    case route of
        OrganizationRoute ->
            genericFetch OrganizationMsg

        AuthenticationRoute ->
            Cmd.map AuthenticationMsg <|
                Wizard.Settings.Authentication.Update.fetchData appState

        PrivacyAndSupportRoute ->
            genericFetch PrivacyAndSupportMsg

        DashboardRoute ->
            genericFetch DashboardMsg

        LookAndFeelRoute ->
            Cmd.map LookAndFeelMsg <|
                Wizard.Settings.LookAndFeel.Update.fetchData appState

        RegistryRoute ->
            Cmd.map RegistryMsg <|
                Wizard.Settings.Registry.Update.fetchData appState

        ProjectsRoute ->
            genericFetch QuestionnairesMsg

        SubmissionRoute ->
            Cmd.map SubmissionMsg <|
                Wizard.Settings.Submission.Update.fetchData appState

        TemplateRoute ->
            Cmd.map TemplateMsg <|
                Wizard.Settings.Template.Update.fetchData appState

        KnowledgeModelsRoute ->
            genericFetch KnowledgeModelsMsg

        UsageRoute ->
            Cmd.map UsageMsg <|
                Wizard.Settings.Usage.Update.fetchData appState

        PlansRoute ->
            Cmd.map PlansMsg <|
                Wizard.Settings.Plans.Update.fetchData appState


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        OrganizationMsg organizationMsg ->
            let
                ( organizationModel, cmd ) =
                    Wizard.Settings.Organization.Update.update (wrapMsg << OrganizationMsg) organizationMsg appState model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        AuthenticationMsg authenticationMsg ->
            let
                ( authenticationModel, cmd ) =
                    Wizard.Settings.Authentication.Update.update (wrapMsg << AuthenticationMsg) authenticationMsg appState model.authenticationModel
            in
            ( { model | authenticationModel = authenticationModel }, cmd )

        PrivacyAndSupportMsg privacyAndSupportMsg ->
            let
                ( privacyAndSupportModel, cmd ) =
                    Wizard.Settings.PrivacyAndSupport.Update.update (wrapMsg << PrivacyAndSupportMsg) privacyAndSupportMsg appState model.privacyAndSupportModel
            in
            ( { model | privacyAndSupportModel = privacyAndSupportModel }, cmd )

        DashboardMsg dashboardMsg ->
            let
                ( dashboardModel, cmd ) =
                    Wizard.Settings.Dashboard.Update.update (wrapMsg << DashboardMsg) dashboardMsg appState model.dashboardModel
            in
            ( { model | dashboardModel = dashboardModel }, cmd )

        LookAndFeelMsg lookAndFeelMsg ->
            let
                ( lookAndFeelModel, cmd ) =
                    Wizard.Settings.LookAndFeel.Update.update (wrapMsg << LookAndFeelMsg) lookAndFeelMsg appState model.lookAndFeelModel
            in
            ( { model | lookAndFeelModel = lookAndFeelModel }, cmd )

        RegistryMsg registryMsg ->
            let
                ( registryModel, cmd ) =
                    Wizard.Settings.Registry.Update.update (wrapMsg << RegistryMsg) registryMsg appState model.registryModel
            in
            ( { model | registryModel = registryModel }, cmd )

        QuestionnairesMsg questionnairesMsg ->
            let
                ( questionnairesModel, cmd ) =
                    Wizard.Settings.Projects.Update.update (wrapMsg << QuestionnairesMsg) questionnairesMsg appState model.questionnairesModel
            in
            ( { model | questionnairesModel = questionnairesModel }, cmd )

        SubmissionMsg documentSubmissionMsg ->
            let
                ( documentSubmissionModel, cmd ) =
                    Wizard.Settings.Submission.Update.update (wrapMsg << SubmissionMsg) documentSubmissionMsg appState model.documentSubmissionModel
            in
            ( { model | documentSubmissionModel = documentSubmissionModel }, cmd )

        TemplateMsg templateMsg ->
            let
                ( templateModel, cmd ) =
                    Wizard.Settings.Template.Update.update (wrapMsg << TemplateMsg) templateMsg appState model.templateModel
            in
            ( { model | templateModel = templateModel }, cmd )

        KnowledgeModelsMsg knowledgeModelsMsg ->
            let
                ( knowledgeModelsModel, cmd ) =
                    Wizard.Settings.KnowledgeModels.Update.update (wrapMsg << KnowledgeModelsMsg) knowledgeModelsMsg appState model.knowledgeModelsModel
            in
            ( { model | knowledgeModelsModel = knowledgeModelsModel }, cmd )

        UsageMsg usageMsg ->
            let
                ( usageModel, cmd ) =
                    Wizard.Settings.Usage.Update.update usageMsg appState model.usageModel
            in
            ( { model | usageModel = usageModel }, cmd )

        PlansMsg plansMsg ->
            let
                ( plansModel, cmd ) =
                    Wizard.Settings.Plans.Update.update plansMsg appState model.plansModel
            in
            ( { model | plansModel = plansModel }, cmd )
