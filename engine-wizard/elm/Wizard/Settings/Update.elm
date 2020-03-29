module Wizard.Settings.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Authentication.Update
import Wizard.Settings.Dashboard.Update
import Wizard.Settings.Generic.Update
import Wizard.Settings.KnowledgeModelRegistry.Update
import Wizard.Settings.LookAndFeel.Update
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))
import Wizard.Settings.Organization.Update
import Wizard.Settings.PrivacyAndSupport.Update
import Wizard.Settings.Questionnaires.Update
import Wizard.Settings.Routes exposing (Route(..))


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    let
        wrapMsg =
            case route of
                OrganizationRoute ->
                    OrganizationMsg

                AuthenticationRoute ->
                    AuthenticationMsg

                PrivacyAndSupportRoute ->
                    PrivacyAndSupportMsg

                DashboardRoute ->
                    DashboardMsg

                LookAndFeelRoute ->
                    LookAndFeelMsg

                KnowledgeModelRegistryRoute ->
                    KnowledgeModelRegistryMsg

                QuestionnairesRoute ->
                    QuestionnairesMsg
    in
    Cmd.map wrapMsg <| Wizard.Settings.Generic.Update.fetchData appState


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

        KnowledgeModelRegistryMsg knowledgeModelRegistryMsg ->
            let
                ( knowledgeModelRegistryModel, cmd ) =
                    Wizard.Settings.KnowledgeModelRegistry.Update.update (wrapMsg << KnowledgeModelRegistryMsg) knowledgeModelRegistryMsg appState model.knowledgeModelRegistryModel
            in
            ( { model | knowledgeModelRegistryModel = knowledgeModelRegistryModel }, cmd )

        QuestionnairesMsg questionnairesMsg ->
            let
                ( questionnairesModel, cmd ) =
                    Wizard.Settings.Questionnaires.Update.update (wrapMsg << QuestionnairesMsg) questionnairesMsg appState model.questionnairesModel
            in
            ( { model | questionnairesModel = questionnairesModel }, cmd )
