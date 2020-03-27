module Wizard.Settings.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Affiliation.Update
import Wizard.Settings.Auth.Update
import Wizard.Settings.Client.Update
import Wizard.Settings.Features.Update
import Wizard.Settings.Info.Update
import Wizard.Settings.Models exposing (Model)
import Wizard.Settings.Msgs exposing (Msg(..))
import Wizard.Settings.Organization.Update
import Wizard.Settings.Routes exposing (Route(..))


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        AffiliationRoute ->
            Cmd.map AffiliationMsg <|
                Wizard.Settings.Affiliation.Update.fetchData appState

        AuthRoute ->
            Cmd.map AuthMsg <|
                Wizard.Settings.Auth.Update.fetchData appState

        ClientRoute ->
            Cmd.map ClientMsg <|
                Wizard.Settings.Client.Update.fetchData appState

        FeaturesRoute ->
            Cmd.map FeaturesMsg <|
                Wizard.Settings.Features.Update.fetchData appState

        InfoRoute ->
            Cmd.map InfoMsg <|
                Wizard.Settings.Info.Update.fetchData appState

        OrganizationRoute ->
            Cmd.map OrganizationMsg <|
                Wizard.Settings.Organization.Update.fetchData appState


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        AffiliationMsg affiliationMsg ->
            let
                ( affiliationModel, cmd ) =
                    Wizard.Settings.Affiliation.Update.update (wrapMsg << AffiliationMsg) affiliationMsg appState model.affiliationModel
            in
            ( { model | affiliationModel = affiliationModel }, cmd )

        AuthMsg authMsg ->
            let
                ( authModel, cmd ) =
                    Wizard.Settings.Auth.Update.update (wrapMsg << AuthMsg) authMsg appState model.authModel
            in
            ( { model | authModel = authModel }, cmd )

        ClientMsg clientMsg ->
            let
                ( clientModel, cmd ) =
                    Wizard.Settings.Client.Update.update (wrapMsg << ClientMsg) clientMsg appState model.clientModel
            in
            ( { model | clientModel = clientModel }, cmd )

        FeaturesMsg featuresMsg ->
            let
                ( featuresModel, cmd ) =
                    Wizard.Settings.Features.Update.update (wrapMsg << FeaturesMsg) featuresMsg appState model.featuresModel
            in
            ( { model | featuresModel = featuresModel }, cmd )

        InfoMsg infoMsg ->
            let
                ( infoModel, cmd ) =
                    Wizard.Settings.Info.Update.update (wrapMsg << InfoMsg) infoMsg appState model.infoModel
            in
            ( { model | infoModel = infoModel }, cmd )

        OrganizationMsg organizationMsg ->
            let
                ( organizationModel, cmd ) =
                    Wizard.Settings.Organization.Update.update (wrapMsg << OrganizationMsg) organizationMsg appState model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )
