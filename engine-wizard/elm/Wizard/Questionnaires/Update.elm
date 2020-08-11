module Wizard.Questionnaires.Update exposing
    ( fetchData
    , onUnload
    , update
    )

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Questionnaires.Create.Update
import Wizard.Questionnaires.CreateMigration.Update
import Wizard.Questionnaires.Detail.Update
import Wizard.Questionnaires.Edit.Update
import Wizard.Questionnaires.Index.Update
import Wizard.Questionnaires.Migration.Update
import Wizard.Questionnaires.Models exposing (Model)
import Wizard.Questionnaires.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ ->
            Cmd.map CreateMsg <|
                Wizard.Questionnaires.Create.Update.fetchData appState model.createModel

        CreateMigrationRoute uuid ->
            Cmd.map CreateMigrationMsg <|
                Wizard.Questionnaires.CreateMigration.Update.fetchData appState uuid

        DetailRoute uuid ->
            Cmd.map DetailMsg <|
                Wizard.Questionnaires.Detail.Update.fetchData appState uuid

        EditRoute uuid ->
            Cmd.map EditMsg <|
                Wizard.Questionnaires.Edit.Update.fetchData appState uuid

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Questionnaires.Index.Update.fetchData

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Wizard.Questionnaires.Migration.Update.fetchData appState uuid


onUnload : Route -> Model -> Cmd Wizard.Msgs.Msg
onUnload route model =
    case route of
        DetailRoute _ ->
            Wizard.Questionnaires.Detail.Update.onUnload model.detailModel

        _ ->
            Cmd.none


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Questionnaires.Create.Update.update (wrapMsg << CreateMsg) cMsg appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        CreateMigrationMsg cmMsg ->
            let
                ( createMigrationModel, cmd ) =
                    Wizard.Questionnaires.CreateMigration.Update.update (wrapMsg << CreateMigrationMsg) cmMsg appState model.createMigrationModel
            in
            ( appState.seed, { model | createMigrationModel = createMigrationModel }, cmd )

        DetailMsg dMsg ->
            let
                ( newSeed, detailModel, cmd ) =
                    Wizard.Questionnaires.Detail.Update.update (wrapMsg << DetailMsg) dMsg appState model.detailModel
            in
            ( newSeed, { model | detailModel = detailModel }, cmd )

        EditMsg eMsg ->
            let
                ( editModel, cmd ) =
                    Wizard.Questionnaires.Edit.Update.update (wrapMsg << EditMsg) eMsg appState model.editModel
            in
            ( appState.seed, { model | editModel = editModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Questionnaires.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( newSeed, migrationModel, cmd ) =
                    Wizard.Questionnaires.Migration.Update.update (wrapMsg << MigrationMsg) mMsg appState model.migrationModel
            in
            ( newSeed, { model | migrationModel = migrationModel }, cmd )
