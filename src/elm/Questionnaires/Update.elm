module Questionnaires.Update exposing (fetchData, update)

import Common.AppState exposing (AppState)
import Msgs
import Questionnaires.Create.Update
import Questionnaires.CreateMigration.Update
import Questionnaires.Detail.Update
import Questionnaires.Edit.Update
import Questionnaires.Index.Update
import Questionnaires.Migration.Update
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routes exposing (Route(..))


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ ->
            Cmd.map CreateMsg <|
                Questionnaires.Create.Update.fetchData appState model.createModel

        CreateMigrationRoute uuid ->
            Cmd.map CreateMigrationMsg <|
                Questionnaires.CreateMigration.Update.fetchData appState uuid

        DetailRoute uuid ->
            Cmd.map DetailMsg <|
                Questionnaires.Detail.Update.fetchData appState uuid

        EditRoute uuid ->
            Cmd.map EditMsg <|
                Questionnaires.Edit.Update.fetchData appState uuid

        IndexRoute ->
            Cmd.map IndexMsg <|
                Questionnaires.Index.Update.fetchData appState

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Questionnaires.Migration.Update.fetchData appState uuid


update : (Msg -> Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Questionnaires.Create.Update.update (wrapMsg << CreateMsg) cMsg appState model.createModel
            in
            ( { model | createModel = createModel }, cmd )

        CreateMigrationMsg cmMsg ->
            let
                ( createMigrationModel, cmd ) =
                    Questionnaires.CreateMigration.Update.update (wrapMsg << CreateMigrationMsg) cmMsg appState model.createMigrationModel
            in
            ( { model | createMigrationModel = createMigrationModel }, cmd )

        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Questionnaires.Detail.Update.update (wrapMsg << DetailMsg) dMsg appState model.detailModel
            in
            ( { model | detailModel = detailModel }, cmd )

        EditMsg eMsg ->
            let
                ( editModel, cmd ) =
                    Questionnaires.Edit.Update.update (wrapMsg << EditMsg) eMsg appState model.editModel
            in
            ( { model | editModel = editModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Questionnaires.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( migrationModel, cmd ) =
                    Questionnaires.Migration.Update.update (wrapMsg << MigrationMsg) mMsg appState model.migrationModel
            in
            ( { model | migrationModel = migrationModel }, cmd )
