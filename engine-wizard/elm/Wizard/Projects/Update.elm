module Wizard.Projects.Update exposing
    ( fetchData
    , isGuarded
    , onUnload
    , update
    )

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Projects.Create.Update
import Wizard.Projects.CreateMigration.Update
import Wizard.Projects.Detail.Update
import Wizard.Projects.Import.Update
import Wizard.Projects.Index.Update
import Wizard.Projects.Migration.Update
import Wizard.Projects.Models exposing (Model)
import Wizard.Projects.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ ->
            Cmd.map CreateMsg <|
                Wizard.Projects.Create.Update.fetchData appState model.createModel

        CreateMigrationRoute uuid ->
            Cmd.map CreateMigrationMsg <|
                Wizard.Projects.CreateMigration.Update.fetchData appState uuid

        DetailRoute uuid _ ->
            Cmd.map DetailMsg <|
                Wizard.Projects.Detail.Update.fetchData appState uuid model.detailModel

        IndexRoute _ _ _ _ _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Projects.Index.Update.fetchData appState model.indexModel

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Wizard.Projects.Migration.Update.fetchData appState uuid

        ImportRoute uuid importerId ->
            Cmd.map ImportMsg <|
                Wizard.Projects.Import.Update.fetchData appState uuid importerId


isGuarded : Route -> AppState -> Wizard.Routes.Route -> Model -> Maybe String
isGuarded route appState nextRoute model =
    case route of
        DetailRoute _ _ ->
            Wizard.Projects.Detail.Update.isGuarded appState nextRoute model.detailModel

        _ ->
            Nothing


onUnload : Route -> Wizard.Routes.Route -> Model -> Cmd Msg
onUnload route nextRoute model =
    case route of
        DetailRoute _ _ ->
            Cmd.map DetailMsg <|
                Wizard.Projects.Detail.Update.onUnload nextRoute model.detailModel

        _ ->
            Cmd.none


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Projects.Create.Update.update (wrapMsg << CreateMsg) cMsg appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        CreateMigrationMsg cmMsg ->
            let
                ( createMigrationModel, cmd ) =
                    Wizard.Projects.CreateMigration.Update.update (wrapMsg << CreateMigrationMsg) cmMsg appState model.createMigrationModel
            in
            ( appState.seed, { model | createMigrationModel = createMigrationModel }, cmd )

        DetailMsg detailMsg ->
            let
                ( newSeed, detailModel, cmd ) =
                    Wizard.Projects.Detail.Update.update (wrapMsg << DetailMsg) detailMsg appState model.detailModel
            in
            ( newSeed, { model | detailModel = detailModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Projects.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( newSeed, migrationModel, cmd ) =
                    Wizard.Projects.Migration.Update.update (wrapMsg << MigrationMsg) mMsg appState model.migrationModel
            in
            ( newSeed, { model | migrationModel = migrationModel }, cmd )

        ImportMsg iMsg ->
            let
                ( newSeed, importModel, cmd ) =
                    Wizard.Projects.Import.Update.update (wrapMsg << ImportMsg) iMsg appState model.importModel
            in
            ( newSeed, { model | importModel = importModel }, cmd )
