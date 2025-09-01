module Wizard.Pages.Projects.Update exposing
    ( fetchData
    , isGuarded
    , onUnload
    , update
    )

import Random exposing (Seed)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Create.Update
import Wizard.Pages.Projects.CreateMigration.Update
import Wizard.Pages.Projects.Detail.Update
import Wizard.Pages.Projects.DocumentDownload.Update
import Wizard.Pages.Projects.FileDownload.Update
import Wizard.Pages.Projects.Import.Update
import Wizard.Pages.Projects.Index.Update
import Wizard.Pages.Projects.Migration.Update
import Wizard.Pages.Projects.Models exposing (Model)
import Wizard.Pages.Projects.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Routes exposing (Route(..))
import Wizard.Routes


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ _ ->
            Cmd.map CreateMsg <|
                Wizard.Pages.Projects.Create.Update.fetchData appState model.createModel

        CreateMigrationRoute uuid ->
            Cmd.map CreateMigrationMsg <|
                Wizard.Pages.Projects.CreateMigration.Update.fetchData appState uuid

        DetailRoute uuid _ ->
            Cmd.map DetailMsg <|
                Wizard.Pages.Projects.Detail.Update.fetchData appState uuid model.detailModel

        IndexRoute _ _ _ _ _ _ _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.Projects.Index.Update.fetchData appState model.indexModel

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Wizard.Pages.Projects.Migration.Update.fetchData appState uuid

        ImportRoute uuid importerId ->
            Cmd.map ImportMsg <|
                Wizard.Pages.Projects.Import.Update.fetchData appState uuid importerId

        DocumentDownloadRoute _ documentUuid ->
            Cmd.map DocumentDownloadMsg <|
                Wizard.Pages.Projects.DocumentDownload.Update.fetchData appState documentUuid

        FileDownloadRoute projectUuid fileUuid ->
            Cmd.map FileDownloadMsg <|
                Wizard.Pages.Projects.FileDownload.Update.fetchData appState projectUuid fileUuid


isGuarded : Route -> AppState -> Wizard.Routes.Route -> Model -> Maybe String
isGuarded route appState nextRoute model =
    case route of
        DetailRoute _ _ ->
            Wizard.Pages.Projects.Detail.Update.isGuarded appState nextRoute model.detailModel

        _ ->
            Nothing


onUnload : Route -> Wizard.Routes.Route -> Model -> Cmd Msg
onUnload route nextRoute model =
    case route of
        DetailRoute _ _ ->
            Cmd.map DetailMsg <|
                Wizard.Pages.Projects.Detail.Update.onUnload nextRoute model.detailModel

        _ ->
            Cmd.none


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Pages.Projects.Create.Update.update (wrapMsg << CreateMsg) cMsg appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        CreateMigrationMsg cmMsg ->
            let
                ( createMigrationModel, cmd ) =
                    Wizard.Pages.Projects.CreateMigration.Update.update (wrapMsg << CreateMigrationMsg) cmMsg appState model.createMigrationModel
            in
            ( appState.seed, { model | createMigrationModel = createMigrationModel }, cmd )

        DetailMsg detailMsg ->
            let
                ( newSeed, detailModel, cmd ) =
                    Wizard.Pages.Projects.Detail.Update.update (wrapMsg << DetailMsg) detailMsg appState model.detailModel
            in
            ( newSeed, { model | detailModel = detailModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.Projects.Index.Update.update (wrapMsg << IndexMsg) iMsg appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( newSeed, migrationModel, cmd ) =
                    Wizard.Pages.Projects.Migration.Update.update (wrapMsg << MigrationMsg) mMsg appState model.migrationModel
            in
            ( newSeed, { model | migrationModel = migrationModel }, cmd )

        ImportMsg iMsg ->
            let
                ( newSeed, importModel, cmd ) =
                    Wizard.Pages.Projects.Import.Update.update (wrapMsg << ImportMsg) iMsg appState model.importModel
            in
            ( newSeed, { model | importModel = importModel }, cmd )

        DocumentDownloadMsg ddMsg ->
            let
                ( documentDownloadModel, cmd ) =
                    Wizard.Pages.Projects.DocumentDownload.Update.update appState ddMsg model.documentDownload
            in
            ( appState.seed, { model | documentDownload = documentDownloadModel }, Cmd.map (wrapMsg << DocumentDownloadMsg) cmd )

        FileDownloadMsg fdMsg ->
            let
                ( fileDownloadModel, cmd ) =
                    Wizard.Pages.Projects.FileDownload.Update.update appState fdMsg model.fileDownload
            in
            ( appState.seed, { model | fileDownload = fileDownloadModel }, Cmd.map (wrapMsg << FileDownloadMsg) cmd )
