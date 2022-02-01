module Wizard.KMEditor.Update exposing (fetchData, onUnload, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Create.Update
import Wizard.KMEditor.Editor.Update
import Wizard.KMEditor.Index.Update
import Wizard.KMEditor.Migration.Update
import Wizard.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Publish.Update
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes


fetchData : Route -> Model -> AppState -> Cmd Msg
fetchData route model appState =
    case route of
        CreateRoute _ _ ->
            Cmd.map CreateMsg <|
                Wizard.KMEditor.Create.Update.fetchData appState model.createModel

        EditorRoute uuid _ ->
            Cmd.map EditorMsg <|
                Wizard.KMEditor.Editor.Update.fetchData appState uuid model.editorModel

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.KMEditor.Index.Update.fetchData

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Wizard.KMEditor.Migration.Update.fetchData uuid appState

        PublishRoute uuid ->
            Cmd.map PublishMsg <|
                Wizard.KMEditor.Publish.Update.fetchData uuid appState


onUnload : Route -> Wizard.Routes.Route -> Model -> Cmd Msg
onUnload route newRoute model =
    case route of
        EditorRoute _ _ ->
            Cmd.map EditorMsg <|
                Wizard.KMEditor.Editor.Update.onUnload newRoute model.editorModel

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.KMEditor.Create.Update.update cMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        EditorMsg e2Msg ->
            let
                ( newSeed, editorModel, cmd ) =
                    Wizard.KMEditor.Editor.Update.update (wrapMsg << EditorMsg) e2Msg appState model.editorModel
            in
            ( newSeed, { model | editorModel = editorModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.KMEditor.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( migrationModel, cmd ) =
                    Wizard.KMEditor.Migration.Update.update mMsg (wrapMsg << MigrationMsg) appState model.migrationModel
            in
            ( appState.seed, { model | migrationModel = migrationModel }, cmd )

        PublishMsg pMsg ->
            let
                ( publishModel, cmd ) =
                    Wizard.KMEditor.Publish.Update.update pMsg (wrapMsg << PublishMsg) appState model.publishModel
            in
            ( appState.seed, { model | publishModel = publishModel }, cmd )
