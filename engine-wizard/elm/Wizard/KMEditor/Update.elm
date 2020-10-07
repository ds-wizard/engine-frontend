module Wizard.KMEditor.Update exposing (fetchData, isGuarded, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Create.Update
import Wizard.KMEditor.Editor.Models
import Wizard.KMEditor.Editor.Update
import Wizard.KMEditor.Index.Update
import Wizard.KMEditor.Migration.Update
import Wizard.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Publish.Update
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> Model -> AppState -> Cmd Msg
fetchData route model appState =
    case route of
        CreateRoute _ ->
            Cmd.map CreateMsg <|
                Wizard.KMEditor.Create.Update.fetchData appState

        EditorRoute uuid ->
            if model.editorModel.kmUuid == uuid && Wizard.KMEditor.Editor.Models.containsChanges model.editorModel then
                Cmd.none

            else
                Cmd.map EditorMsg <|
                    Wizard.KMEditor.Editor.Update.fetchData uuid appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.KMEditor.Index.Update.fetchData

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Wizard.KMEditor.Migration.Update.fetchData uuid appState

        PublishRoute uuid ->
            Cmd.map PublishMsg <|
                Wizard.KMEditor.Publish.Update.fetchData uuid appState


isGuarded : Route -> AppState -> Model -> Maybe String
isGuarded route appState model =
    case route of
        EditorRoute _ ->
            Wizard.KMEditor.Editor.Update.isGuarded appState model.editorModel

        _ ->
            Nothing


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
                    Wizard.KMEditor.Editor.Update.update e2Msg (wrapMsg << EditorMsg) appState model.editorModel
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
