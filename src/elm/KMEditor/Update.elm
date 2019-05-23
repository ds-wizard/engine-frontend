module KMEditor.Update exposing (fetchData, isGuarded, update)

import Common.AppState exposing (AppState)
import KMEditor.Create.Update
import KMEditor.Editor.Models
import KMEditor.Editor.Update
import KMEditor.Index.Update
import KMEditor.Migration.Update
import KMEditor.Models exposing (Model)
import KMEditor.Msgs exposing (Msg(..))
import KMEditor.Publish.Update
import KMEditor.Routing exposing (Route(..))
import Msgs
import Random exposing (Seed)


fetchData : Route -> (Msg -> Msgs.Msg) -> Model -> AppState -> Cmd Msgs.Msg
fetchData route wrapMsg model appState =
    case route of
        CreateRoute _ ->
            KMEditor.Create.Update.fetchData (wrapMsg << CreateMsg) appState

        EditorRoute uuid ->
            if model.editorModel.kmUuid == uuid && KMEditor.Editor.Models.containsChanges model.editorModel then
                Cmd.none

            else
                KMEditor.Editor.Update.fetchData (wrapMsg << EditorMsg) uuid appState

        IndexRoute ->
            KMEditor.Index.Update.fetchData (wrapMsg << IndexMsg) appState

        MigrationRoute uuid ->
            KMEditor.Migration.Update.fetchData (wrapMsg << MigrationMsg) uuid appState

        PublishRoute uuid ->
            Cmd.map (wrapMsg << PublishMsg) <|
                KMEditor.Publish.Update.fetchData uuid appState


isGuarded : Route -> Model -> Maybe String
isGuarded route model =
    case route of
        EditorRoute _ ->
            KMEditor.Editor.Update.isGuarded model.editorModel

        _ ->
            Nothing


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    KMEditor.Create.Update.update cMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        EditorMsg e2Msg ->
            let
                ( newSeed, editorModel, cmd ) =
                    KMEditor.Editor.Update.update e2Msg (wrapMsg << EditorMsg) appState model.editorModel
            in
            ( newSeed, { model | editorModel = editorModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    KMEditor.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( migrationModel, cmd ) =
                    KMEditor.Migration.Update.update mMsg (wrapMsg << MigrationMsg) appState model.migrationModel
            in
            ( appState.seed, { model | migrationModel = migrationModel }, cmd )

        PublishMsg pMsg ->
            let
                ( publishModel, cmd ) =
                    KMEditor.Publish.Update.update pMsg (wrapMsg << PublishMsg) appState model.publishModel
            in
            ( appState.seed, { model | publishModel = publishModel }, cmd )
