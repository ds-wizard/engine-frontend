module KMEditor.Update exposing (fetchData, isGuarded, update)

import Auth.Models exposing (Session)
import KMEditor.Create.Update
import KMEditor.Editor.Models exposing (containsChanges)
import KMEditor.Editor.Update
import KMEditor.Index.Update
import KMEditor.Migration.Update
import KMEditor.Models exposing (Model)
import KMEditor.Msgs exposing (Msg(..))
import KMEditor.Publish.Update
import KMEditor.Routing exposing (Route(..))
import Models exposing (State)
import Msgs
import Random exposing (Seed)


fetchData : Route -> (Msg -> Msgs.Msg) -> Model -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg model session =
    case route of
        Create _ ->
            KMEditor.Create.Update.fetchData (wrapMsg << CreateMsg) session

        Editor uuid ->
            if model.editorModel.branchUuid == uuid && containsChanges model.editorModel then
                Cmd.none

            else
                KMEditor.Editor.Update.fetchData (wrapMsg << EditorMsg) uuid session

        Index ->
            KMEditor.Index.Update.fetchData (wrapMsg << IndexMsg) session

        Migration uuid ->
            KMEditor.Migration.Update.fetchData (wrapMsg << MigrationMsg) uuid session

        Publish uuid ->
            KMEditor.Publish.Update.fetchData (wrapMsg << PublishMsg) uuid session


isGuarded : Route -> Model -> Maybe String
isGuarded route model =
    case route of
        Editor uuid ->
            KMEditor.Editor.Update.isGuarded model.editorModel

        _ ->
            Nothing


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    KMEditor.Create.Update.update cMsg (wrapMsg << CreateMsg) state model.createModel
            in
            ( state.seed, { model | createModel = createModel }, cmd )

        EditorMsg eMsg ->
            let
                ( newSeed, editorModel, cmd ) =
                    KMEditor.Editor.Update.update eMsg (wrapMsg << EditorMsg) state model.editorModel
            in
            ( newSeed, { model | editorModel = editorModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    KMEditor.Index.Update.update iMsg (wrapMsg << IndexMsg) state model.indexModel
            in
            ( state.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( migrationModel, cmd ) =
                    KMEditor.Migration.Update.update mMsg (wrapMsg << MigrationMsg) state.session model.migrationModel
            in
            ( state.seed, { model | migrationModel = migrationModel }, cmd )

        PublishMsg pMsg ->
            let
                ( publishModel, cmd ) =
                    KMEditor.Publish.Update.update pMsg (wrapMsg << PublishMsg) state model.publishModel
            in
            ( state.seed, { model | publishModel = publishModel }, cmd )
