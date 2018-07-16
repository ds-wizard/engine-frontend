module KMEditor.Update exposing (..)

import Auth.Models exposing (Session)
import KMEditor.Create.Update
import KMEditor.Editor.Update
import KMEditor.Index.Update
import KMEditor.Migration.Update
import KMEditor.Models exposing (Model)
import KMEditor.Msgs exposing (Msg(..))
import KMEditor.Publish.Update
import KMEditor.Routing exposing (Route(..))
import Msgs
import Random.Pcg exposing (Seed)


fetchData : Route -> (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData route wrapMsg session =
    case route of
        Create _ ->
            KMEditor.Create.Update.fetchData (wrapMsg << CreateMsg) session

        Editor uuid ->
            KMEditor.Editor.Update.fetchData (wrapMsg << EditorMsg) uuid session

        Index ->
            KMEditor.Index.Update.fetchData (wrapMsg << IndexMsg) session

        Migration uuid ->
            KMEditor.Migration.Update.fetchData (wrapMsg << MigrationMsg) uuid session

        Publish uuid ->
            KMEditor.Publish.Update.fetchData (wrapMsg << PublishMsg) uuid session


update : Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg seed session model =
    case msg of
        CreateMsg msg ->
            let
                ( newSeed, createModel, cmd ) =
                    KMEditor.Create.Update.update msg (wrapMsg << CreateMsg) seed session model.createModel
            in
            ( newSeed, { model | createModel = createModel }, cmd )

        EditorMsg msg ->
            let
                ( newSeed, editor2Model, cmd ) =
                    KMEditor.Editor.Update.update msg (wrapMsg << EditorMsg) seed session model.editor2Model
            in
            ( newSeed, { model | editor2Model = editor2Model }, cmd )

        IndexMsg msg ->
            let
                ( indexModel, cmd ) =
                    KMEditor.Index.Update.update msg (wrapMsg << IndexMsg) session model.indexModel
            in
            ( seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg msg ->
            let
                ( migrationModel, cmd ) =
                    KMEditor.Migration.Update.update msg (wrapMsg << MigrationMsg) session model.migrationModel
            in
            ( seed, { model | migrationModel = migrationModel }, cmd )

        PublishMsg msg ->
            let
                ( publishModel, cmd ) =
                    KMEditor.Publish.Update.update msg (wrapMsg << PublishMsg) session model.publishModel
            in
            ( seed, { model | publishModel = publishModel }, cmd )
