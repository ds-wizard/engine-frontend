module Wizard.Pages.KMEditor.Update exposing (fetchData, isGuarded, onUnload, update)

import Random exposing (Seed)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Create.Update
import Wizard.Pages.KMEditor.Editor.Update
import Wizard.Pages.KMEditor.Index.Update
import Wizard.Pages.KMEditor.Migration.Update
import Wizard.Pages.KMEditor.Models exposing (Model)
import Wizard.Pages.KMEditor.Msgs exposing (Msg(..))
import Wizard.Pages.KMEditor.Publish.Update
import Wizard.Pages.KMEditor.Routes exposing (Route(..))
import Wizard.Routes


fetchData : Route -> Model -> AppState -> Cmd Msg
fetchData route model appState =
    case route of
        CreateRoute _ _ ->
            Cmd.map CreateMsg <|
                Wizard.Pages.KMEditor.Create.Update.fetchData appState model.createModel

        EditorRoute uuid _ ->
            Cmd.map EditorMsg <|
                Wizard.Pages.KMEditor.Editor.Update.fetchData appState uuid model.editorModel

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.KMEditor.Index.Update.fetchData

        MigrationRoute uuid ->
            Cmd.map MigrationMsg <|
                Wizard.Pages.KMEditor.Migration.Update.fetchData uuid appState

        PublishRoute uuid ->
            Cmd.map PublishMsg <|
                Wizard.Pages.KMEditor.Publish.Update.fetchData uuid appState


isGuarded : Route -> AppState -> Wizard.Routes.Route -> Model -> Maybe String
isGuarded route appState nextRoute model =
    case route of
        EditorRoute _ _ ->
            Wizard.Pages.KMEditor.Editor.Update.isGuarded appState nextRoute model.editorModel

        _ ->
            Nothing


onUnload : Route -> Wizard.Routes.Route -> Model -> Cmd Msg
onUnload route nextRoute model =
    case route of
        EditorRoute _ _ ->
            Cmd.map EditorMsg <|
                Wizard.Pages.KMEditor.Editor.Update.onUnload nextRoute model.editorModel

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Pages.KMEditor.Create.Update.update cMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        EditorMsg e2Msg ->
            let
                ( newSeed, editorModel, cmd ) =
                    Wizard.Pages.KMEditor.Editor.Update.update (wrapMsg << EditorMsg) e2Msg appState model.editorModel
            in
            ( newSeed, { model | editorModel = editorModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.KMEditor.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        MigrationMsg mMsg ->
            let
                ( migrationModel, cmd ) =
                    Wizard.Pages.KMEditor.Migration.Update.update mMsg (wrapMsg << MigrationMsg) appState model.migrationModel
            in
            ( appState.seed, { model | migrationModel = migrationModel }, cmd )

        PublishMsg pMsg ->
            let
                ( publishModel, cmd ) =
                    Wizard.Pages.KMEditor.Publish.Update.update pMsg (wrapMsg << PublishMsg) appState model.publishModel
            in
            ( appState.seed, { model | publishModel = publishModel }, cmd )
