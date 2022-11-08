module Wizard.DocumentTemplateEditors.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplateEditors.Create.Update
import Wizard.DocumentTemplateEditors.Editor.Update
import Wizard.DocumentTemplateEditors.Index.Update
import Wizard.DocumentTemplateEditors.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Msgs


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ _ ->
            Cmd.map CreateMsg <|
                Wizard.DocumentTemplateEditors.Create.Update.fetchData appState model.createModel

        EditorRoute packageId ->
            Cmd.map EditorMsg <|
                Wizard.DocumentTemplateEditors.Editor.Update.fetchData packageId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.DocumentTemplateEditors.Index.Update.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.DocumentTemplateEditors.Create.Update.update cMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        EditorMsg eMsg ->
            let
                ( newSeed, editorModel, cmd ) =
                    Wizard.DocumentTemplateEditors.Editor.Update.update appState (wrapMsg << EditorMsg) eMsg model.editorModel
            in
            ( newSeed, { model | editorModel = editorModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.DocumentTemplateEditors.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
