module Wizard.DocumentTemplateEditors.Update exposing (fetchData, isGuarded, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplateEditors.Create.Update
import Wizard.DocumentTemplateEditors.Editor.Update
import Wizard.DocumentTemplateEditors.Index.Update
import Wizard.DocumentTemplateEditors.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ _ ->
            Cmd.map CreateMsg <|
                Wizard.DocumentTemplateEditors.Create.Update.fetchData appState model.createModel

        EditorRoute documentTemplateId subroute ->
            Cmd.map EditorMsg <|
                Wizard.DocumentTemplateEditors.Editor.Update.fetchData appState documentTemplateId subroute model.editorModel

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.DocumentTemplateEditors.Index.Update.fetchData


isGuarded : Route -> AppState -> Wizard.Routes.Route -> Model -> Maybe String
isGuarded route appState nextRoute model =
    case route of
        EditorRoute _ _ ->
            Wizard.DocumentTemplateEditors.Editor.Update.isGuarded appState nextRoute model.editorModel

        _ ->
            Nothing


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
