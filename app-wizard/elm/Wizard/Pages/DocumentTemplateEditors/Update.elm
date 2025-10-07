module Wizard.Pages.DocumentTemplateEditors.Update exposing (fetchData, isGuarded, update)

import Random exposing (Seed)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplateEditors.Create.Update
import Wizard.Pages.DocumentTemplateEditors.Editor.Update
import Wizard.Pages.DocumentTemplateEditors.Index.Update
import Wizard.Pages.DocumentTemplateEditors.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Msgs exposing (Msg(..))
import Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Routes


fetchData : Route -> AppState -> Model -> Cmd Msg
fetchData route appState model =
    case route of
        CreateRoute _ _ ->
            Cmd.map CreateMsg <|
                Wizard.Pages.DocumentTemplateEditors.Create.Update.fetchData appState model.createModel

        EditorRoute documentTemplateId subroute ->
            Cmd.map EditorMsg <|
                Wizard.Pages.DocumentTemplateEditors.Editor.Update.fetchData appState documentTemplateId subroute model.editorModel

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.DocumentTemplateEditors.Index.Update.fetchData


isGuarded : Route -> AppState -> Wizard.Routes.Route -> Model -> Maybe String
isGuarded route appState nextRoute model =
    case route of
        EditorRoute _ _ ->
            Wizard.Pages.DocumentTemplateEditors.Editor.Update.isGuarded appState nextRoute model.editorModel

        _ ->
            Nothing


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg cMsg ->
            let
                ( createModel, cmd ) =
                    Wizard.Pages.DocumentTemplateEditors.Create.Update.update cMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        EditorMsg eMsg ->
            let
                ( newSeed, editorModel, cmd ) =
                    Wizard.Pages.DocumentTemplateEditors.Editor.Update.update appState (wrapMsg << EditorMsg) eMsg model.editorModel
            in
            ( newSeed, { model | editorModel = editorModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.DocumentTemplateEditors.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
