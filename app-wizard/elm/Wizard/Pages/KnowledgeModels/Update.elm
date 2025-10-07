module Wizard.Pages.KnowledgeModels.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Detail.Update
import Wizard.Pages.KnowledgeModels.Import.Update
import Wizard.Pages.KnowledgeModels.Index.Update
import Wizard.Pages.KnowledgeModels.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Msgs exposing (Msg(..))
import Wizard.Pages.KnowledgeModels.Preview.Update
import Wizard.Pages.KnowledgeModels.ResourcePage.Update
import Wizard.Pages.KnowledgeModels.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        DetailRoute packageId ->
            Cmd.map DetailMsg <|
                Wizard.Pages.KnowledgeModels.Detail.Update.fetchData packageId appState

        IndexRoute _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.KnowledgeModels.Index.Update.fetchData

        PreviewRoute packageId _ ->
            Cmd.map PreviewMsg <|
                Wizard.Pages.KnowledgeModels.Preview.Update.fetchData appState packageId

        ResourcePageRoute kmId _ ->
            Cmd.map ResourcePageMsg <|
                Wizard.Pages.KnowledgeModels.ResourcePage.Update.fetchData appState kmId

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        DetailMsg dMsg ->
            let
                ( detailModel, cmd ) =
                    Wizard.Pages.KnowledgeModels.Detail.Update.update dMsg (wrapMsg << DetailMsg) appState model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )

        ImportMsg impMsg ->
            let
                ( importModel, cmd ) =
                    Wizard.Pages.KnowledgeModels.Import.Update.update impMsg (wrapMsg << ImportMsg) appState model.importModel
            in
            ( appState.seed, { model | importModel = importModel }, cmd )

        IndexMsg iMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.KnowledgeModels.Index.Update.update iMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        PreviewMsg pMsg ->
            let
                ( newSeed, projectModel, cmd ) =
                    Wizard.Pages.KnowledgeModels.Preview.Update.update pMsg (wrapMsg << PreviewMsg) appState model.previewModel
            in
            ( newSeed, { model | previewModel = projectModel }, cmd )

        ResourcePageMsg rpMsg ->
            let
                ( resourcePageModel, cmd ) =
                    Wizard.Pages.KnowledgeModels.ResourcePage.Update.update appState rpMsg model.resourcePageModel
            in
            ( appState.seed, { model | resourcePageModel = resourcePageModel }, cmd )
