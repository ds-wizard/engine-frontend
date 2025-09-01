module Wizard.Pages.KMEditor.Models exposing (Model, initLocalModel, initialModel)

import Random exposing (Seed)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Create.Models
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.Pages.KMEditor.Editor.Models as Editor
import Wizard.Pages.KMEditor.Index.Models
import Wizard.Pages.KMEditor.Migration.Models
import Wizard.Pages.KMEditor.Publish.Models
import Wizard.Pages.KMEditor.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Pages.KMEditor.Create.Models.Model
    , editorModel : Editor.Model
    , indexModel : Wizard.Pages.KMEditor.Index.Models.Model
    , migrationModel : Wizard.Pages.KMEditor.Migration.Models.Model
    , publishModel : Wizard.Pages.KMEditor.Publish.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Pages.KMEditor.Create.Models.initialModel appState Nothing Nothing
    , editorModel = Editor.init appState Uuid.nil Nothing
    , indexModel = Wizard.Pages.KMEditor.Index.Models.initialModel PaginationQueryString.empty
    , migrationModel = Wizard.Pages.KMEditor.Migration.Models.initialModel Uuid.nil
    , publishModel = Wizard.Pages.KMEditor.Publish.Models.initialModel
    }


initLocalModel : AppState -> Route -> Model -> ( Seed, Model )
initLocalModel appState route model =
    let
        withSeed m =
            ( appState.seed, m )
    in
    case route of
        CreateRoute selectedPackage edit ->
            withSeed { model | createModel = Wizard.Pages.KMEditor.Create.Models.initialModel appState selectedPackage edit }

        EditorRoute uuid subroute ->
            if uuid == model.editorModel.uuid then
                let
                    ( newSeed, editorModel ) =
                        Editor.initPageModel appState subroute model.editorModel
                in
                ( newSeed
                , { model | editorModel = editorModel }
                )

            else
                let
                    mbEditorUuid =
                        case subroute of
                            KMEditorRoute.Edit mbUuid ->
                                mbUuid

                            _ ->
                                Nothing

                    ( newSeed, editorModel ) =
                        Editor.initPageModel appState subroute <| Editor.init appState uuid mbEditorUuid
                in
                ( newSeed
                , { model | editorModel = editorModel }
                )

        IndexRoute paginationQueryString ->
            withSeed { model | indexModel = Wizard.Pages.KMEditor.Index.Models.initialModel paginationQueryString }

        MigrationRoute uuid ->
            withSeed { model | migrationModel = Wizard.Pages.KMEditor.Migration.Models.initialModel uuid }

        PublishRoute _ ->
            withSeed { model | publishModel = Wizard.Pages.KMEditor.Publish.Models.initialModel }
