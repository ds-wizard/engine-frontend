module Wizard.DocumentTemplateEditors.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplateEditors.Create.Models
import Wizard.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute
import Wizard.DocumentTemplateEditors.Editor.Models
import Wizard.DocumentTemplateEditors.Index.Models
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.DocumentTemplateEditors.Create.Models.Model
    , indexModel : Wizard.DocumentTemplateEditors.Index.Models.Model
    , editorModel : Wizard.DocumentTemplateEditors.Editor.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.DocumentTemplateEditors.Create.Models.initialModel appState Nothing Nothing
    , indexModel = Wizard.DocumentTemplateEditors.Index.Models.initialModel PaginationQueryString.empty
    , editorModel = Wizard.DocumentTemplateEditors.Editor.Models.initialModel appState "" DTEditorRoute.Settings
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute selectedDocumentTemplate edit ->
            { model | createModel = Wizard.DocumentTemplateEditors.Create.Models.initialModel appState selectedDocumentTemplate edit }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.DocumentTemplateEditors.Index.Models.initialModel paginationQueryString }

        EditorRoute documentTemplateId subroute ->
            if documentTemplateId == model.editorModel.documentTemplateId then
                { model | editorModel = Wizard.DocumentTemplateEditors.Editor.Models.setEditorFromRoute subroute model.editorModel }

            else
                { model | editorModel = Wizard.DocumentTemplateEditors.Editor.Models.initialModel appState documentTemplateId subroute }
