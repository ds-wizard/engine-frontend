module Wizard.Pages.DocumentTemplateEditors.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Create.Models
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute
import Wizard.Pages.DocumentTemplateEditors.Editor.Models
import Wizard.Pages.DocumentTemplateEditors.Index.Models
import Wizard.Pages.DocumentTemplateEditors.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Pages.DocumentTemplateEditors.Create.Models.Model
    , indexModel : Wizard.Pages.DocumentTemplateEditors.Index.Models.Model
    , editorModel : Wizard.Pages.DocumentTemplateEditors.Editor.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Pages.DocumentTemplateEditors.Create.Models.initialModel appState Nothing Nothing
    , indexModel = Wizard.Pages.DocumentTemplateEditors.Index.Models.initialModel PaginationQueryString.empty
    , editorModel = Wizard.Pages.DocumentTemplateEditors.Editor.Models.initialModel appState "" DTEditorRoute.Settings
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute selectedDocumentTemplate edit ->
            { model | createModel = Wizard.Pages.DocumentTemplateEditors.Create.Models.initialModel appState selectedDocumentTemplate edit }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.Pages.DocumentTemplateEditors.Index.Models.initialModel paginationQueryString }

        EditorRoute documentTemplateId subroute ->
            if documentTemplateId == model.editorModel.documentTemplateId then
                { model | editorModel = Wizard.Pages.DocumentTemplateEditors.Editor.Models.setEditorFromRoute subroute model.editorModel }

            else
                { model | editorModel = Wizard.Pages.DocumentTemplateEditors.Editor.Models.initialModel appState documentTemplateId subroute }
