module Wizard.DocumentTemplateEditors.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.DocumentTemplateEditors.Common.DocumentTemplateEditorCreateForm as DocumentTemplateEditorCreateForm exposing (DocumentTemplateEditorCreateForm)


type alias Model =
    { savingDocumentTemplate : ActionResult ()
    , form : Form FormError DocumentTemplateEditorCreateForm
    , documentTemplateTypeHintInputModel : TypeHintInput.Model DocumentTemplateSuggestion
    , documentTemplate : ActionResult DocumentTemplateDetail
    , selectedDocumentTemplate : Maybe String
    , edit : Bool
    }


initialModel : AppState -> Maybe String -> Maybe Bool -> Model
initialModel appState selectedDocumentTemplate edit =
    { savingDocumentTemplate = ActionResult.Unset
    , form = DocumentTemplateEditorCreateForm.init appState selectedDocumentTemplate
    , documentTemplateTypeHintInputModel = TypeHintInput.init "basedOn"
    , documentTemplate = ActionResult.Loading
    , selectedDocumentTemplate = selectedDocumentTemplate
    , edit = Maybe.withDefault False edit
    }
