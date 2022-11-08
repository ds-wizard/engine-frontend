module Wizard.DocumentTemplateEditors.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Shared.Data.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Shared.Data.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Form.FormError exposing (FormError)
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


initialModel : Maybe String -> Maybe Bool -> Model
initialModel selectedDocumentTemplate edit =
    { savingDocumentTemplate = ActionResult.Unset
    , form = DocumentTemplateEditorCreateForm.init selectedDocumentTemplate
    , documentTemplateTypeHintInputModel = TypeHintInput.init "basedOn"
    , documentTemplate = ActionResult.Loading
    , selectedDocumentTemplate = selectedDocumentTemplate
    , edit = Maybe.withDefault False edit
    }
