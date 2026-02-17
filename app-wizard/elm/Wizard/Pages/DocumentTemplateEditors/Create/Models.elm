module Wizard.Pages.DocumentTemplateEditors.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Uuid exposing (Uuid)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Common.DocumentTemplateEditorCreateForm as DocumentTemplateEditorCreateForm exposing (DocumentTemplateEditorCreateForm)


type alias Model =
    { savingDocumentTemplate : ActionResult ()
    , form : Form FormError DocumentTemplateEditorCreateForm
    , documentTemplateTypeHintInputModel : TypeHintInput.Model DocumentTemplateSuggestion
    , documentTemplate : ActionResult DocumentTemplateDetail
    , selectedDocumentTemplateUuid : Maybe Uuid
    , edit : Bool
    }


initialModel : AppState -> Maybe Uuid -> Maybe Bool -> Model
initialModel appState selectedDocumentTemplateUuid edit =
    { savingDocumentTemplate = ActionResult.Unset
    , form = DocumentTemplateEditorCreateForm.init appState selectedDocumentTemplateUuid
    , documentTemplateTypeHintInputModel = TypeHintInput.init "basedOn"
    , documentTemplate = ActionResult.Loading
    , selectedDocumentTemplateUuid = selectedDocumentTemplateUuid
    , edit = Maybe.withDefault False edit
    }
