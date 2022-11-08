module Wizard.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Data.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Error.ApiError exposing (ApiError)
import Version exposing (Version)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = FormMsg Form.Msg
    | FormSetVersion Version
    | PostDocumentTemplateDraftCompleted (Result ApiError DocumentTemplateDraftDetail)
    | DocumentTemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)
