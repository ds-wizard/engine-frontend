module Wizard.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Version exposing (Version)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostDocumentTemplateDraftCompleted (Result ApiError DocumentTemplateDraftDetail)
    | DocumentTemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)
