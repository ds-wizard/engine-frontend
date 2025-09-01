module Wizard.Pages.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Version exposing (Version)
import Wizard.Api.Models.CreatedEntityWithId exposing (CreatedEntityWithId)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Components.TypeHintInput as TypeHintInput


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostDocumentTemplateDraftCompleted (Result ApiError CreatedEntityWithId)
    | DocumentTemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)
