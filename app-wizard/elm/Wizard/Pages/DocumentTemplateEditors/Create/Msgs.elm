module Wizard.Pages.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UuidResponse exposing (UuidResponse)
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Version exposing (Version)
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostDocumentTemplateDraftCompleted (Result ApiError UuidResponse)
    | DocumentTemplateTypeHintInputMsg (TypeHintInput.Msg DocumentTemplateSuggestion)
    | GetDocumentTemplateCompleted (Result ApiError DocumentTemplateDetail)
