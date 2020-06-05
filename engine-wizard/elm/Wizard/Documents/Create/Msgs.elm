module Wizard.Documents.Create.Msgs exposing (..)

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Pagination.Pagination exposing (Pagination)
import Wizard.Documents.Common.Document exposing (Document)
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type Msg
    = GetQuestionnairesCompleted (Result ApiError (Pagination Questionnaire))
    | GetTemplatesCompleted (Result ApiError (List Template))
    | FormMsg Form.Msg
    | PostDocumentCompleted (Result ApiError Document)
