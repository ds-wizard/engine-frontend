module Wizard.Documents.Create.Msgs exposing (..)

import Form
import Shared.Data.Document exposing (Document)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetQuestionnairesCompleted (Result ApiError (Pagination Questionnaire))
    | GetTemplatesCompleted (Result ApiError (List Template))
    | FormMsg Form.Msg
    | PostDocumentCompleted (Result ApiError Document)
