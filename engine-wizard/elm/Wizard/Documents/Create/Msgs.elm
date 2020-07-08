module Wizard.Documents.Create.Msgs exposing (..)

import Form
import Shared.Data.Document exposing (Document)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | GetTemplatesCompleted (Result ApiError (List Template))
    | FormMsg Form.Msg
    | PostDocumentCompleted (Result ApiError Document)
