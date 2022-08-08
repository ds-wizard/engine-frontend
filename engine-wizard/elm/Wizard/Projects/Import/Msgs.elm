module Wizard.Projects.Import.Msgs exposing (Msg(..))

import Json.Encode as E
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)
    | GetQuestionnaireImporterComplete (Result ApiError QuestionnaireImporter)
    | GotImporterData E.Value
    | PutImportData
    | PutImporterDataComplete (Result ApiError ())
