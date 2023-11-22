module Wizard.Projects.Import.Msgs exposing (Msg(..))

import Json.Decode as D
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.Importer.ImporterEvent exposing (ImporterEvent)
import Wizard.Projects.Import.Models exposing (SidePanel)


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)
    | GetQuestionnaireImporterComplete (Result ApiError QuestionnaireImporter)
    | FetchKnowledgeModelStringComplete (Result ApiError String)
    | GotImporterData (Result D.Error (List ImporterEvent))
    | PutImportData
    | PutImporterDataComplete (Result ApiError ())
    | QuestionnaireMsg Questionnaire.Msg
    | ChangeSidePanel SidePanel
