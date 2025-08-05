module Wizard.Projects.Import.Msgs exposing (Msg(..))

import Json.Decode as D
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Api.Models.QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.Questionnaire.Importer.ImporterEvent exposing (ImporterEvent)
import Wizard.Projects.Import.Models exposing (SidePanel)


type Msg
    = GetQuestionnaireComplete (Result ApiError (QuestionnaireDetailWrapper QuestionnaireQuestionnaire))
    | GetQuestionnaireImporterComplete (Result ApiError QuestionnaireImporter)
    | FetchKnowledgeModelStringComplete (Result ApiError String)
    | GotImporterData (Result D.Error (List ImporterEvent))
    | PutImportData
    | PutImporterDataComplete (Result ApiError ())
    | QuestionnaireMsg Questionnaire.Msg
    | ChangeSidePanel SidePanel
