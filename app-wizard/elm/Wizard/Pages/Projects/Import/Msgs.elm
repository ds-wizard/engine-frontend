module Wizard.Pages.Projects.Import.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Json.Decode as D
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireImporter exposing (QuestionnaireImporter)
import Wizard.Api.Models.QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer.ImporterEvent exposing (ImporterEvent)
import Wizard.Pages.Projects.Import.Models exposing (SidePanel)


type Msg
    = GetQuestionnaireComplete (Result ApiError (QuestionnaireDetailWrapper QuestionnaireQuestionnaire))
    | GetQuestionnaireImporterComplete (Result ApiError QuestionnaireImporter)
    | FetchKnowledgeModelStringComplete (Result ApiError String)
    | GotImporterData (Result D.Error (List ImporterEvent))
    | PutImportData
    | PutImporterDataComplete (Result ApiError ())
    | QuestionnaireMsg Questionnaire.Msg
    | ChangeSidePanel SidePanel
