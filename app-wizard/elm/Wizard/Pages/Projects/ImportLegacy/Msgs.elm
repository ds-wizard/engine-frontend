module Wizard.Pages.Projects.ImportLegacy.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Json.Decode as D
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectImporter exposing (ProjectImporter)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer.ImporterEvent exposing (ImporterEvent)
import Wizard.Pages.Projects.ImportLegacy.Models exposing (SidePanel)


type Msg
    = GetQuestionnaireComplete (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | GetQuestionnaireImporterComplete (Result ApiError ProjectImporter)
    | FetchKnowledgeModelStringComplete (Result ApiError String)
    | GotImporterData (Result D.Error (List ImporterEvent))
    | PutImportData
    | PutImporterDataComplete (Result ApiError ())
    | QuestionnaireMsg Questionnaire.Msg
    | ChangeSidePanel SidePanel
