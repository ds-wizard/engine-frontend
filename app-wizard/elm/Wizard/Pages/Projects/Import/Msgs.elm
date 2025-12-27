module Wizard.Pages.Projects.Import.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.Questionnaire.Importer.ImporterEvent exposing (ImporterEvent)
import Wizard.Pages.Projects.Import.Models exposing (SidePanel)


type Msg
    = GetQuestionnaireComplete (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | FetchKnowledgeModelStringComplete (Result ApiError String)
    | GotImporterData (List ImporterEvent)
    | PutImportData
    | PutImporterDataComplete (Result ApiError ())
    | QuestionnaireMsg Questionnaire.Msg
    | ChangeSidePanel SidePanel
