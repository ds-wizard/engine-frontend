module Wizard.Questionnaires.Index.Msgs exposing (Msg(..))

import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Common.Components.Listing.Msgs as Listing
import Wizard.Questionnaires.Common.CloneQuestionnaireModal.Msgs as CloneQuestionnaireModal
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs as DeleteQuestionnaireModal


type Msg
    = DeleteQuestionnaireMigration Uuid
    | DeleteQuestionnaireMigrationCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg Questionnaire)
    | DeleteQuestionnaireModalMsg DeleteQuestionnaireModal.Msg
    | CloneQuestionnaireModalMsg CloneQuestionnaireModal.Msg
