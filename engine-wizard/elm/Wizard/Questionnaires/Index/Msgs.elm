module Wizard.Questionnaires.Index.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type Msg
    = GetQuestionnairesCompleted (Result ApiError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
    | DeleteQuestionnaireMigration String
    | DeleteQuestionnaireMigrationCompleted (Result ApiError ())
    | CloneQuestionnaire Questionnaire
    | CloneQuestionnaireCompleted (Result ApiError Questionnaire)
    | ListingMsg Listing.Msg
