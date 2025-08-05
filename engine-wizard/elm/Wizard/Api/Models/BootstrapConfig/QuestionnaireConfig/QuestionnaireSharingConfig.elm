module Wizard.Api.Models.BootstrapConfig.QuestionnaireConfig.QuestionnaireSharingConfig exposing
    ( QuestionnaireSharingConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)


type alias QuestionnaireSharingConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireSharing
    , anonymousEnabled : Bool
    }


decoder : Decoder QuestionnaireSharingConfig
decoder =
    D.succeed QuestionnaireSharingConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireSharing.decoder
        |> D.required "anonymousEnabled" D.bool


default : QuestionnaireSharingConfig
default =
    { enabled = True
    , defaultValue = QuestionnaireSharing.RestrictedQuestionnaire
    , anonymousEnabled = False
    }
