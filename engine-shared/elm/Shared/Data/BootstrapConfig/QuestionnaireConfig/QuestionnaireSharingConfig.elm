module Shared.Data.BootstrapConfig.QuestionnaireConfig.QuestionnaireSharingConfig exposing
    ( QuestionnaireSharingConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)


type alias QuestionnaireSharingConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireSharing
    }


decoder : Decoder QuestionnaireSharingConfig
decoder =
    D.succeed QuestionnaireSharingConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireSharing.decoder


default : QuestionnaireSharingConfig
default =
    { enabled = True
    , defaultValue = QuestionnaireSharing.RestrictedQuestionnaire
    }
