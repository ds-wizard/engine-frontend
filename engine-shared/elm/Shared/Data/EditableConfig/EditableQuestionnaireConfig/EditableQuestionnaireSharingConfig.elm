module Shared.Data.EditableConfig.EditableQuestionnaireConfig.EditableQuestionnaireSharingConfig exposing
    ( EditableQuestionnaireSharingConfig
    , decoder
    , default
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)


type alias EditableQuestionnaireSharingConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireSharing
    }


decoder : Decoder EditableQuestionnaireSharingConfig
decoder =
    D.succeed EditableQuestionnaireSharingConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireSharing.decoder


encode : EditableQuestionnaireSharingConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", QuestionnaireSharing.encode config.defaultValue )
        ]


default : EditableQuestionnaireSharingConfig
default =
    { enabled = True
    , defaultValue = QuestionnaireSharing.RestrictedQuestionnaire
    }
