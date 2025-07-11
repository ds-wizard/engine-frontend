module Wizard.Api.Models.EditableConfig.EditableQuestionnaireConfig.EditableQuestionnaireSharingConfig exposing
    ( EditableQuestionnaireSharingConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)


type alias EditableQuestionnaireSharingConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireSharing
    , anonymousEnabled : Bool
    }


decoder : Decoder EditableQuestionnaireSharingConfig
decoder =
    D.succeed EditableQuestionnaireSharingConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireSharing.decoder
        |> D.required "anonymousEnabled" D.bool


encode : EditableQuestionnaireSharingConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", QuestionnaireSharing.encode config.defaultValue )
        , ( "anonymousEnabled", E.bool config.anonymousEnabled )
        ]
