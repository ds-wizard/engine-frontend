module Wizard.Settings.Common.EditableQuestionnairesConfig exposing
    ( EditableQuestionnairesConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Common.Config.Partials.QuestionnaireVisibilityConfig as QuestionnaireVisibilityConfig exposing (QuestionnaireVisibilityConfig)
import Wizard.Common.Config.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias EditableQuestionnairesConfig =
    { questionnaireVisibility : QuestionnaireVisibilityConfig
    , levels : SimpleFeatureConfig
    , feedback : Feedback
    , summaryReport : SimpleFeatureConfig
    }


type alias Feedback =
    { enabled : Bool
    , token : String
    , owner : String
    , repo : String
    }


decoder : Decoder EditableQuestionnairesConfig
decoder =
    D.succeed EditableQuestionnairesConfig
        |> D.required "questionnaireVisibility" QuestionnaireVisibilityConfig.decoder
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "feedback" feedbackDecoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder


feedbackDecoder : Decoder Feedback
feedbackDecoder =
    D.succeed Feedback
        |> D.required "enabled" D.bool
        |> D.required "token" D.string
        |> D.required "owner" D.string
        |> D.required "repo" D.string


encode : EditableQuestionnairesConfig -> E.Value
encode config =
    E.object
        [ ( "questionnaireVisibility", QuestionnaireVisibilityConfig.encode config.questionnaireVisibility )
        , ( "levels", SimpleFeatureConfig.encode config.levels )
        , ( "feedback", encodeFeedback config.feedback )
        , ( "summaryReport", SimpleFeatureConfig.encode config.summaryReport )
        ]


encodeFeedback : Feedback -> E.Value
encodeFeedback feedback =
    E.object
        [ ( "enabled", E.bool feedback.enabled )
        , ( "token", E.string feedback.token )
        , ( "owner", E.string feedback.owner )
        , ( "repo", E.string feedback.repo )
        ]
