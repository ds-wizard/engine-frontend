module Shared.Data.EditableConfig.EditableQuestionnairesConfig exposing
    ( EditableQuestionnairesConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Shared.Data.EditableConfig.EditableQuestionnaireConfig.EditableQuestionnaireSharingConfig as EditableQuestionnaireSharingConfig exposing (EditableQuestionnaireSharingConfig)
import Shared.Data.EditableConfig.EditableQuestionnaireConfig.EditableQuestionnaireVisibilityConfig as EditableQuestionnaireVisibilityConfig exposing (EditableQuestionnaireVisibilityConfig)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation exposing (QuestionnaireCreation)


type alias EditableQuestionnairesConfig =
    { questionnaireVisibility : EditableQuestionnaireVisibilityConfig
    , questionnaireSharing : EditableQuestionnaireSharingConfig
    , questionnaireCreation : QuestionnaireCreation
    , feedback : Feedback
    , summaryReport : SimpleFeatureConfig
    , projectTagging : ProjectTagging
    }


type alias Feedback =
    { enabled : Bool
    , token : String
    , owner : String
    , repo : String
    }


type alias ProjectTagging =
    { enabled : Bool
    , tags : List String
    }


decoder : Decoder EditableQuestionnairesConfig
decoder =
    D.succeed EditableQuestionnairesConfig
        |> D.required "questionnaireVisibility" EditableQuestionnaireVisibilityConfig.decoder
        |> D.required "questionnaireSharing" EditableQuestionnaireSharingConfig.decoder
        |> D.required "questionnaireCreation" QuestionnaireCreation.decoder
        |> D.required "feedback" feedbackDecoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder
        |> D.required "projectTagging" projectTaggingDecoder


feedbackDecoder : Decoder Feedback
feedbackDecoder =
    D.succeed Feedback
        |> D.required "enabled" D.bool
        |> D.required "token" D.string
        |> D.required "owner" D.string
        |> D.required "repo" D.string


projectTaggingDecoder : Decoder ProjectTagging
projectTaggingDecoder =
    D.succeed ProjectTagging
        |> D.required "enabled" D.bool
        |> D.required "tags" (D.list D.string)


encode : EditableQuestionnairesConfig -> E.Value
encode config =
    E.object
        [ ( "questionnaireVisibility", EditableQuestionnaireVisibilityConfig.encode config.questionnaireVisibility )
        , ( "questionnaireSharing", EditableQuestionnaireSharingConfig.encode config.questionnaireSharing )
        , ( "questionnaireCreation", QuestionnaireCreation.encode config.questionnaireCreation )
        , ( "feedback", encodeFeedback config.feedback )
        , ( "summaryReport", SimpleFeatureConfig.encode config.summaryReport )
        , ( "projectTagging", encodeProjectTagging config.projectTagging )
        ]


encodeFeedback : Feedback -> E.Value
encodeFeedback feedback =
    E.object
        [ ( "enabled", E.bool feedback.enabled )
        , ( "token", E.string feedback.token )
        , ( "owner", E.string feedback.owner )
        , ( "repo", E.string feedback.repo )
        ]


encodeProjectTagging : ProjectTagging -> E.Value
encodeProjectTagging projectTagging =
    E.object
        [ ( "enabled", E.bool projectTagging.enabled )
        , ( "tags", E.list E.string projectTagging.tags )
        ]
