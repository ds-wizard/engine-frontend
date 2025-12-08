module Wizard.Api.Models.EditableConfig.EditableProjectConfig exposing
    ( EditableProjectConfig
    , Feedback
    , ProjectTagging
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Api.Models.EditableConfig.EditableProjectConfig.EditableProjectSharingConfig as EditableProjectSharingConfig exposing (EditableProjectSharingConfig)
import Wizard.Api.Models.EditableConfig.EditableProjectConfig.EditableProjectVisibilityConfig as EditableProjectVisibilityConfig exposing (EditableProjectVisibilityConfig)
import Wizard.Api.Models.Project.ProjectCreation as ProjectCreation exposing (ProjectCreation)


type alias EditableProjectConfig =
    { projectVisibility : EditableProjectVisibilityConfig
    , projectSharing : EditableProjectSharingConfig
    , projectCreation : ProjectCreation
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


decoder : Decoder EditableProjectConfig
decoder =
    D.succeed EditableProjectConfig
        |> D.required "projectVisibility" EditableProjectVisibilityConfig.decoder
        |> D.required "projectSharing" EditableProjectSharingConfig.decoder
        |> D.required "projectCreation" ProjectCreation.decoder
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


encode : EditableProjectConfig -> E.Value
encode config =
    E.object
        [ ( "projectVisibility", EditableProjectVisibilityConfig.encode config.projectVisibility )
        , ( "projectSharing", EditableProjectSharingConfig.encode config.projectSharing )
        , ( "projectCreation", ProjectCreation.encode config.projectCreation )
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
