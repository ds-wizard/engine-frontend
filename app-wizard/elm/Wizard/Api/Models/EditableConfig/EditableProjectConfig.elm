module Wizard.Api.Models.EditableConfig.EditableProjectConfig exposing
    ( EditableProjectConfig
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
    , summaryReport : SimpleFeatureConfig
    , projectTagging : ProjectTagging
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
        |> D.required "summaryReport" SimpleFeatureConfig.decoder
        |> D.required "projectTagging" projectTaggingDecoder


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
        , ( "summaryReport", SimpleFeatureConfig.encode config.summaryReport )
        , ( "projectTagging", encodeProjectTagging config.projectTagging )
        ]


encodeProjectTagging : ProjectTagging -> E.Value
encodeProjectTagging projectTagging =
    E.object
        [ ( "enabled", E.bool projectTagging.enabled )
        , ( "tags", E.list E.string projectTagging.tags )
        ]
