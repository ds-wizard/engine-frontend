module Common.Api.Models.BootstrapConfig.ProjectConfig exposing
    ( ProjectConfig
    , decoder
    , default
    )

import Common.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Common.Api.Models.BootstrapConfig.ProjectConfig.ProjectSharingConfig as ProjectSharingConfig exposing (ProjectSharingConfig)
import Common.Api.Models.BootstrapConfig.ProjectConfig.ProjectVisibilityConfig as ProjectVisibilityConfig exposing (ProjectVisibilityConfig)
import Common.Api.Models.Project.ProjectCreation as ProjectCreation exposing (ProjectCreation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias ProjectConfig =
    { projectVisibility : ProjectVisibilityConfig
    , projectSharing : ProjectSharingConfig
    , projectCreation : ProjectCreation
    , summaryReport : SimpleFeatureConfig
    , projectTagging : SimpleFeatureConfig
    }


decoder : Decoder ProjectConfig
decoder =
    D.succeed ProjectConfig
        |> D.required "projectVisibility" ProjectVisibilityConfig.decoder
        |> D.required "projectSharing" ProjectSharingConfig.decoder
        |> D.required "projectCreation" ProjectCreation.decoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder
        |> D.required "projectTagging" SimpleFeatureConfig.decoder


default : ProjectConfig
default =
    { projectVisibility = ProjectVisibilityConfig.default
    , projectSharing = ProjectSharingConfig.default
    , projectCreation = ProjectCreation.TemplateAndCustom
    , summaryReport = SimpleFeatureConfig.init True
    , projectTagging = SimpleFeatureConfig.init True
    }
