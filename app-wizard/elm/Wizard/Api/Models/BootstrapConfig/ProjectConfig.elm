module Wizard.Api.Models.BootstrapConfig.ProjectConfig exposing
    ( ProjectConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Api.Models.BootstrapConfig.ProjectConfig.ProjectSharingConfig as ProjectSharingConfig exposing (ProjectSharingConfig)
import Wizard.Api.Models.BootstrapConfig.ProjectConfig.ProjectVisibilityConfig as ProjectVisibilityConfig exposing (ProjectVisibilityConfig)
import Wizard.Api.Models.Project.ProjectCreation as ProjectCreation exposing (ProjectCreation)


type alias ProjectConfig =
    { projectVisibility : ProjectVisibilityConfig
    , projectSharing : ProjectSharingConfig
    , projectCreation : ProjectCreation
    , feedback : SimpleFeatureConfig
    , summaryReport : SimpleFeatureConfig
    , projectTagging : SimpleFeatureConfig
    }


decoder : Decoder ProjectConfig
decoder =
    D.succeed ProjectConfig
        |> D.required "projectVisibility" ProjectVisibilityConfig.decoder
        |> D.required "projectSharing" ProjectSharingConfig.decoder
        |> D.required "projectCreation" ProjectCreation.decoder
        |> D.required "feedback" SimpleFeatureConfig.decoder
        |> D.required "summaryReport" SimpleFeatureConfig.decoder
        |> D.required "projectTagging" SimpleFeatureConfig.decoder


default : ProjectConfig
default =
    { projectVisibility = ProjectVisibilityConfig.default
    , projectSharing = ProjectSharingConfig.default
    , projectCreation = ProjectCreation.TemplateAndCustom
    , feedback = SimpleFeatureConfig.init True
    , summaryReport = SimpleFeatureConfig.init True
    , projectTagging = SimpleFeatureConfig.init True
    }
