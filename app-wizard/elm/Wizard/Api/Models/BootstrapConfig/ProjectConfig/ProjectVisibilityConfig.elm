module Wizard.Api.Models.BootstrapConfig.ProjectConfig.ProjectVisibilityConfig exposing
    ( ProjectVisibilityConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)


type alias ProjectVisibilityConfig =
    { enabled : Bool
    , defaultValue : ProjectVisibility
    }


decoder : Decoder ProjectVisibilityConfig
decoder =
    D.succeed ProjectVisibilityConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" ProjectVisibility.decoder


default : ProjectVisibilityConfig
default =
    { enabled = True
    , defaultValue = ProjectVisibility.Private
    }
