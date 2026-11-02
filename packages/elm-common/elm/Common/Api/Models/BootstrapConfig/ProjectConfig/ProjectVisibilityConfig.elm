module Common.Api.Models.BootstrapConfig.ProjectConfig.ProjectVisibilityConfig exposing
    ( ProjectVisibilityConfig
    , decoder
    , default
    )

import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


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
