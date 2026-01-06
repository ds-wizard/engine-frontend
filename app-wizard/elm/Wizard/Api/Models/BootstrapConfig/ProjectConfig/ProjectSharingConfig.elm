module Wizard.Api.Models.BootstrapConfig.ProjectConfig.ProjectSharingConfig exposing
    ( ProjectSharingConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)


type alias ProjectSharingConfig =
    { enabled : Bool
    , defaultValue : ProjectSharing
    , anonymousEnabled : Bool
    }


decoder : Decoder ProjectSharingConfig
decoder =
    D.succeed ProjectSharingConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" ProjectSharing.decoder
        |> D.required "anonymousEnabled" D.bool


default : ProjectSharingConfig
default =
    { enabled = True
    , defaultValue = ProjectSharing.Restricted
    , anonymousEnabled = False
    }
