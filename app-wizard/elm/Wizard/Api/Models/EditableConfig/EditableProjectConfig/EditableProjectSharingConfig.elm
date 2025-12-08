module Wizard.Api.Models.EditableConfig.EditableProjectConfig.EditableProjectSharingConfig exposing
    ( EditableProjectSharingConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)


type alias EditableProjectSharingConfig =
    { enabled : Bool
    , defaultValue : ProjectSharing
    , anonymousEnabled : Bool
    }


decoder : Decoder EditableProjectSharingConfig
decoder =
    D.succeed EditableProjectSharingConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" ProjectSharing.decoder
        |> D.required "anonymousEnabled" D.bool


encode : EditableProjectSharingConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", ProjectSharing.encode config.defaultValue )
        , ( "anonymousEnabled", E.bool config.anonymousEnabled )
        ]
