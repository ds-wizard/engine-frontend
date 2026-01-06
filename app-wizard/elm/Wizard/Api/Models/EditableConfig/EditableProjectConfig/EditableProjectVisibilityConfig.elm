module Wizard.Api.Models.EditableConfig.EditableProjectConfig.EditableProjectVisibilityConfig exposing
    ( EditableProjectVisibilityConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)


type alias EditableProjectVisibilityConfig =
    { enabled : Bool
    , defaultValue : ProjectVisibility
    }


decoder : Decoder EditableProjectVisibilityConfig
decoder =
    D.succeed EditableProjectVisibilityConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" ProjectVisibility.decoder


encode : EditableProjectVisibilityConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", ProjectVisibility.encode config.defaultValue )
        ]
