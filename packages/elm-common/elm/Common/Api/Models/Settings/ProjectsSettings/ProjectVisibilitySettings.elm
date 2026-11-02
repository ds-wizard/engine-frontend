module Common.Api.Models.Settings.ProjectsSettings.ProjectVisibilitySettings exposing
    ( ProjectVisibilitySettings
    , decoder
    , encode
    )

import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ProjectVisibilitySettings =
    { enabled : Bool
    , defaultValue : ProjectVisibility
    }


decoder : Decoder ProjectVisibilitySettings
decoder =
    D.succeed ProjectVisibilitySettings
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" ProjectVisibility.decoder


encode : ProjectVisibilitySettings -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", ProjectVisibility.encode config.defaultValue )
        ]
