module Common.Api.Models.Settings.ProjectsSettings.ProjectSharingSettings exposing
    ( ProjectSharingSettings
    , decoder
    , encode
    )

import Common.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ProjectSharingSettings =
    { enabled : Bool
    , defaultValue : ProjectSharing
    , anonymousEnabled : Bool
    }


decoder : Decoder ProjectSharingSettings
decoder =
    D.succeed ProjectSharingSettings
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" ProjectSharing.decoder
        |> D.required "anonymousEnabled" D.bool


encode : ProjectSharingSettings -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", ProjectSharing.encode config.defaultValue )
        , ( "anonymousEnabled", E.bool config.anonymousEnabled )
        ]
